#functions to establish simple linear shifts from one place to another

#but this is kinda stupid, I know more about the locations... maybe I should
#estimate a different transfer function...

#wrapper function: takes a list with the summarized? phenological records?
#also need to indicate in which column I am interested in

est_phen_gaps <- function(target_df, target_col, split_col){
  
  # target_df <- flower_summarized %>% 
  #   mutate(location = factor(location, levels = c('Cieza',
  #                                                 'Klein-Altendorf',
  #                                                 'Meknes',
  #                                                 'Sfax',
  #                                                 'Zaragoza')))
  #split_col <- 'flowering_type'
  #target_col <- 'mean'^
  
  target_df$value <- target_df[, target_col]
  
  #split into list of data.frames
  target_list <- target_df %>% 
    mutate(source = 'observed') %>% 
    split(f = target_df[,split_col])
  
  #get first degree relationships
  first_deg_rel_df <- purrr::map(target_list, function(x) est_1st_relationships(in_df = x, target_col = target_col)) %>% 
    bind_rows(.id = 'id_col')
  
  ##find out which combinations of species and location are missing
  #get missing combinations for each flowering type
  
  species <- levels(as.factor(target_df$species))
  locations <- levels(as.factor(target_df$location))
  
  miss_list <- purrr::map(target_list, function(x){
    comb_all <- expand.grid(species, locations)
    
    miss_df <- comb_all[!(paste0(comb_all$Var1, comb_all$Var2) %in%
                            paste0(x$species, x$location)),] %>% 
      set_colnames(c('species', 'location'))
    
    return(miss_df)
  })
  
  #estimate values based on first degree relationships
  filled_list <- purrr::map2(miss_list, target_list, function(miss, tar) est_shift(miss_df = miss, 
                                                                    obs_df = tar, 
                                                                    relationship_df = first_deg_rel_df))

  #weed out entries from the miss list which are now filled

  miss_list <- purrr::map2(miss_list, filled_list, function(miss, fill){
    
    miss[(!paste0(miss$species, miss$location) %in% paste0(fill$species, fill$location)),]
  })
  
  #establish second degree relationships: between obs and first_deg
  #make combinations of observed and deg1
  second_deg_rel_df <- purrr::map2(target_list, filled_list, function(obs, fill){
    
    #going all combinations of observed and filled
    row_i <- expand.grid(1:nrow(obs), 1:nrow(fill))
    
    #only keep pairs of the same species 
    row_i <- row_i[obs[row_i$Var1,'species'] == fill[row_i$Var2,'species'],]
    
    #remove pairs for which the source is equal to one donor
    row_i <- row_i[obs[row_i$Var1, 'location'] != fill[row_i$Var2,'donor'],]
    
    #remove selve comparisons
    row_i <- row_i[as.character(obs[row_i$Var1,'location']) != as.character(fill[row_i$Var2,'location']),]
    
    
    #calculate second degree relationship (in both directions)
    return(rbind(data.frame(
      'from' = obs[row_i$Var1, 'location'],
      'to' = fill[row_i$Var2, 'location'],
      'species' = fill[row_i$Var2, 'species'],
      'shift' =   fill[row_i$Var2, 'value'] - obs[row_i$Var1, target_col]
    ),
    data.frame(
      'from' = fill[row_i$Var2, 'location'],
      'to' = obs[row_i$Var1, 'location'],
      'species' = fill[row_i$Var2, 'species'],
      'shift' =   obs[row_i$Var1, target_col] - fill[row_i$Var2, 'value']
    )
    ))
  }) %>% 
    bind_rows()
  
  #make sure to remove second degree relationships for which we have already first degree relationships
  #should be impossible, but anyway...
  second_deg_rel_df <- second_deg_rel_df[!(paste0(second_deg_rel_df$from, second_deg_rel_df$to, second_deg_rel_df$species) %in%
    paste0(first_deg_rel_df$from, first_deg_rel_df$to, first_deg_rel_df$species)),]
  
  #there are duplicated values in the second degree relationship
  second_deg_rel_df <- second_deg_rel_df %>% 
    mutate(shift = round(shift, digits = 4)) %>% 
    group_by(from, to) %>% 
    summarize(shift = unique(shift))
  
  #add filled values to the target_df
  filled_list <- purrr::map2(target_list, filled_list, function(obs, fill){
    
    obs %>% 
      dplyr::select(species, location, source, value) %>% 
      mutate(donor = 'observed') %>% 
      rbind.data.frame(fill)
    
  })
  
  
  filled_list2 <- purrr::map2(miss_list, filled_list, function(miss, tar) est_shift(miss_df = miss, 
                                                                                   obs_df = tar, 
                                                                                   relationship_df = second_deg_rel_df,
                                                                                   label = 'deg_2'))
  
  #weed out entries from the miss list which are now filled
  miss_list <- purrr::map2(miss_list, filled_list2, function(miss, fill){
    
    miss[(!paste0(miss$species, miss$location) %in% paste0(fill$species, fill$location)),]
  })
  
  #add second degree relationship to fill list
  filled_list <- purrr::map2(filled_list, filled_list2, rbind)

  
  #thrid degree relationships (between deg_1 and deg_1)
  #establish second degree relationships: between obs and first_deg
  #make combinations of observed and deg1
  third_deg_rel_df <- purrr::map2(target_list, filled_list, function(obs, fill){
    
    fill <- fill %>% 
      filter(source == 'deg_1')
    
    #going all combinations of observed and filled
    row_i <- expand.grid(1:nrow(fill), 1:nrow(fill))
    
    #only keep pairs of the same species 
    row_i <- row_i[fill[row_i$Var1,'species'] == fill[row_i$Var2,'species'],]
    
    #remove pairs for which the source is equal to one donor
    row_i <- row_i[fill[row_i$Var1, 'location'] != fill[row_i$Var2,'donor'],]
    row_i <- row_i[fill[row_i$Var1, 'donor'] != fill[row_i$Var2,'location'],]
    
    #remove selve comparisons
    row_i <- row_i[as.character(fill[row_i$Var1,'location']) != as.character(fill[row_i$Var2,'location']),]
    
    
    #calculate second degree relationship (in both directions)
    return(data.frame(
      'from' = fill[row_i$Var1, 'location'],
      'to' = fill[row_i$Var2, 'location'],
      'species' = fill[row_i$Var2, 'species'],
      'shift' =   fill[row_i$Var2, 'value'] - fill[row_i$Var1, 'value']
      ))
    }) %>% 
    bind_rows()
  

  #remove second degree relationships, for which a first degree was already established
  third_deg_rel_df <- third_deg_rel_df[!(paste0(third_deg_rel_df$from, third_deg_rel_df$to) %in%
                                                             c(paste0(first_deg_rel_df$from, first_deg_rel_df$to),
                                                               paste0(second_deg_rel_df$from, second_deg_rel_df$to))),]
  

  filled_list3 <- purrr::map2(miss_list, filled_list, function(miss, tar) est_shift_d3(miss_df = miss, 
                                                                                    obs_df = tar, 
                                                                                    relationship_df = third_deg_rel_df))
  

  #weed out entries from the miss list which are now filled
  miss_list <- purrr::map2(miss_list, filled_list3, function(miss, fill){
    
    miss <- miss[(!paste0(miss$species, miss$location) %in% paste0(fill$species, fill$location)),]
    
    if(nrow(miss) != 0){
      
      cat("Could not fill values for species: ", unique(as.character(miss$species)), "\n",
          'in the locations: ', unique(as.character(miss$location)), "\n")
      
      return(miss)
    } else {
      return(NULL)
    }
  })
  
  #add second degree relationship to fill list
  filled_list <- purrr::map2(filled_list, filled_list3, rbind)
  
  #add missing values to filled list
  fill_list <- purrr::map2(filled_list, miss_list, function(fill, miss){
    
    if(is.null(miss)){
      return(fill)
    } else {

      fill <- miss %>% 
        mutate(source = NA, 
               value = NA, 
               donor = NA) %>% 
        rbind(fill)
      
      return(fill)
    }
  }) %>% 
    bind_rows(.id = 'id')
  
  fill_list[,split_col] <- fill_list$id
  
  return(fill_list %>% 
    dplyr::select(-id )
  )
  
}


#function to establish the relationships

est_1st_relationships <- function(in_df, target_col){
   #in_df <- target_list[[1]]
  
  ind_row <- expand.grid(1:nrow(in_df), 1:nrow(in_df))
  
  #remove self comparison
  ind_row <- ind_row %>% 
    filter(Var1 != Var2)
  
  #remove comparisons of the same location
  ind_row <- ind_row[in_df[ind_row$Var1, 'location'] != in_df[ind_row$Var2, 'location'],]
  
  #remove comparisons across locations
  ind_row <- ind_row[in_df[ind_row$Var1, 'species'] == in_df[ind_row$Var2, 'species'],]
  
  
  #create data.frame of relationships

  relationship_df <- data.frame(
    'from' = in_df[ind_row$Var1, 'location'],
    'to' = in_df[ind_row$Var2, 'location'],
    'species' = in_df[ind_row$Var2, 'species'],
    'shift' =   in_df[ind_row$Var2, target_col] - in_df[ind_row$Var1, target_col]
  ) %>% 
    na.omit()
  
  return(relationship_df)
}

est_shift_d3 <- function(miss_df, obs_df, relationship_df) {
  
  # miss_df <- miss_list[[1]]
  # obs_df <- filled_list[[1]]
  # relationship_df <- second_deg_rel_df
  
  #for each missing species in the location, search for a station with which the 
  #missing location has a relationship with and a fitting observation
  filled_df <-  purrr::map2(miss_df$species, miss_df$location, function(spec, loc){
    
    #filter the observed data for needed species
    rel_sub <- relationship_df %>% 
      filter(to == loc) 
    
    #if there is no observation for that species, it can't be fixed and go to next
    if(nrow(rel_sub) == 0){
      return(NULL)
    }
    
    #check if there is an observation for the species coming from the location for which we have relationships
    obs_sub <- obs_df %>% 
      filter(species == spec, location %in% rel_sub$from)
    
    if(nrow(obs_sub) == 0){
      return(NULL)
    } else {
      
      row_i <- expand.grid(1:nrow(obs_sub), 1:nrow(rel_sub)) #for each of the row combinations, calculate shift 
      
      return(data.frame(species = spec,
                        location = loc,
                        value = obs_sub$value[row_i$Var1] + rel_sub$shift[row_i$Var2],
                        source = 'deg_3',
                        donor = obs_sub$location[row_i$Var1]))
      
      
    }
  }) %>% 
    bind_rows()
  
  
  return(filled_df)
}



est_shift <- function(miss_df, obs_df, relationship_df, label = 'deg_1') {
  
  # miss_df <- miss_list[[1]]
  # obs_df <- filled_list[[1]]
  # relationship_df <- second_deg_rel_df
  
  #for each missing species in the location, search for a station with which the 
  #missing location has a relationship with and a fitting observation
  filled_df <-  purrr::map2(miss_df$species, miss_df$location, function(spec, loc){
    
    #filter the observed data for needed species
    obs_sub <- obs_df %>% 
      filter(source == 'observed',
             species == spec) 
    
    #if there is no observation for that species, it can't be fixed and go to next
    if(nrow(obs_sub) == 0){
      return(NULL)
    }
    
    #subset possible observations that could help fill the missing relationship
    rel_sub <- relationship_df %>% 
      filter(to == as.character(loc),
             from %in%  as.character(obs_sub$location))
    
    if(nrow(rel_sub) == 0){
      return(NULL)
    }
    
    #subset the observations again
    obs_sub <- obs_sub %>% 
      filter(location %in% rel_sub$from)
    
    
    #if there is no combination of relationship and local observation available, then go to next
    if(nrow(obs_sub) == 0){
      return(NULL)
    } else {
      
      row_i <- expand.grid(1:nrow(obs_sub), 1:nrow(rel_sub)) #for each of the row combinations, calculate shift 
      
      return(data.frame(species = spec,
                        location = loc,
                        value = obs_sub$value[row_i$Var1] + rel_sub$shift[row_i$Var2],
                        source = label,
                        donor = obs_sub$location[row_i$Var1]))
    }
  }) %>% 
    bind_rows() 
  
  
  return(filled_df)
}

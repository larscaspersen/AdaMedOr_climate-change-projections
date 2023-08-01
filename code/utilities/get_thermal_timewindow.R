
#input:
# weather_list_obs:   list of observed weather which corresponds to the phenology data.frame (observation_df)
#                     one element for one weather station. each element is data.frame of minimum and maximum temperature for the whole observation period
#                     needs a column called Date
# weather_list_pred:  list of weather, for which the time windows should be applied to (the ones which are also used to make phenological predictions)
#                     can be the same as weather_list_obs
#                     also contains daily Tmin and Tmax
#                     needs a colum called date
# observation_df:     data.frame which contains the phenological records
#                     phenological records should be in date.format
#                     needs column called species and location (location names need to be identical with element names of weather_list_obs and weather_list_pred)
# frost_threshold:    numeric, temperature in degree C for which the frost risk should be calculated
# heat_threshold:     numeric, temperature in degree C for whioch the heat risk should be calculated. 
#                     when defining the temperature window, it is always assumed that the heat risk is "left" to the maximum heat risk for the location (which works only for northern latitudes)
# target_col_obs:     character, name of the column in which the phenological records are stored in the observation_df
# run_mean_window:    numeric, by default 10. when calculating frost and heat risk, the individual risks per day of the year are smoothed by calculating the running mean
#                     this argument defines the window width of the running mean
#                     
# output:             data.frame containing the upper and lower boundary of thermal time window (in day of the year)
#                     has columns for species, location, flowering_type (same as input argument target_col_obs) and upper and lower for the day of the year thresholds
#

get_thermal_window_phenology <- function(weather_list_obs,
                                         weather_list_pred,
                                         observation_df, 
                                         frost_threshold, 
                                         heat_threshold,
                                         target_col_obs,
                                         run_mean_window = 10,
                                         padding = 0.02){
  
  
  #species to do the analysis for
  species <- unique(observation_df$species)
  
  
  #check if DATE is present, if so make it to Date
  weather_list_obs <- purrr::map(weather_list_obs, function(x){
    if('date' %in% tolower(colnames(x)) & ('Date' %in% colnames(x)) == FALSE){
      
      #find out which version is present
      i <- which(tolower(colnames(x)) == 'date')[1]
      
      
      x$Date <- x[,i]
      
      return(x)
    } else if('date' %in% tolower(colnames(x)) == FALSE){
      stop('weather_list_obs does not contain a column which is called "Date", "DATE" or written in a different case. All weather data need a date column for the function to work')
    } else { 
      return(x)}
  })
  
  #check if DATE is present, if so make it to Date
  weather_list_pred <- purrr::map(weather_list_pred, function(x){
    if('date' %in% tolower(colnames(x)) & ('Date' %in% colnames(x)) == FALSE){
      
      #find out which version is present
      i <- which(tolower(colnames(x)) == 'date')[1]
      
      
      x$Date <- x[,i]
      
      return(x)
    } else if('date' %in% tolower(colnames(x)) == FALSE){
      stop('weather_list_obs does not contain a column which is called "Date", "DATE" or written in a different case. All weather data need a date column for the function to work')
    } else { 
      return(x)}
  })
  
  
  #-------------------------------------------------------------------------#
  # establish risks of the phenology data and the corrosponding weather data
  #-------------------------------------------------------------------------#
  
  
  
  #get frost and heat risk for each day of the year
  frost_risk <- purrr::map(weather_list_obs, function(x){
    x %>% 
      mutate(doy = lubridate::yday(Date)) %>% 
      group_by(doy) %>% 
      summarise(chance_frost = sum(Tmin <= frost_threshold) / n()) %>% 
      mutate(run_mean_frost = chillR::runn_mean(chance_frost, runn_mean = run_mean_window))
  }) %>% 
    bind_rows(.id = 'location') %>% 
    tidyr::expand_grid(species = species)
  
  
  #heat risk for each day of the year
  heat_risk <- purrr::map(weather_list_obs, function(x){
    x %>% 
      mutate(doy = lubridate::yday(Date)) %>% 
      group_by(doy) %>% 
      summarise(chance_heat = sum(Tmax >= heat_threshold) / n()) %>% 
      mutate(run_mean_heat = chillR::runn_mean(chance_heat, runn_mean = run_mean_window))
  }) %>% 
    bind_rows(.id = 'location') %>% 
    tidyr::expand_grid(species = species)
  
  
  #save the target column in a different column, which I can access by name
  observation_df$target_pheno <- observation_df[, target_col_obs]
  
  
  #summarize the phenology data, know for each day of the year how many flowering records were measured for each species and location
  flower_sum <- observation_df %>% 
    mutate(doy_pheno = lubridate::yday(target_pheno)) %>% 
    group_by(species, location, doy_pheno) %>% 
    summarise(n_flower = n())
  
  
  
  max_frost <- observation_df %>%
    mutate(doy_pheno = lubridate::yday(target_pheno)) %>%
    dplyr::select(species, location, cultivar, flowering_f50, doy_pheno) %>%
    merge.data.frame(frost_risk, by.x = c('species', 'location', 'doy_pheno'), by.y = c('species', 'location', 'doy'), all = TRUE) %>%
    na.omit() %>%
    group_by(species) %>%
    summarise(max_risk_frost = max(run_mean_frost))

  max_heat <- observation_df %>%
    mutate(doy_pheno = lubridate::yday(target_pheno)) %>%
    dplyr::select(species, location, cultivar, flowering_f50, doy_pheno) %>%
    merge.data.frame(heat_risk, by.x = c('species', 'location', 'doy_pheno'), by.y = c('species', 'location', 'doy'), all = TRUE) %>%
    na.omit() %>%
    group_by(species) %>%
    summarise(max_risk_heat = max(run_mean_heat))
  
  observation_df$target_doy <- lubridate::yday(observation_df$target_pheno)




  #get maximum frost risk accepted per species
  #
  #assign the calculated frost risk for each day for which we had flowering data
  #group by species (so have different locations in the same group) and calculate the maximum frost risk associated with flowering for the species in the dataset
  #say that this is the maximum frost tolerated
  #
  #problem: this leads to very narrow windows, because lower end is very strict if you have only data from tunisia or morocco
  #         how can I reduce the "exclusivity" of the lower boundary? round values? add an extra tolerance of 5%?
  #
  
  frost_limit <- frost_risk %>% 
    group_by(species) %>% 
    merge.data.frame(flower_sum, by.y = c('location', 'doy_pheno', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
    na.omit() %>% 
    group_by(species) %>% 
    summarise(max_frost_risk = max(run_mean_frost)) %>% 
    mutate(flowering_type = target_col_obs,
           max_frost_risk_padded = max_frost_risk + padding)
  
  
  heat_limit <- heat_risk %>% 
    group_by(species) %>% 
    merge.data.frame(flower_sum, by.y = c('location', 'doy_pheno', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
    na.omit() %>% 
    group_by(species) %>% 
    summarise(max_heat_risk = max(run_mean_heat)) %>% 
    mutate(flowering_type = target_col_obs,
           max_heat_risk_padded = max_heat_risk + padding)
  
  
  #combine upper and lower risk limit
  #data.frame for each species saying what is the maximum frost and heat risk tolerated
  limits_df <-  merge.data.frame(frost_limit, heat_limit, by = c('species', 'flowering_type'))
  
  
  
  
  #---------------------------------------------------------------------------#
  #calculate window for the weahter data set used for phenological predictions
  #---------------------------------------------------------------------------#
  
  
  #calculate frost and heat risk for the other weather data
  
  #get frost and heat risk for each day of the year
  frost_risk <- purrr::map(weather_list_pred, function(x){
    x %>% 
      mutate(doy = lubridate::yday(Date)) %>% 
      group_by(doy) %>% 
      summarise(chance_frost = sum(Tmin <= frost_threshold) / n()) %>% 
      mutate(run_mean_frost = chillR::runn_mean(chance_frost, runn_mean = run_mean_window))
  }) %>% 
    bind_rows(.id = 'location') %>% 
    tidyr::expand_grid(species = species)
  
  #heat risk for each day of the year
  heat_risk <- purrr::map(weather_list_pred, function(x){
    x %>% 
      mutate(doy = lubridate::yday(Date)) %>% 
      group_by(doy) %>% 
      summarise(chance_heat = sum(Tmax >= heat_threshold) / n()) %>% 
      mutate(run_mean_heat = chillR::runn_mean(chance_heat, runn_mean = run_mean_window))
  }) %>% 
    bind_rows(.id = 'location') %>% 
    tidyr::expand_grid(species = species)
  
  
  
  #translate the risk into boundaries in terms of doy
  lower <- frost_risk %>% 
    merge.data.frame(limits_df, by = c('species')) %>% 
    filter(run_mean_frost <=  max_frost_risk) %>% 
    group_by(location, species, flowering_type) %>% 
    summarise(min_doy = min(doy))
  
  #padded
  lower_padded <- frost_risk %>% 
    merge.data.frame(limits_df, by = c('species')) %>% 
    filter(run_mean_frost <=  max_frost_risk_padded) %>% 
    group_by(location, species, flowering_type) %>% 
    summarise(min_doy_padded = min(doy))
  
  
  
  
  #only take values left of the maximum heat risk
  max_heat_risk <- heat_risk %>% 
    group_by(location, .drop = FALSE) %>% 
    summarise(max_risk = max(run_mean_heat))
  
  doy_max_risk_df <- merge.data.frame(heat_risk, max_heat_risk, by = 'location') %>% 
    group_by(location) %>% 
    filter(run_mean_heat == max_risk) %>% 
    summarise(doy_max_risk = min(doy))
  
  upper <- heat_risk %>% 
    merge.data.frame(doy_max_risk_df, by = 'location') %>% 
    filter(doy <= doy_max_risk) %>% 
    merge.data.frame(limits_df, by = c('species')) %>% 
    filter(run_mean_heat <= max_heat_risk) %>% 
    group_by(location, species, flowering_type) %>% 
    summarise(max_doy = max(doy))
  
  upper_padded <- heat_risk %>% 
    merge.data.frame(doy_max_risk_df, by = 'location') %>% 
    filter(doy <= doy_max_risk) %>% 
    merge.data.frame(limits_df, by = c('species')) %>% 
    filter(run_mean_heat <= max_heat_risk_padded) %>% 
    group_by(location, species, flowering_type) %>% 
    summarise(max_doy_padded = max(doy))
  
  
  upper <- merge.data.frame(upper, upper_padded, by = c('location', 'species', 'flowering_type'))
  lower <- merge.data.frame(lower, lower_padded, by = c('location', 'species', 'flowering_type'))
  
  
  thermal_time_window_final <- merge.data.frame(lower, upper, by = c('location', 'species', 'flowering_type'))
  

  return(thermal_time_window_final)
  
}
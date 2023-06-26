extract_cmip6_no_internet <- function(coordinates,
                                      metric = c('tasmin', 'tasmax'),
                                      path_download = 'temp_cmip6',
                                      keep_downloaded = TRUE){
  #get names of downloaded files
  fnames <- list.files(paste0(path_download,'/'))
  fnames <- paste0(path_download,'/', fnames)
  
  #only work with files which are not empty
  fnames <- fnames[file.size(fnames) > 0L]
  
  extracted_df <- purrr::map(fnames, function(x){
    
    #print(x)
    extract_df <- LarsChill:::extract_cmip_data(fname = x, coords = coordinates)
    
    fragment_names <- stringr::str_split(stringr::str_split(x, '/')[[1]][2],'_')[[1]]
    
    extract_df$variable <- fragment_names[1]
    extract_df$model <- fragment_names[3]
    extract_df$ssp <- fragment_names[4]
    
    return(extract_df)
    
  } )
  
  
  extracted_df <- do.call(rbind, extracted_df)
  
  #omit rows with NA
  extracted_df <- stats::na.omit(extracted_df)
  
  #library(tidyverse)
  
  if('pr' %in% metric){
    pr_adj <- extracted_df %>% 
      reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
      filter(.data$variable == 'pr') %>% 
      mutate(value = round(.data$value * 60 * 60 * 24, digits = 2))
  } else {
    pr_adj <- NULL
  }
  
  if('tasmin' %in% metric){
    tmin_adj <- extracted_df %>% 
      reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
      filter(.data$variable == 'tasmin') %>% 
      mutate(value = round(.data$value - 273.15, digits = 2))
  } else {
    tmin_adj <- NULL
  }
  
  if('tasmax' %in% metric){
    tmax_adj <- extracted_df %>% 
      reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
      filter(.data$variable == 'tasmax') %>% 
      mutate(value = round(.data$value - 273.15, digits = 2))
  } else {
    tmax_adj <- NULL
  }
  
  #in case the metric contains variable which are not tasmin, tasmax, pr
  if(any(!metric %in% c('tasmin', 'tasmax', 'pr'))){
    other_adj <- extracted_df %>% 
      reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
      filter(!.data$variable %in% c('tasmax', 'tasmin', 'pr'))
  } else {
    other_adj <- NULL
  }
  
  #bind everything back together, bring back to long format
  extracted_df <- rbind(tmin_adj, tmax_adj, pr_adj, other_adj) %>% 
    reshape2::dcast(formula = Date + variable + model + ssp ~ id)
  
  
  if(keep_downloaded == FALSE){
    unlink(paste0(path_download,'/'), recursive = TRUE)
  }
  
  return(extracted_df)
}
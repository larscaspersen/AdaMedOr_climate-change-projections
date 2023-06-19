get_daily_bias <- function(weather, variable_target, variable_aux, window_width = 31, doy){
  
  half_width <- floor(window_width / 2)
  
  #get doys of target days
  lim_doy <- doy + c(-half_width,half_width)
  
  #adjust for lower lims in old year
  lim_doy <- ifelse(lim_doy <= 0, yes = lim_doy + 365,no = lim_doy)
  
  #adjsut for upper lims in new year
  lim_doy <- ifelse(lim_doy >= 365, yes = lim_doy - 365, no = lim_doy)
  
  
  #if the range of days falls inbetween two years, than different subsetting needed
  if(lim_doy[1] > lim_doy[2]){
    
    target_days <- weather$JDay >= lim_doy[1] | weather$JDay <= lim_doy[2]
    
  } else {
    
    target_days <- weather$JDay >= lim_doy[1] & weather$JDay <= lim_doy[2]
  }
  
  
  
  return(data.frame(doy = doy,
                    mean = mean(weather[[variable_target]][target_days] - weather[[variable_aux]][target_days], na.rm =T), 
                    sd = stats::sd(weather[[variable_target]][target_days] - weather[[variable_aux]][target_days], na.rm =T)))
  
}

get_daily_bias_df <- function(weather, target_vars = c('Tmin', 'Tmax'), aux_vars = c('Tmin.aux', 'Tmax.aux'), window_width = 31){
  
  bias_Tmin <- purrr::map(1:365, function(x) get_daily_bias(weather, target_vars[1], aux_vars[1], doy = x, window_width = window_width)) %>% 
    bind_rows()
  
  bias_Tmax <- purrr::map(1:365, function(x) get_daily_bias(weather, target_vars[2], aux_vars[2], doy = x, window_width = window_width)) %>% 
    bind_rows()
  
  colnames(bias_Tmin) <- c('JDay', paste0(target_vars[1], '_mean_bias'), paste0(target_vars[1], '_sd_bias'))
  colnames(bias_Tmax) <- c('JDay', paste0(target_vars[2], '_mean_bias'), paste0(target_vars[2], '_sd_bias'))
  
  bias_df <- merge.data.frame(bias_Tmin, bias_Tmax, by = 'JDay')
  
  return(bias_df)
  
}











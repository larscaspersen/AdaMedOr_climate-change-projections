get_thermal_window_phenology <- function(weather_list_obs,
                                         weather_list_pred,
                                         observation_df, 
                                         frost_threshold, 
                                         heat_threshold,
                                         target_col_obs,
                                         run_mean_window = 10){
  
  #species to do the analysis for
  species <- unique(observation_df$species)
  
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
  
  
  
  flower_sum <- observation_df %>% 
    mutate(doy_pheno = lubridate::yday(target_pheno)) %>% 
    group_by(species, location, doy_pheno) %>% 
    summarise(n_flower = n())
  
  
  #get maximum frost risk accepted per species
  frost_limit <- frost_risk %>% 
    group_by(species) %>% 
    merge.data.frame(flower_sum, by.y = c('location', 'doy_pheno', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
    na.omit() %>% 
    group_by(species) %>% 
    summarise(max_frost_risk = max(run_mean_frost)) %>% 
    mutate(flowering_type = target_col_obs)
  
  
  heat_limit <- heat_risk %>% 
    group_by(species) %>% 
    merge.data.frame(flower_sum, by.y = c('location', 'doy_pheno', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
    na.omit() %>% 
    group_by(species) %>% 
    summarise(max_heat_risk = max(run_mean_heat)) %>% 
    mutate(flowering_type = target_col_obs)
  
  
  #combine upper and lower risk limit
  limits_df <-  merge.data.frame(frost_limit, heat_limit, by = c('species', 'flowering_type'))
  
  
  
  #translate the risk into boundaries in terms of doy
  lower <- frost_risk %>% 
    merge.data.frame(limits_df, by = c('species')) %>% 
    filter(run_mean_frost <=  max_frost_risk) %>% 
    group_by(location, species, flowering_type) %>% 
    summarise(min_doy = min(doy))
  
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
  
  thermal_time_window_final <- merge.data.frame(lower, upper, by = c('location', 'species', 'flowering_type'))
  
  return(thermal_time_window_final)
  
}
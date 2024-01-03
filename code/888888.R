#nadjust time window method by adding california data

library(tidyverse)
library(ggplot2)
#library(readr)
library(chillR)
#library(devtools)
# install_github("larscaspersen/addition_chillR")
library(LarsChill)

#value for NA in predicted phenology
val_na <- 9999

#---------------------------------------------#
#decide on padding based on the error type 1 and type 2
#---------------------------------------------#

#make predictions for the actual weather data
cka <- read.csv('data/weather_ready/cka_clean.csv') %>% 
  filter(Year < 2022)
cieza <- read.csv('data/weather_ready/cieza_clean_patched.csv')
sfax <- read.csv('data/weather_ready/sfax_clean.csv')
meknes <- read.csv('data/weather_ready/meknes_clean.csv')
zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv') %>% 
  filter(Year < 2022)
santomera <- read.csv('data/weather_ready/murcia_clean.csv')
durham <- read.csv('data/california_almond//combined_3_cimis_12_1948-2022.csv') %>% 
  mutate(Date = lubridate::mdy(DATE))
modesto <- read.csv('data/california_almond//target_2_724926_23258.csv') %>% 
  mutate(Date = lubridate::ymd_hms(DATE))

station_list_ca <- read.csv('data/california_almond//station_list_cimis.csv')
bloom_california <- read.csv('data/california_almond/bloom_combined.csv') %>% 
  mutate(species = 'Almond',
         begin_flowering_f5 = paste(Year, Month, Day, sep = '-'),
         begin_flowering_f5 = lubridate::ymd(begin_flowering_f5),
         cultivar = Variety,
         location = Location, 
         year = Year) %>% 
  dplyr::select(species, cultivar, location, year, begin_flowering_f5) %>% 
  na.omit() %>% 
  mutate(begin_flowering_f5 = as.character(begin_flowering_f5))

#add chico and modesto climate data



weatherlist_ca <- list('Chico' = durham,
                       'Modesto' = modesto)

weather_list_obs <- list('Klein-Altendorf' = cka,
                         'Cieza' = cieza,
                         'Zaragoza' = zaragoza,
                         'Sfax' = sfax,
                         'Meknes' = meknes,
                         'Santomera' = santomera)
weather_list_pred <- weather_list_obs

adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv') %>% 
  filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),
         !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014)) %>% 
  mutate(species =recode(species, `Japanese plum` = 'European plum'))


frost_threshold <- 0
heat_threshold <- 30
observation_df <- adamedor %>% 
  filter(!(species %in% c( 'Peach', 'Olive'))) %>% 
  select(species, cultivar, location, year, begin_flowering_f5) %>% 
  mutate(begin_flowering_f5 = as.character(begin_flowering_f5)) %>% 
  rbind(bloom_california) %>% 
  mutate(begin_flowering_f5= lubridate::ymd(begin_flowering_f5))







thermal_time_window_withca <- LarsChill::get_thermal_window_phenology(weather_list_obs = c(weather_list_obs, weatherlist_ca), 
                                                                      weather_list_pred = c(weather_list_obs, weatherlist_ca), 
                                                                      observation_df = observation_df, 
                                                                      frost_threshold = frost_threshold, 
                                                                      heat_threshold = heat_threshold, 
                                                                      target_col_obs = 'begin_flowering_f5',
                                                                      padding = 0.05)

thermal_time_window <- LarsChill::get_thermal_window_phenology(weather_list_obs = c(weather_list_obs), 
                                                                      weather_list_pred = c(weather_list_obs), 
                                                                      observation_df = observation_df, 
                                                                      frost_threshold = frost_threshold, 
                                                                      heat_threshold = heat_threshold, 
                                                                      target_col_obs = 'begin_flowering_f5',
                                                                      padding = 0.05)


thermal_timewindow_fixed_risk <- function(weather_list_pred,
                                          observation_df,
                                          frost_risk = 0.2,
                                          frost_threshold = 0,
                                          heat_risk = 0.05,
                                          heat_threshold = 30,
                                          target_col_obs = 'begin_flowering_f5',
                                          padding = 0.05){
  

  limits_df <- data.frame(species = unique(observation_df$species)) %>%  
    mutate(flowering_type = target_col_obs,
           max_frost_risk = frost_risk,
           max_frost_risk_padded = frost_risk + padding,
           max_heat_risk = heat_risk,
           max_heat_risk_padded = heat_risk + padding)
  
  
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


thermal_time_window <- LarsChill::get_thermal_window_phenology(weather_list_obs = c(weather_list_obs), 
                                                               weather_list_pred = c(weather_list_obs), 
                                                               observation_df = observation_df, 
                                                               frost_threshold = frost_threshold, 
                                                               heat_threshold = heat_threshold, 
                                                               target_col_obs = 'begin_flowering_f5',
                                                               padding = 0.05)


test <- thermal_timewindow_fixed_risk(weather_list_pred = weather_list_obs, 
                              observation_df = observation_df,
                              frost_risk = 0.2, 
                              frost_threshold = 0,
                              heat_risk = 0.05, 
                              heat_threshold = 30, 
                              target_col_obs = 'begin_flowering_f5')


weather_list_obs = c(weather_list_obs, weatherlist_ca)
weather_list_pred = c(weather_list_obs, weatherlist_ca)
observation_df = observation_df
frost_threshold = frost_threshold
heat_threshold = heat_threshold
target_col_obs = 'begin_flowering_f5'
padding = 0.05
run_mean_window = 10

#species to do the analysis for
species <- unique(observation_df$species)


# #check if DATE is present, if so make it to Date
# weather_list_obs <- purrr::map(weather_list_obs, function(x){
#   if('date' %in% tolower(colnames(x)) & ('Date' %in% colnames(x)) == FALSE){
#     
#     #find out which version is present
#     i <- which(tolower(colnames(x)) == 'date')[1]
#     
#     
#     x$Date <- x[,i]
#     
#     return(x)
#   } else if('date' %in% tolower(colnames(x)) == FALSE){
#     stop('weather_list_obs does not contain a column which is called "Date", "DATE" or written in a different case. All weather data need a date column for the function to work')
#   } else { 
#     return(x)}
# })
# 
# #check if DATE is present, if so make it to Date
# weather_list_pred <- purrr::map(weather_list_pred, function(x){
#   if('date' %in% tolower(colnames(x)) & ('Date' %in% colnames(x)) == FALSE){
#     
#     #find out which version is present
#     i <- which(tolower(colnames(x)) == 'date')[1]
#     
#     
#     x$Date <- x[,i]
#     
#     return(x)
#   } else if('date' %in% tolower(colnames(x)) == FALSE){
#     stop('weather_list_obs does not contain a column which is called "Date", "DATE" or written in a different case. All weather data need a date column for the function to work')
#   } else { 
#     return(x)}
# })
# 
# 
# #-------------------------------------------------------------------------#
# # establish risks of the phenology data and the corrosponding weather data
# #-------------------------------------------------------------------------#
# 
# 
# 
# #get frost and heat risk for each day of the year
# frost_risk <- purrr::map(weather_list_obs, function(x){
#   x %>% 
#     mutate(doy = lubridate::yday(Date)) %>% 
#     group_by(doy) %>% 
#     summarise(chance_frost = sum(Tmin <= frost_threshold) / n()) %>% 
#     mutate(run_mean_frost = chillR::runn_mean(chance_frost, runn_mean = run_mean_window))
# }) %>% 
#   bind_rows(.id = 'location') %>% 
#   tidyr::expand_grid(species = species)
# 
# 
# #heat risk for each day of the year
# heat_risk <- purrr::map(weather_list_obs, function(x){
#   x %>% 
#     mutate(doy = lubridate::yday(Date)) %>% 
#     group_by(doy) %>% 
#     summarise(chance_heat = sum(Tmax >= heat_threshold) / n()) %>% 
#     mutate(run_mean_heat = chillR::runn_mean(chance_heat, runn_mean = run_mean_window))
# }) %>% 
#   bind_rows(.id = 'location') %>% 
#   tidyr::expand_grid(species = species)
# 
# 
# #save the target column in a different column, which I can access by name
# observation_df$target_pheno <- observation_df[, target_col_obs]
# 
# 
# #summarize the phenology data, know for each day of the year how many flowering records were measured for each species and location
# flower_sum <- observation_df %>% 
#   mutate(doy_pheno = lubridate::yday(target_pheno)) %>% 
#   group_by(species, location, doy_pheno) %>% 
#   summarise(n_flower = n())
# 
# 
# flower_sum <- flower_sum %>% 
#   mutate(doy = doy_pheno) %>% 
#   merge(frost_risk, by = c('location', 'species', 'location', 'doy'), all.x = TRUE)
# 
# ggplot(frost_risk, aes(x = doy, y = run_mean_frost, col = location))+
#   geom_line() +
#   geom_point(data = flower_sum, aes(x = doy, y = run_mean_frost, col = location)) +
#   facet_wrap(~species)
# ggsave('frost_risk_with_california.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')
# 
# 
# flower_sum <- observation_df %>% 
#   mutate(doy_pheno = lubridate::yday(target_pheno)) %>% 
#   group_by(species, location, doy_pheno) %>% 
#   summarise(n_flower = n())
# 
# flower_sum <- flower_sum %>% 
#   mutate(doy = doy_pheno) %>% 
#   merge(heat_risk, by = c('location', 'species', 'location', 'doy'), all.x = TRUE)
# 
# ggplot(heat_risk, aes(x = doy, y = run_mean_heat, col = location))+
#   geom_line() +
#   geom_point(data = flower_sum, aes(x = doy, y = run_mean_heat, col = location)) +
#   facet_wrap(~species)











#what difference for almonds does it make to have chico and modesto?





#-----------------------------------#
#begin function
#-----------------------------------#

weather_list_obs = weather_list_obs
weather_list_pred = weather_list_obs
observation_df = observation_df
# frost_threshold = frost_threshold
# heat_threshold = heat_threshold
# target_col_obs = x
# padding = p
run_mean_window = 10

if(is.null(weather_list_pred)){
  weather_list_pred <- weather_list_obs
}

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
avg_temps <- purrr::map(weather_list_obs, function(x){
  x %>% 
    dplyr::mutate(doy = lubridate::yday(.data$Date)) %>% 
    dplyr::group_by(.data$doy) %>% 
    dplyr::summarise(avg_Tmin = mean(.data$Tmin),
                     avg_Tmax = mean(.data$Tmax)) %>% 
    dplyr::mutate(avg_Tmin_run = chillR::runn_mean(.data$avg_Tmin, runn_mean = run_mean_window),
                  avg_Tmax_run = chillR::runn_mean(.data$avg_Tmax, runn_mean = run_mean_window))
}) %>% 
  dplyr::bind_rows(.id = 'location') %>% 
  tidyr::expand_grid(species = species) %>% 
  pivot_longer(cols = starts_with('avg')) %>% 
  filter(name %in% c('avg_Tmin_run', 'avg_Tmax_run')) %>% 
  mutate(species = tolower(species))

#combine with the phenological observations

obs_df <- observation_df %>% 
  mutate(pheno_first = lubridate::yday(begin_flowering_f5),
         pheno_full = lubridate::yday(flowering_f50),
         species = recode(species, `Japanese Plum` = 'European Plum')) %>% 
  dplyr::select(location, species, cultivar, pheno_first, pheno_full) %>% 
  pivot_longer(starts_with('pheno'), values_to = 'doy', names_to = 'phenostage') %>% 
  mutate(species = tolower(species)) %>% 
  filter((species %in% c('apple', 'european plum') & phenostage == 'pheno_first') | (species %in% c('pear', 'apricot', 'almond', 'pistachio', 'sweet cherry') & phenostage == 'pheno_full')) %>% 
  merge(avg_temps, by = c('species', 'location', 'doy')) 


avg_temps %>% 
  ggplot(aes(x = doy, y = value, col = location)) +
  geom_point(data = obs_df, aes(x = doy, y = value) ) +
  geom_line()+
  facet_grid(species~name)

#compare with observations


observation_df





#heat risk for each day of the year
heat_risk <- purrr::map(weather_list_obs, function(x){
  x %>% 
    dplyr::mutate(doy = lubridate::yday(.data$Date)) %>% 
    dplyr::group_by(.data$doy) %>% 
    dplyr::summarise(chance_heat = sum(.data$Tmax >= heat_threshold) / n()) %>% 
    dplyr::mutate(run_mean_heat = chillR::runn_mean(.data$chance_heat, runn_mean = run_mean_window))
}) %>% 
  dplyr::bind_rows(.id = 'location') %>% 
  tidyr::expand_grid(species = species)


#save the target column in a different column, which I can access by name
observation_df$target_pheno <- observation_df[, target_col_obs]


#summarize the phenology data, know for each day of the year how many flowering records were measured for each species and location
flower_sum <- observation_df %>% 
  dplyr::mutate(doy_pheno = lubridate::yday(.data$target_pheno)) %>% 
  dplyr::group_by(.data$species, .data$location, .data$doy_pheno) %>% 
  dplyr::summarise(n_flower = n())



max_frost <- observation_df %>%
  dplyr::mutate(doy_pheno = lubridate::yday(.data$target_pheno)) %>%
  dplyr::select(.data$species, .data$location, .data$cultivar, .data$flowering_f50, .data$doy_pheno) %>%
  merge(frost_risk, by.x = c('species', 'location', 'doy_pheno'), by.y = c('species', 'location', 'doy'), all = TRUE) %>%
  stats::na.omit() %>%
  dplyr::group_by(.data$species) %>%
  dplyr::summarise(max_risk_frost = max(.data$run_mean_frost))

max_heat <- observation_df %>%
  dplyr::mutate(doy_pheno = lubridate::yday(.data$target_pheno)) %>%
  dplyr::select(.data$species, .data$location, .data$cultivar, .data$flowering_f50, .data$doy_pheno) %>%
  merge(heat_risk, by.x = c('species', 'location', 'doy_pheno'), by.y = c('species', 'location', 'doy'), all = TRUE) %>%
  stats::na.omit() %>%
  dplyr::group_by(.data$species) %>%
  dplyr::summarise(max_risk_heat = max(.data$run_mean_heat))

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
  dplyr::group_by(.data$species) %>% 
  merge(flower_sum, by.y = c('location', 'doy_pheno', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
  stats::na.omit() %>% 
  dplyr::group_by(.data$species) %>% 
  dplyr::summarise(max_frost_risk = max(.data$run_mean_frost)) %>% 
  dplyr::mutate(flowering_type = target_col_obs,
                max_frost_risk_padded = .data$max_frost_risk + padding)


heat_limit <- heat_risk %>% 
  dplyr::group_by(.data$species) %>% 
  merge(flower_sum, by.y = c('location', 'doy_pheno', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
  stats::na.omit() %>% 
  dplyr::group_by(.data$species) %>% 
  dplyr::summarise(max_heat_risk = max(.data$run_mean_heat)) %>% 
  dplyr::mutate(flowering_type = target_col_obs,
                max_heat_risk_padded = .data$max_heat_risk + padding)


#combine upper and lower risk limit
#data.frame for each species saying what is the maximum frost and heat risk tolerated
limits_df <-  merge(frost_limit, heat_limit, by = c('species', 'flowering_type'))




#---------------------------------------------------------------------------#
#calculate window for the weahter data set used for phenological predictions
#---------------------------------------------------------------------------#


#calculate frost and heat risk for the other weather data

#get frost and heat risk for each day of the year
frost_risk <- purrr::map(weather_list_pred, function(x){
  x %>% 
    dplyr::mutate(doy = lubridate::yday(.data$Date)) %>% 
    dplyr::group_by(.data$doy) %>% 
    dplyr::summarise(chance_frost = sum(.data$Tmin <= frost_threshold) / n()) %>% 
    dplyr::mutate(run_mean_frost = chillR::runn_mean(.data$chance_frost, runn_mean = run_mean_window))
}) %>% 
  dplyr::bind_rows(.id = 'location') %>% 
  tidyr::expand_grid(species = species)

#heat risk for each day of the year
heat_risk <- purrr::map(weather_list_pred, function(x){
  x %>% 
    dplyr::mutate(doy = lubridate::yday(.data$Date)) %>% 
    dplyr::group_by(.data$doy) %>% 
    dplyr::summarise(chance_heat = sum(.data$Tmax >= heat_threshold) / n()) %>% 
    dplyr::mutate(run_mean_heat = chillR::runn_mean(.data$chance_heat, runn_mean = run_mean_window))
}) %>% 
  dplyr::bind_rows(.id = 'location') %>% 
  tidyr::expand_grid(species = species)



#translate the risk into boundaries in terms of doy
lower <- frost_risk %>% 
  merge(limits_df, by = c('species')) %>% 
  dplyr::filter(.data$run_mean_frost <=  .data$max_frost_risk) %>% 
  dplyr::group_by(.data$location, .data$species, .data$flowering_type) %>% 
  dplyr::summarise(min_doy = min(.data$doy))

#padded
lower_padded <- frost_risk %>% 
  merge(limits_df, by = c('species')) %>% 
  dplyr::filter(.data$run_mean_frost <=  .data$max_frost_risk_padded) %>% 
  dplyr::group_by(.data$location, .data$species, .data$flowering_type) %>% 
  dplyr::summarise(min_doy_padded = min(.data$doy))




#only take values left of the maximum heat risk
max_heat_risk <- heat_risk %>% 
  dplyr::group_by(location, .drop = FALSE) %>% 
  dplyr::summarise(max_risk = max(.data$run_mean_heat))

doy_max_risk_df <- merge(heat_risk, max_heat_risk, by = 'location') %>% 
  dplyr::group_by(.data$location) %>% 
  dplyr::filter(.data$run_mean_heat == .data$max_risk) %>% 
  dplyr::summarise(doy_max_risk = min(.data$doy))

upper <- heat_risk %>% 
  merge(doy_max_risk_df, by = 'location') %>% 
  dplyr::filter(.data$doy <= .data$doy_max_risk) %>% 
  merge(limits_df, by = c('species')) %>% 
  dplyr::filter(.data$run_mean_heat <= .data$max_heat_risk) %>% 
  dplyr::group_by(.data$location, .data$species, .data$flowering_type) %>% 
  dplyr::summarise(max_doy = max(.data$doy))

upper_padded <- heat_risk %>% 
  merge(doy_max_risk_df, by = 'location') %>% 
  dplyr::filter(.data$doy <= .data$doy_max_risk) %>% 
  merge(limits_df, by = c('species')) %>% 
  dplyr::filter(.data$run_mean_heat <= .data$max_heat_risk_padded) %>% 
  dplyr::group_by(.data$location, .data$species, .data$flowering_type) %>% 
  dplyr::summarise(max_doy_padded = max(.data$doy))


upper <- merge(upper, upper_padded, by = c('location', 'species', 'flowering_type'))
lower <- merge(lower, lower_padded, by = c('location', 'species', 'flowering_type'))


thermal_time_window_final <- merge.data.frame(lower, upper, by = c('location', 'species', 'flowering_type'))


return(thermal_time_window_final)






pad <- seq(0, 0.1, by = 0.01)
therm_windows <- purrr::map(pad, function(p){
  
  purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) LarsChill::get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
                                                                                                           weather_list_pred = weather_list_obs, 
                                                                                                           observation_df = observation_df, 
                                                                                                           frost_threshold = frost_threshold, 
                                                                                                           heat_threshold = heat_threshold, 
                                                                                                           target_col_obs = x,
                                                                                                           padding = p)) %>% 
    bind_rows() %>% 
    mutate(padding = p)
}) %>% 
  bind_rows()
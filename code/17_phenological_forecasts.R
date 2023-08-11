library(tidyverse)
library(ggplot2)
#library(readr)
library(chillR)
#library(devtools)
# install_github("larscaspersen/addition_chillR")
library(LarsChill)

### Read all the cultivars located in each single location

## Raw data
stations <-
  read.csv("data/weather_ready/weather_station_phenological_observations.csv") 
## Data filtered by Lars

## Find the combinations of models, cities and years we have in our database
#files<-list.files("./data/Future-sim-weather")
files <- list.files("data/future_weather/")

List_models <- data.frame()
for (a in files) {
  First_split <- str_split(a, pattern = '_') %>% unlist(.)
  Second_split <- str_split_fixed(First_split[3], pattern = "\\.", 4)
  Newrow <- data.frame(
    Code = First_split[1],
    Location = First_split[2],
    Scenario = Second_split[1],
    Model = Second_split[2],
    Year = Second_split[3]
  )
  List_models <- rbind.data.frame(List_models, Newrow)
}

#These two parameters are fixed for all the models
Tc = 36
theta_star = 279

#load performance file for weightening of ensemble predictions
performance_df <- read.csv('data/performance_fitted_models.csv')

#########################################################
###### Apricot
########################################################

## Load all the fitting results from the 10 repetitions
apricot_fit <- apple_fit <- eplum_fit <- jplum_fit <- pistachio_fit <- cherry_fit <- almond_fit <- pear_fit <-  list()

for(i in 1:10){
  apricot_fit[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting_clean/', prefix = paste0('repeat', i, '_'))
  apple_fit[[i]]  <- load_fitting_result('data/fitting/apple/', prefix = paste0('repeat', i, '_'))
  eplum_fit[[i]] <- load_fitting_result('data/fitting/european_plum/', prefix = paste0('repeat', i, '_'))
  jplum_fit[[i]] <- load_fitting_result('data/fitting/japanese_plum/', prefix = paste0('repeat', i, '_'))
  pistachio_fit[[i]] <- load_fitting_result('data/fitting/pistachio/', prefix = paste0('repeat', i, '_'))
  cherry_fit[[i]] <- load_fitting_result('data/fitting/sweet_cherry/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  #almond_fit[[i]]  <- load_fitting_result(path = 'data/fitting/almond/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  #almond_fit[[i]] <- load_fitting_result(path = 'data/fitting/almond/repeated_fitting_new_bounds', prefix = paste0('repeat', i, '_'))
  almond_fit[[i]]  <- load_fitting_result(path = 'data/fitting/almond/repeated_fitting_santomera_cleanly_saved/', prefix = paste0('repeat', i, '_'))
  pear_fit[[i]] <- load_fitting_result('data/fitting/pear/', prefix = paste0('repeat', i, '_'))
}



fit_list_combined <- list('Apricot' = apricot_fit,
                          'Apple' = apple_fit,
                 'European Plum' = eplum_fit,
                 'Japanese Plum' = jplum_fit,
                 'Pistachio' = pistachio_fit,
                 'Sweet Cherry' = cherry_fit,
                 'Almond' = almond_fit,
                 'Pear' = pear_fit)




#-------------------------------#
# FUTURE WEATHER ####
#-------------------------------#



#maybe read at first all weather data
future_weather_list <- chillR::load_temperature_scenarios('data/future_weather/', prefix = '')


#preparation of input for the wrapper function for the ensemble predictions
input_list <- list()

for(i in 1:length(fit_list_combined)){
  spec <- names(fit_list_combined)[i]
  
  for(cult in names(fit_list_combined[[i]][[1]])){
    input_list[[paste0(spec, '_', cult)]] <- list(
      'models' = purrr::map(fit_list_combined[[i]], cult),
      'confidence' =     performance_df %>% 
        filter(species == spec, cultivar == cult, split == 'Validation') %>% 
        arrange(repetition) %>% 
        pull(rpiq_adj)
    )
    
  }
}
#the function 

#wrapper function which makes the predictions for all cultivars / species for one seasonlist of generated weather
wrapper_ensemble_predictions <- function(SeasonList, input_list, plot_progress = FALSE){
  
  test <- purrr::map(input_list, function(input) pheno_ensemble_prediction(par_list = input$models, confidence = input$confidence, temp = SeasonList), .progress = plot_progress )
  
  
  sd_prediction <- test %>% 
    purrr::map('sd') %>% 
    do.call('cbind',.) %>% 
    reshape2::melt(id.vars = NULL, value.name = 'sd') %>% 
    rename(year = Var1, species_cultivar = Var2)
  
  prediction <- test %>% 
    purrr::map('predicted') %>% 
    do.call('cbind',.) %>% 
    reshape2::melt(id.vars = NULL, value.name = 'pheno_predicted') %>% 
    rename(year = Var1, species_cultivar = Var2) %>% 
    merge.data.frame(sd_prediction, by = c('year', 'species_cultivar')) %>% 
    arrange(year)
  
  
  
  return(prediction)
}


#
all_predictions <- purrr::map(1:length(future_weather_list), function(i){
  split_name <- names(future_weather_list)[i] %>% 
    str_split_1(pattern = c('_')) %>%
    str_split(pattern = '\\.') %>% 
    unlist()
  
  lat <- stations %>% 
    filter(station_name == split_name[1]) %>% 
    pull(latitude)
  
  out <- future_weather_list[[i]] %>% 
    chillR::stack_hourly_temps(latitude = lat) %>% 
    purrr::pluck('hourtemps') %>% 
    genSeasonList(years = 2001:2100) %>% 
    wrapper_ensemble_predictions(input_list) %>% 
    mutate(location = split_name[1],
           gcm = split_name[3],
           ssp = split_name[2],
           scenario_year = split_name[4])
  
  return(out)
}, .progress = TRUE)


all_predictions_df <- all_predictions %>% 
  bind_rows()

#write.csv(all_predictions_df, 'data/projected_bloomdates_ensemble.csv', row.names = FALSE)


#also do the predictions for apples

# input_list <- list()
# 
# i <-2
# spec <- 'Apple'
#   
# for(cult in names(fit_list_combined[[i]][[1]])){
#   input_list[[paste0(spec, '_', cult)]] <- list(
#     'models' = purrr::map(fit_list_combined[[i]], cult),
#     'confidence' =     performance_df %>% 
#       filter(species == spec, cultivar == cult, split == 'Validation') %>% 
#       arrange(repetition) %>% 
#       pull(rpiq_adj)
#   )
#   
#   }
# 
# apple_predictions <- purrr::map(1:length(future_weather_list), function(i){
#   split_name <- names(future_weather_list)[i] %>% 
#     str_split_1(pattern = c('_')) %>%
#     str_split(pattern = '\\.') %>% 
#     unlist()
#   
#   lat <- Lat %>% 
#     filter(station_name == split_name[1]) %>% 
#     pull(latitude)
#   
#   out <- future_weather_list[[i]] %>% 
#     chillR::stack_hourly_temps(latitude = lat) %>% 
#     purrr::pluck('hourtemps') %>% 
#     genSeasonList(years = 2001:2100) %>% 
#     wrapper_ensemble_predictions(input_list) %>% 
#     mutate(location = split_name[1],
#            gcm = split_name[3],
#            ssp = split_name[2],
#            scenario_year = split_name[4])
#   
#   return(out)
# }, .progress = TRUE)
# 
# 
# all_predictions_df <- read.csv('data/projected_bloomdates_ensemble.csv')
# 
# all_predictions_df <- apple_predictions %>% 
#   bind_rows() %>% 
#   rbind(all_predictions_df)
# 
# write.csv(all_predictions_df, 'data/projected_bloomdates_ensemble.csv', row.names = FALSE)


# #have to redo predictions for almonds and the location santomera
# all_predictions <- purrr::map(1:length(future_weather_list), function(i){
#   split_name <- names(future_weather_list)[i] %>% 
#     str_split_1(pattern = c('_')) %>%
#     str_split(pattern = '\\.') %>% 
#     unlist()
#   
#   if(split_name[1] != 'Santomera'){
#     return(NULL)
#   }
#   
#   lat <- stations %>% 
#     filter(station_name == split_name[1]) %>% 
#     pull(latitude)
#   
#   out <- future_weather_list[[i]] %>% 
#     chillR::stack_hourly_temps(latitude = lat) %>% 
#     purrr::pluck('hourtemps') %>% 
#     genSeasonList(years = 2001:2100) %>% 
#     wrapper_ensemble_predictions(input_list) %>% 
#     mutate(location = split_name[1],
#            gcm = split_name[3],
#            ssp = split_name[2],
#            scenario_year = split_name[4])
#   
#   return(out)
# }, .progress = TRUE)
# 
# santomera_predictions_df <- all_predictions %>% 
#   bind_rows()
# 
# all_predictions_df <- read.csv('data/projected_bloomdates_ensemble.csv')
# 
# all_predictions_df <- all_predictions_df %>% 
#   rbind(santomera_predictions_df)
# 
# write.csv(x = all_predictions_df, 'data/projected_bloomdates_ensemble.csv', row.names = FALSE)
# 

#have to redo predictions now for almonds except santomoera
# rm(all_predictions_df, santomera_predictions_df)
# 
# names(input_list)
# 
# #make almond predictions for all places but santomera (because we did it in the step before)
# sub_input <- input_list[grepl('Almond', names(input_list))]
# 
# all_predictions <- purrr::map(1:length(future_weather_list), function(i){
#   split_name <- names(future_weather_list)[i] %>% 
#     str_split_1(pattern = c('_')) %>%
#     str_split(pattern = '\\.') %>% 
#     unlist()
#   
#   if(split_name[1] == 'Santomera'){
#     return(NULL)
#   }
#   
#   lat <- stations %>% 
#     filter(station_name == split_name[1]) %>% 
#     pull(latitude)
#   
#   out <- future_weather_list[[i]] %>% 
#     chillR::stack_hourly_temps(latitude = lat) %>% 
#     purrr::pluck('hourtemps') %>% 
#     genSeasonList(years = 2001:2100) %>% 
#     wrapper_ensemble_predictions(sub_input) %>% 
#     mutate(location = split_name[1],
#            gcm = split_name[3],
#            ssp = split_name[2],
#            scenario_year = split_name[4])
#   
#   return(out)
# }, .progress = TRUE)
# 
# almond_predictions_df <- all_predictions %>% 
#   bind_rows()
# 
# all_predictions_df <- read.csv('data/projected_bloomdates_ensemble.csv')
# 
# all_predictions_df <- all_predictions_df %>% 
#   filter(!(location %in% c('Klein-Altendorf', 'Meknes', 'Cieza', 'Zaragoza', 'Sfax') & grepl('Almond', species_cultivar))) %>% 
#   rbind(almond_predictions_df)
# 
# write.csv(x = all_predictions_df, 'data/projected_bloomdates_ensemble.csv', row.names = FALSE)




# adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')
# 
# adamedor <- adamedor %>% 
#   filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),
#          !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014))
# 
# flower_summarized <- adamedor %>% 
#   group_by(species, location) %>% 
#   mutate(begin_flowering_f5 = lubridate::yday(begin_flowering_f5), 
#          flowering_f50 = lubridate::yday(flowering_f50)) %>% 
#   summarize(mean.begin_flowering_f5 = mean(begin_flowering_f5, na.rm = TRUE),
#             sd.begin_flowering_f5 = sd(begin_flowering_f5, na.rm = TRUE),
#             max_dist.begin_flowering_f5 = max(abs(begin_flowering_f5 - mean(begin_flowering_f5, na.rm = TRUE)), na.rm = TRUE) * 1.5,
#             mean.flowering_f50 = mean(flowering_f50, na.rm = TRUE),
#             sd.flowering_f50 = sd(flowering_f50, na.rm = TRUE),
#             max_dist.flowering_f50 = max(abs(flowering_f50 - mean(flowering_f50, na.rm = TRUE)), na.rm = TRUE) * 1.5) %>% 
#   reshape2::melt(id.vars = c('species', 'location')) %>% 
#   mutate(value = replace(value, value %in% c(NaN, NA, -Inf), NA)) %>% 
#   separate(col = variable, into = c('variable', 'flowering_type'), sep = '\\.') %>% 
#   reshape2::dcast(species + location + flowering_type ~ variable, value.var = 'value') %>% 
#   relocate(species, location, flowering_type, mean, sd, max_dist) %>% 
#   mutate(location = as.factor(location),
#          species = recode(species, 
#                           `European plum` = "European Plum", 
#                           `Japanese plum` = "Japanese Plum") )
# 
# adamedor %>% 
#   filter(species %in% c('Apricot', 'Pear', 'Sweet Cherry', 'Almond', 'European plum', 'Japanese plum', 'Pistachio'),
#          location %in% c('Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf', 'Sfax')) %>% 
#   dplyr::select(species, cultivar, location, begin_flowering_f5, flowering_f50) %>% 
#   mutate(begin_flowering_f5 = lubridate::yday(begin_flowering_f5), 
#          flowering_f50 = lubridate::yday(flowering_f50)) %>% 
#   reshape2::melt(id.var = c('species', 'cultivar', 'location')) %>% 
#   ggplot(aes(x = location, y = value)) +
#   geom_boxplot() +
#   facet_grid(species~variable)
# 
# #add the acceptable time window on that location
# flower_summarized <- flower_summarized %>% 
#   mutate(upper = mean + max_dist,
#          lower = mean - max_dist)
# 
# 
# adamedor %>% 
#   filter(species %in% c('Apricot', 'Pear', 'Sweet Cherry', 'Almond', 'European plum', 'Japanese plum', 'Pistachio'),
#          location %in% c('Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf', 'Sfax')) %>% 
#   dplyr::select(species, cultivar, location, begin_flowering_f5, flowering_f50) %>% 
#   mutate(begin_flowering_f5 = lubridate::yday(begin_flowering_f5), 
#          flowering_f50 = lubridate::yday(flowering_f50)) %>% 
#   reshape2::melt(id.var = c('species', 'cultivar', 'location'), variable.name = 'flowering_type') %>% 
#   merge.data.frame(flower_summarized, by = c('species', 'location', 'flowering_type')) %>% 
#   mutate(location = as.factor(location)) %>% 
#   ggplot(aes(x = as.numeric(location), y = value, group = location)) +
#   geom_rect(aes(ymin = lower, ymax = upper, xmin = as.numeric(location) - 0.45, xmax = as.numeric(location) + 0.45)) +
#   geom_boxplot() +
#   scale_x_continuous(breaks=c(1:5),
#                      labels=c('Cieza', 'Klein-Altendorf', 'Meknes', 'Sfax', 'Zaragoza'))+
#   facet_grid(species~flowering_type)
# #--> we need a window for each location and each species....




#--------------------------------------#
# HISTORIC WEATHER PREDICTIONS ####
#--------------------------------------#



#read historic weather generator data+
hist_gen_weather <- chillR::load_temperature_scenarios('data/hist-sim-weather/', prefix = 'hist_gen_2015')


hist_all_predictions <- purrr::map(1:length(hist_gen_weather), function(i){
  location <- names(hist_gen_weather)[i] 
  
  lat <- Lat %>% 
    filter(station_name == location) %>% 
    pull(latitude)
  
  colnames(hist_gen_weather[[i]]) <- c('DATE', 'Year', 'Month', 'Day', 'nodata', 'Tmin', 'Tmax')
  
  out <- hist_gen_weather[[i]] %>% 
    chillR::stack_hourly_temps(latitude = lat) %>% 
    purrr::pluck('hourtemps') %>% 
    genSeasonList(years = 2001:2100) %>% 
    wrapper_ensemble_predictions(input_list) %>% 
    mutate(location = location,
           gcm = 'historical',
           ssp = 'historical',
           scenario_year = 2015)
  
  return(out)
}, .progress = TRUE) %>% 
  bind_rows() %>% 
  separate(col = species_cultivar, into = c('species', 'cultivar'), sep = '_') %>% 
  mutate(flowering_type = ifelse(species %in% c('Sweet Cherry', 'Pistachio', 'Pear', 'Apricot'), yes = 'flowering_f50', no = 'begin_flowering_f5'))
  

#write.csv(hist_all_predictions, 'data/projected_bloomdates_ensemble_historic_scenarios.csv', row.names = FALSE)
hist_all_predictions <- read.csv('data/projected_bloomdates_ensemble_historic_scenarios.csv')

#need to indicate if the modelled phenology is f5 or f50
#cherry flowering_f50
#almond begin_flowering_f5
#apricot flowering_f50
#pistachio flowering_f50
#j plum f_5
#e plum f5
#pear f50



#-----------------------------------------------#
# PREDICTIONS FOR OBSERVED WEATHER ####
#-----------------------------------------------#

stations <-
  read.csv("data/weather_ready/weather_station_phenological_observations.csv")


#make predictions for the actual weather data
cka <- read.csv('data/weather_ready/cka_clean.csv') %>% 
  filter(Year < 2022)
cieza <- read.csv('data/weather_ready/cieza_clean_patched.csv')
sfax <- read.csv('data/weather_ready/sfax_clean.csv')
meknes <- read.csv('data/weather_ready/meknes_clean.csv')
zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv') %>% 
  filter(Year < 2022)

cka_season <- cka %>% 
  stack_hourly_temps(lat = stations$latitude[stations$station_name == 'Klein-Altendorf']) %>% 
  purrr::pluck('hourtemps') %>% 
  genSeasonList(years = min(cka$Year):max(cka$Year))
names(cka_season) <- min(cka$Year):max(cka$Year)

cieza_season <- cieza %>% 
  stack_hourly_temps(lat = stations$latitude[stations$station_name == 'Cieza']) %>% 
  purrr::pluck('hourtemps') %>% 
  genSeasonList(years = min(cieza$Year):max(cieza$Year))
names(cieza_season) <- min(cieza$Year):max(cieza$Year)

sfax_season <- sfax %>% 
  stack_hourly_temps(lat = stations$latitude[stations$station_name == 'Sfax']) %>% 
  purrr::pluck('hourtemps') %>% 
  genSeasonList(years = min(sfax$Year):max(sfax$Year))
names(sfax_season) <- min(sfax$Year):max(sfax$Year)

meknes_season <- meknes %>% 
  stack_hourly_temps(lat = stations$latitude[stations$station_name == 'Meknes']) %>% 
  purrr::pluck('hourtemps') %>% 
  genSeasonList(years = min(meknes$Year):max(meknes$Year))
names(meknes_season) <- min(meknes$Year):max(meknes$Year)

zaragoza_season <- zaragoza %>% 
  stack_hourly_temps(lat = stations$latitude[stations$station_name == 'Zaragoza']) %>% 
  purrr::pluck('hourtemps') %>% 
  genSeasonList(years = min(zaragoza$Year):max(zaragoza$Year))
names(zaragoza_season) <- min(zaragoza$Year):max(zaragoza$Year)
  
SeasonList <- list('Klein-Altendorf' = cka_season,
                   'Sfax' = sfax_season,
                   'Meknes' = meknes_season,
                   'Zaragoza' = zaragoza_season,
                   'Cieza' = cieza_season)

#obsweather predictions
obs_all_predictions <- purrr::map(1:length(SeasonList), function(i){
  location <- names(SeasonList)[i] 
  
  out <- SeasonList[[i]] %>% 
    wrapper_ensemble_predictions(input_list, plot_progress = TRUE) %>% 
    mutate(location = location,
           gcm = 'observed weather',
           ssp = 'observed weather',
           scenario_year = rep(names(SeasonList[[i]]), each = length(input_list)))
  
  
  return(out)
}, .progress = TRUE) %>% 
  bind_rows() %>% 
  separate(col = species_cultivar, into = c('species', 'cultivar'), sep = '_') %>% 
  mutate(flowering_type = ifelse(species %in% c('Sweet Cherry', 'Pistachio', 'Pear', 'Apricot'), yes = 'flowering_f50', no = 'begin_flowering_f5'))


#write.csv(x = obs_all_predictions, file = 'data/projected_bloomdates_ensemble_observed_weather.csv', row.names = FALSE)
obs_all_predictions <- read.csv('data/projected_bloomdates_ensemble_observed_weather.csv')















adamedor %>% 
  filter(species %in% c('Apricot', 'Pear', 'Sweet Cherry', 'Almond', 'European plum', 'Japanese plum', 'Pistachio'),
         location %in% c('Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf', 'Sfax')) %>% 
  dplyr::select(species, cultivar, location, begin_flowering_f5, flowering_f50) %>% 
  mutate(begin_flowering_f5 = lubridate::yday(begin_flowering_f5), 
         flowering_f50 = lubridate::yday(flowering_f50)) %>% 
  reshape2::melt(id.var = c('species', 'cultivar', 'location'), variable.name = 'flowering_type') %>% 
  merge.data.frame(flower_summarized, by = c('species', 'location', 'flowering_type')) %>% 
  mutate(location = as.factor(location),
         species = recode(species, 
                          `European plum` = "European Plum", 
                          `Japanese plum` = "Japanese Plum") ) %>% 
  filter((species %in% c('Sweet Cherry', 'Pistachio', 'Pear', 'Apricot') & flowering_type == 'flowering_f50') |
         (species %in% c('Almond', 'European Plum', 'Japanese Plum') & flowering_type == 'begin_flowering_f5')) %>% 
  ggplot(aes(x = as.numeric(location), y = value, group = location, fill = 'calculated time window')) +
  geom_rect(aes(ymin = lower, ymax = upper, xmin = as.numeric(location) - 0.1, xmax = as.numeric(location) + 0.1),
            position= position_nudge(x=-.25)) +
  geom_boxplot(width = 0.2, aes(fill = 'observed flowering'), position= position_nudge(x=-.25)) +
  geom_boxplot(data = obs_all_predictions, aes(x = as.numeric(as.factor(location)), y = pheno_predicted, group = location,
                                               fill = 'prediction obs weather'), 
               width = 0.2) +
  geom_boxplot(data = hist_all_predictions, aes(x = as.numeric(as.factor(location)), y = pheno_predicted, group = location,
                                                fill = 'historic simulation'), 
               width = 0.2, position= position_nudge(x=+.25)) +
  scale_x_continuous(breaks=c(1:5),
                     labels=c('Cieza', 'CKA', 'Meknes', 'Sfax', 'Zaragoza'))+
  scale_fill_manual(breaks = c('observed flowering', 'calculated time window', 'prediction obs weather', 'historic simulation'),
                    values = c('lightgreen', 'lightgrey', 'lightblue', 'lightsalmon')) +
  facet_wrap(~species) +
  theme_bw(base_size = 15)


#vielleicht sollte ich mal pro kutlivar / spezies gucken wie es aussieht
adamedor_reorg <- adamedor %>% 
  filter(species %in% c('Apricot', 'Pear', 'Sweet Cherry', 'Almond', 'European plum', 'Japanese plum', 'Pistachio'),
         location %in% c('Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf', 'Sfax')) %>% 
  dplyr::select(species, cultivar, location, year, begin_flowering_f5, flowering_f50) %>% 
  mutate(begin_flowering_f5 = lubridate::yday(begin_flowering_f5), 
         flowering_f50 = lubridate::yday(flowering_f50)) %>% 
  reshape2::melt(id.var = c('species', 'cultivar', 'location', 'year'), variable.name = 'flowering_type') %>% 
  mutate(location = as.factor(location),
         species = recode(species, 
                          `European plum` = "European Plum", 
                          `Japanese plum` = "Japanese Plum") ) %>% 
  filter((species %in% c('Sweet Cherry', 'Pistachio', 'Pear', 'Apricot') & flowering_type == 'flowering_f50') |
           (species %in% c('Almond', 'European Plum', 'Japanese Plum') & flowering_type == 'begin_flowering_f5'))




flower_summarized <- flower_summarized %>% 
  dplyr::filter(location %in% c('Sfax', 'Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf'),
                !(species %in% c('Peach', 'Olive'))) %>% 
  na.omit()













#use first degree relationship to infer on possible values 
#--> this will create multiple possible values 

#











t[1, comb[,1]] - t[1, comb[,2]]

combn(t[1,-1], 2) %>% 
  t() %>% 
  as.data.frame()   %>% 
  mutate(V1 = as.numeric(V1),
         V2 = as.numeric(V2),
         diff = ifelse(is.na(V1) | is.na(V2), yes = NA, no = V1 - V2))




obs_pred <- obs_all_predictions %>% 
  mutate(pheno = pheno_predicted) %>% 
  dplyr::select(year, species, cultivar, pheno, location, gcm, ssp, flowering_type)

hist_pred <- hist_all_predictions %>% 
  mutate(pheno = pheno_predicted,
         gcm = 'hist simulation',
         ssp = 'hist simulation') %>% 
  dplyr::select(year, species, cultivar, pheno, location, gcm, ssp, flowering_type)
  
  
t <- adamedor_reorg %>%   
  mutate(gcm = 'observed flowering',
         ssp = 'observed flowering',
         pheno = value) %>% 
  dplyr::select(year, species, cultivar, pheno, location, gcm, ssp, flowering_type) %>% 
  rbind.data.frame(obs_pred) %>% 
  rbind.data.frame(hist_pred) %>% 
  merge.data.frame(flower_summarized, by = c('species', 'location', 'flowering_type'), all.x = TRUE) %>%
  filter((species %in% c('Sweet Cherry', 'Pistachio', 'Pear', 'Apricot') & flowering_type == 'flowering_f50') |
           (species %in% c('Almond', 'European Plum', 'Japanese Plum') & flowering_type == 'begin_flowering_f5')) %>% 
  mutate(location = factor(location, 
                           levels = c('Sfax', 'Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf'), 
                           labels = c('Sfax', 'Meknes', 'Cieza', 'Zaragoza', 'CKA')),
         gcm = factor(gcm, 
                        levels = c('observed flowering', 'calculated time window', 'observed weather', 'hist simulation'),
                        labels = c('observed flowering', 'empirical time window', 'modelled: obs weather', 'modelled: hist sim weather')))

t %>% 
  mutate(species = factor(species, 
                          levels = c('Almond', 'Apricot', 'European Plum', 'Japanese Plum', 'Pear', 'Pistachio', 'Sweet Cherry'),
                          labels = c('Almond', 'Apricot', 'Eu. Plum', 'Jp. Plum', 'Pear', 'Pistachio', 'Sw. Cherry'))) %>% 
  ggplot(aes(x = 1, fill = gcm) ) +
  geom_rect(aes(ymin = lower, ymax = upper, xmin = 1 - 0.37, xmax = 1 - 0.125,
                fill = 'empirical time window')) +
  geom_boxplot(aes(y = pheno, fill = gcm)) +
  scale_fill_manual(breaks = c('observed flowering', 'empirical time window', 'modelled: obs weather', 'modelled: hist sim weather'),
                    values = c('lightgreen', 'lightgrey', 'lightblue', 'lightsalmon')) +
  facet_grid(species ~ location) +
  theme_bw(base_size = 15)





#calculate shift function between locations
#from cieza to zaragoza

flower_summarized %>% 
  filter(location %in% c('Klein-Altendorf', 'Zaragoza'),
         species %in% c('Pear', 'Sweet Cherry')) %>% 
  group_by(species) %>% 
  summarise(shift_upper = )


#simplify code by ditching flowering types for which we do not have simulated data
















# fit_list <- apricot_fit 
# 
# ## Extract the names of the cultivars selected by Lars
# cultivars <- names(fit_list[[1]])
# 
# Results_apricot <- data.frame()
# for (a in cultivars) {
#   #Filter the locations in which a certain cultivar is present
#   Locations <- filtered %>%
#     filter(species == "Apricot" & cultivar == a) %>%
#     pull(location) %>%
#     str_split(., pattern = ", ") %>%
#     unlist()
#   
#   # Control for missing locations (names that dont match)
#   if (length(Locations) < 1) {
#     print(paste("Not matching name for:", a, "!!!!!!"))
#     next
#   }
#   
#   # Loop to determine the scenarios included for that sampling location
#   for (b in Locations) {
#     
#     Scenarios <- filter(List_models, Location == b)
#     
#     if (nrow(Scenarios) == 0) {
#       
#       print(paste("No temperature data for:", a, b))
#       next
#     }
#     
#     SSP <- unique(Scenarios$Scenario)
#     # Loop to determine the models applied to this scenario
#     
#     for (c in SSP) {
#       Models <- filter(Scenarios, Scenario == c)
#       
#       Result <- purrr::map(unique(Models$Model), function(d) {
#         Code_2050 <- filter(Models, Model == d & Year == 2050)
#         Code_2085 <- filter(Models, Model == d & Year == 2085)
#         
#         File_code_2050 <- paste0(Code_2050$Code,
#                                  "_",
#                                  Code_2050$Location)
#         
#         File_code_2085 <- paste0(Code_2085$Code,
#                                  "_",
#                                  Code_2085$Location)
#         
#         # Meteo_2050 <- read.csv(paste0("./data/Future-sim-weather/",
#         #                               files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]),
#         #                        fileEncoding = "ISO-8859-1")
#         # Meteo_2085 <- read.csv(paste0("./data/Future-sim-weather/",
#         #                               files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]),
#         #                        fileEncoding = "ISO-8859-1")
#         
#         Meteo_2050 <- read.csv(paste0("./data/future_weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]),
#                                fileEncoding = "ISO-8859-1")
#         Meteo_2085 <- read.csv(paste0("./data/future_weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]),
#                                fileEncoding = "ISO-8859-1")
#         
#         
#         Latitude <- Lat %>%
#           dplyr::filter(station_name == b) %>%
#           pull(latitude)
#         
#         Meteo_2050 <-
#           stack_hourly_temps(Meteo_2050, latitude = Latitude)
#         Meteo_2085 <-
#           stack_hourly_temps(Meteo_2085, latitude = Latitude)
#         Meteo_2050 <-
#           genSeasonList(Meteo_2050$hourtemps, years = c(2001:2100))
#         Meteo_2085 <-
#           genSeasonList(Meteo_2085$hourtemps, years = c(2001:2100))
#         
#         par_list <- purrr::map(fit_list, a)
#         
#         #use rpiq of validation performance
#         confidence <- performance_df %>% 
#           filter(species == 'Apricot', cultivar == a, split == 'Validation') %>% 
#           arrange(repetition) %>% 
#           pull(rpiq_adj)
#         
#         # confidence <- purrr::map(fit_list, a) %>%
#         #   purrr::map_dbl('fbest')
#         # confidence <- 1 / confidence
#         
#         out_obs_2050 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2050)
#         
#         out_df_2050 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2050,
#           predicted = out_obs_2050$predicted,
#           sd = out_obs_2050$sd
#         )
#         
#         out_obs_2085 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2085)
#         
#         out_df_2085 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2085,
#           predicted = out_obs_2085$predicted,
#           sd = out_obs_2085$sd
#         )
#         Results <- rbind.data.frame(out_df_2050, out_df_2085)
#         return(Results)
#       }, .progress = TRUE) %>%
#         bind_rows()
#       Results_apricot <- rbind(Results_apricot, Result)
#       print(paste("Completed:", a, b, c))
#     }
#   }
# }
# #--> this is really slow :(

# 
# save(Results_apricot, file = "data/Results_apricot_future.RData")
# 
# 
# # load("data/Results_apricot_future.RData")
# 
# 
# # ggplot(Results_apricot, aes(x=cultivar, y=predicted, fill=Scenario))+
# #   geom_boxplot()+
# #   facet_grid(rows = vars(Year), cols = vars(Location), scales = "free_y")
# 
# ############################################################
# ### Almond
# #########################################################
# 
# ## Load all the fitting results from the 10 repetitions
# fit_list <- list()
# for (i in 1:10) {
#   fit_list[[i]] <-
#     load_fitting_result('data/fitting/almond/repeated_fitting/',
#                         prefix = paste0('repeat', i, '_'))
# }
# 
# cultivars <- names(fit_list[[1]])
# 
# Results_almond <- data.frame()
# for (a in cultivars) {
#   #Filter the locations in which a certain cultivar is present
#   Locations <- filter(filtered, species == "Almond" &
#                         cultivar == a) %>%
#     pull(4) %>%
#     str_split(., pattern = ", ") %>%
#     unlist()
#   # Control for missing locations (names that dont match)
#   if (length(Locations) < 1) {
#     print(paste("Not matching name for:", a, "!!!!!!"))
#     next
#   }
#   # Loop to determine the scenarios included for that sampling location
#   for (b in Locations) {
#     Scenarios <- filter(List_models, Location == b)
#     if (nrow(Scenarios) == 0) {
#       print(paste("No temperature data for:", a, b))
#       next
#     }
#     SSP <- unique(Scenarios$Scenario)
#     # Loop to determine the models applied to this scenario
#     for (c in SSP) {
#       Models <- filter(Scenarios, Scenario == c)
#       
#       Result <- purrr::map(unique(Models$Model), function(d) {
#         Code_2050 <- filter(Models, Model == d & Year == 2050)
#         Code_2085 <- filter(Models, Model == d & Year == 2085)
#         
#         File_code_2050 <- paste0(Code_2050$Code,
#                                  "_",
#                                  Code_2050$Location)
#         
#         File_code_2085 <- paste0(Code_2085$Code,
#                                  "_",
#                                  Code_2085$Location)
#         
#         Meteo_2050 <- read.csv(paste0("./data/Future-sim-weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]),
#                                fileEncoding = "ISO-8859-1")
#         Meteo_2085 <- read.csv(paste0("./data/Future-sim-weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]),
#                                fileEncoding = "ISO-8859-1")
#         Latitude <- Lat %>%
#           dplyr::filter(Locations == b) %>%
#           pull(Lat)
#         Meteo_2050 <-
#           stack_hourly_temps(Meteo_2050, latitude = Latitude)
#         Meteo_2085 <-
#           stack_hourly_temps(Meteo_2085, latitude = Latitude)
#         Meteo_2050 <-
#           genSeasonList(Meteo_2050$hourtemps, years = c(2000:2100))
#         Meteo_2085 <-
#           genSeasonList(Meteo_2085$hourtemps, years = c(2000:2100))
#         
#         par_list <- purrr::map(fit_list, a)
#         #use fvlaue instead
#         confidence <- purrr::map(fit_list, a) %>%
#           purrr::map_dbl('fbest')
#         confidence <- 1 / confidence
#         
#         out_obs_2050 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2050)
#         
#         out_df_2050 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2050,
#           predicted = out_obs_2050$predicted,
#           sd = out_obs_2050$sd
#         )
#         
#         out_obs_2085 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2085)
#         
#         out_df_2085 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2085,
#           predicted = out_obs_2085$predicted,
#           sd = out_obs_2085$sd
#         )
#         Results <- rbind.data.frame(out_df_2050, out_df_2085)
#         return(Results)
#       }, .progress = TRUE) %>%
#         bind_rows()
#       Results_almond <- rbind(Results_almond, Result)
#       print(paste("Completed:", a, b, c))
#     }
#   }
# }
# 
# save(Results_almond, file = "data/Results_almond_future.RData")
# 
# 
# ############################################################
# ### Sweet cherry
# #########################################################
# 
# ## Load all the fitting results from the 10 repetitions
# fit_list <- list()
# for (i in 1:10) {
#   fit_list[[i]] <-
#     load_fitting_result('data/fitting/sweet_cherry/repeated_fitting/',
#                         prefix = paste0('repeat', i, '_'))
# }
# 
# cultivars <- names(fit_list[[1]])
# 
# Results_sweet_cherry <- data.frame()
# for (a in cultivars) {
#   #Filter the locations in which a certain cultivar is present
#   Locations <-
#     filter(filtered, species == "Sweet Cherry" & cultivar == a) %>%
#     pull(4) %>%
#     str_split(., pattern = ", ") %>%
#     unlist()
#   # Control for missing locations (names that dont match)
#   if (length(Locations) < 1) {
#     print(paste("Not matching name for:", a, "!!!!!!"))
#     next
#   }
#   # Loop to determine the scenarios included for that sampling location
#   for (b in Locations) {
#     Scenarios <- filter(List_models, Location == b)
#     if (nrow(Scenarios) == 0) {
#       print(paste("No temperature data for:", a, b))
#       next
#     }
#     SSP <- unique(Scenarios$Scenario)
#     # Loop to determine the models applied to this scenario
#     for (c in SSP) {
#       Models <- filter(Scenarios, Scenario == c)
#       
#       Result <- purrr::map(unique(Models$Model), function(d) {
#         Code_2050 <- filter(Models, Model == d & Year == 2050)
#         Code_2085 <- filter(Models, Model == d & Year == 2085)
#         
#         File_code_2050 <- paste0(Code_2050$Code,
#                                  "_",
#                                  Code_2050$Location)
#         
#         File_code_2085 <- paste0(Code_2085$Code,
#                                  "_",
#                                  Code_2085$Location)
#         
#         Meteo_2050 <- read.csv(paste0("./data/Future-sim-weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]),
#                                fileEncoding = "ISO-8859-1")
#         Meteo_2085 <- read.csv(paste0("./data/Future-sim-weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]),
#                                fileEncoding = "ISO-8859-1")
#         Latitude <- Lat %>%
#           dplyr::filter(Locations == b) %>%
#           pull(Lat)
#         Meteo_2050 <-
#           stack_hourly_temps(Meteo_2050, latitude = Latitude)
#         Meteo_2085 <-
#           stack_hourly_temps(Meteo_2085, latitude = Latitude)
#         Meteo_2050 <-
#           genSeasonList(Meteo_2050$hourtemps, years = c(2000:2100))
#         Meteo_2085 <-
#           genSeasonList(Meteo_2085$hourtemps, years = c(2000:2100))
#         
#         par_list <- purrr::map(fit_list, a)
#         #use fvlaue instead
#         confidence <- purrr::map(fit_list, a) %>%
#           purrr::map_dbl('fbest')
#         confidence <- 1 / confidence
#         
#         out_obs_2050 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2050)
#         
#         out_df_2050 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2050,
#           predicted = out_obs_2050$predicted,
#           sd = out_obs_2050$sd
#         )
#         
#         out_obs_2085 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2085)
#         
#         out_df_2085 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2085,
#           predicted = out_obs_2085$predicted,
#           sd = out_obs_2085$sd
#         )
#         Results <- rbind.data.frame(out_df_2050, out_df_2085)
#         return(Results)
#       }, .progress = TRUE) %>%
#         bind_rows()
#       Results_sweet_cherry <- rbind(Results_sweet_cherry, Result)
#       print(paste("Completed:", a, b, c))
#     }
#   }
# }
# 
# save(Results_sweet_cherry, file = "data/Results_sweet_cherry_future.RData")
# 
# 
# ############################################################
# ### Pear
# #########################################################
# 
# ## Load all the fitting results from the 10 repetitions
# fit_list <- list()
# for (i in 1:10) {
#   fit_list[[i]] <-
#     load_fitting_result('data/fitting/pear/', prefix = paste0('repeat', i, '_'))
# }
# 
# cultivars <- names(fit_list[[1]])
# 
# Results_pear <- data.frame()
# for (a in cultivars) {
#   #Filter the locations in which a certain cultivar is present
#   Locations <- filter(filtered, species == "Pear" & cultivar == a) %>%
#     pull(4) %>%
#     str_split(., pattern = ", ") %>%
#     unlist()
#   # Control for missing locations (names that dont match)
#   if (length(Locations) < 1) {
#     print(paste("Not matching name for:", a, "!!!!!!"))
#     next
#   }
#   # Loop to determine the scenarios included for that sampling location
#   for (b in Locations) {
#     Scenarios <- filter(List_models, Location == b)
#     if (nrow(Scenarios) == 0) {
#       print(paste("No temperature data for:", a, b))
#       next
#     }
#     SSP <- unique(Scenarios$Scenario)
#     # Loop to determine the models applied to this scenario
#     for (c in SSP) {
#       Models <- filter(Scenarios, Scenario == c)
#       
#       Result <- purrr::map(unique(Models$Model), function(d) {
#         Code_2050 <- filter(Models, Model == d & Year == 2050)
#         Code_2085 <- filter(Models, Model == d & Year == 2085)
#         
#         File_code_2050 <- paste0(Code_2050$Code,
#                                  "_",
#                                  Code_2050$Location)
#         
#         File_code_2085 <- paste0(Code_2085$Code,
#                                  "_",
#                                  Code_2085$Location)
#         
#         Meteo_2050 <- read.csv(paste0("./data/Future-sim-weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]),
#                                fileEncoding = "ISO-8859-1")
#         Meteo_2085 <- read.csv(paste0("./data/Future-sim-weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]),
#                                fileEncoding = "ISO-8859-1")
#         Latitude <- Lat %>%
#           dplyr::filter(Locations == b) %>%
#           pull(Lat)
#         Meteo_2050 <-
#           stack_hourly_temps(Meteo_2050, latitude = Latitude)
#         Meteo_2085 <-
#           stack_hourly_temps(Meteo_2085, latitude = Latitude)
#         Meteo_2050 <-
#           genSeasonList(Meteo_2050$hourtemps, years = c(2000:2100))
#         Meteo_2085 <-
#           genSeasonList(Meteo_2085$hourtemps, years = c(2000:2100))
#         
#         par_list <- purrr::map(fit_list, a)
#         #use fvlaue instead
#         confidence <- purrr::map(fit_list, a) %>%
#           purrr::map_dbl('fbest')
#         confidence <- 1 / confidence
#         
#         out_obs_2050 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2050)
#         
#         out_df_2050 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2050,
#           predicted = out_obs_2050$predicted,
#           sd = out_obs_2050$sd
#         )
#         
#         out_obs_2085 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2085)
#         
#         out_df_2085 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2085,
#           predicted = out_obs_2085$predicted,
#           sd = out_obs_2085$sd
#         )
#         Results <- rbind.data.frame(out_df_2050, out_df_2085)
#         return(Results)
#       }, .progress = TRUE) %>%
#         bind_rows()
#       Results_pear <- rbind(Results_pear, Result)
#       print(paste("Completed:", a, b, c))
#     }
#   }
# }
# 
# save(Results_pear, file = "data/Results_pear_future.RData")
# 
# 
# ############################################################
# ### Pistachio
# #########################################################
# 
# ## Load all the fitting results from the 10 repetitions
# fit_list <- list()
# for (i in 1:10) {
#   fit_list[[i]] <-
#     load_fitting_result('data/fitting/pistachio/', prefix = paste0('repeat', i, '_'))
# }
# 
# cultivars <- names(fit_list[[1]])
# 
# Results_pistachio <- data.frame()
# for (a in cultivars) {
#   #Filter the locations in which a certain cultivar is present
#   Locations <- filter(filtered, species == "Pistachio" &
#                         cultivar == a) %>%
#     pull(4) %>%
#     str_split(., pattern = ", ") %>%
#     unlist()
#   # Control for missing locations (names that dont match)
#   if (length(Locations) < 1) {
#     print(paste("Not matching name for:", a, "!!!!!!"))
#     next
#   }
#   # Loop to determine the scenarios included for that sampling location
#   for (b in Locations) {
#     Scenarios <- filter(List_models, Location == b)
#     if (nrow(Scenarios) == 0) {
#       print(paste("No temperature data for:", a, b))
#       next
#     }
#     SSP <- unique(Scenarios$Scenario)
#     # Loop to determine the models applied to this scenario
#     for (c in SSP) {
#       Models <- filter(Scenarios, Scenario == c)
#       
#       Result <- purrr::map(unique(Models$Model), function(d) {
#         Code_2050 <- filter(Models, Model == d & Year == 2050)
#         Code_2085 <- filter(Models, Model == d & Year == 2085)
#         
#         File_code_2050 <- paste0(Code_2050$Code,
#                                  "_",
#                                  Code_2050$Location)
#         
#         File_code_2085 <- paste0(Code_2085$Code,
#                                  "_",
#                                  Code_2085$Location)
#         
#         Meteo_2050 <- read.csv(paste0("./data/Future-sim-weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]),
#                                fileEncoding = "ISO-8859-1")
#         Meteo_2085 <- read.csv(paste0("./data/Future-sim-weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]),
#                                fileEncoding = "ISO-8859-1")
#         Latitude <- Lat %>%
#           dplyr::filter(Locations == b) %>%
#           pull(Lat)
#         Meteo_2050 <-
#           stack_hourly_temps(Meteo_2050, latitude = Latitude)
#         Meteo_2085 <-
#           stack_hourly_temps(Meteo_2085, latitude = Latitude)
#         Meteo_2050 <-
#           genSeasonList(Meteo_2050$hourtemps, years = c(2000:2100))
#         Meteo_2085 <-
#           genSeasonList(Meteo_2085$hourtemps, years = c(2000:2100))
#         
#         par_list <- purrr::map(fit_list, a)
#         #use fvlaue instead
#         confidence <- purrr::map(fit_list, a) %>%
#           purrr::map_dbl('fbest')
#         confidence <- 1 / confidence
#         
#         out_obs_2050 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2050)
#         
#         out_df_2050 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2050,
#           predicted = out_obs_2050$predicted,
#           sd = out_obs_2050$sd
#         )
#         
#         out_obs_2085 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2085)
#         
#         out_df_2085 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2085,
#           predicted = out_obs_2085$predicted,
#           sd = out_obs_2085$sd
#         )
#         Results <- rbind.data.frame(out_df_2050, out_df_2085)
#         return(Results)
#       }, .progress = TRUE) %>%
#         bind_rows()
#       Results_pistachio <- rbind(Results_pistachio, Result)
#       print(paste("Completed:", a, b, c))
#     }
#   }
# }
# 
# save(Results_pistachio, file = "data/Results_pistachio_future.RData")
# 
# 
# 
# 
# 
# 
# ############################################################
# ### European plum // NO DATA
# #########################################################
# 
# ## Load all the fitting results from the 10 repetitions
# fit_list <- list()
# for (i in 1:10) {
#   fit_list[[i]] <-
#     load_fitting_result('data/fitting/european_plum/repeated_fitting/',
#                         prefix = paste0('repeat', i, '_'))
# }
# 
# cultivars <- names(fit_list[[1]])
# 
# Results_european_plum <- data.frame()
# for (a in cultivars) {
#   #Filter the locations in which a certain cultivar is present
#   Locations <-
#     filter(filtered, species == "European Plum" & cultivar == a) %>%
#     pull(4) %>%
#     str_split(., pattern = ", ") %>%
#     unlist()
#   # Control for missing locations (names that dont match)
#   if (length(Locations) < 1) {
#     print(paste("Not matching name for:", a, "!!!!!!"))
#     next
#   }
#   # Loop to determine the scenarios included for that sampling location
#   for (b in Locations) {
#     Scenarios <- filter(List_models, Location == b)
#     if (nrow(Scenarios) == 0) {
#       print(paste("No temperature data for:", a, b))
#       next
#     }
#     SSP <- unique(Scenarios$Scenario)
#     # Loop to determine the models applied to this scenario
#     for (c in SSP) {
#       Models <- filter(Scenarios, Scenario == c)
#       
#       Result <- purrr::map(unique(Models$Model), function(d) {
#         Code_2050 <- filter(Models, Model == d & Year == 2050)
#         Code_2085 <- filter(Models, Model == d & Year == 2085)
#         
#         File_code_2050 <- paste0(Code_2050$Code,
#                                  "_",
#                                  Code_2050$Location)
#         
#         File_code_2085 <- paste0(Code_2085$Code,
#                                  "_",
#                                  Code_2085$Location)
#         
#         Meteo_2050 <- read.csv(paste0("./data/Future-sim-weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]),
#                                fileEncoding = "ISO-8859-1")
#         Meteo_2085 <- read.csv(paste0("./data/Future-sim-weather/",
#                                       files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]),
#                                fileEncoding = "ISO-8859-1")
#         Latitude <- Lat %>%
#           dplyr::filter(Locations == b) %>%
#           pull(Lat)
#         Meteo_2050 <-
#           stack_hourly_temps(Meteo_2050, latitude = Latitude)
#         Meteo_2085 <-
#           stack_hourly_temps(Meteo_2085, latitude = Latitude)
#         Meteo_2050 <-
#           genSeasonList(Meteo_2050$hourtemps, years = c(2000:2100))
#         Meteo_2085 <-
#           genSeasonList(Meteo_2085$hourtemps, years = c(2000:2100))
#         
#         par_list <- purrr::map(fit_list, a)
#         #use fvlaue instead
#         confidence <- purrr::map(fit_list, a) %>%
#           purrr::map_dbl('fbest')
#         confidence <- 1 / confidence
#         
#         out_obs_2050 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2050)
#         
#         out_df_2050 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2050,
#           predicted = out_obs_2050$predicted,
#           sd = out_obs_2050$sd
#         )
#         
#         out_obs_2085 <-
#           pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2085)
#         
#         out_df_2085 <- data.frame(
#           cultivar = a,
#           Location = b,
#           Scenario = c,
#           Model = d,
#           Year = 2085,
#           predicted = out_obs_2085$predicted,
#           sd = out_obs_2085$sd
#         )
#         Results <- rbind.data.frame(out_df_2050, out_df_2085)
#         return(Results)
#       }, .progress = TRUE) %>%
#         bind_rows()
#       Results_european_plum <- rbind(Results_european_plum, Result)
#       print(paste("Completed:", a, b, c))
#     }
#   }
# }
# 
# save(Results_european_plum, file = "data/Results_european_plum_future.RData")

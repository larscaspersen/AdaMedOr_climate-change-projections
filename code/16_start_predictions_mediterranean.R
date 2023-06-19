library("tidyverse")
library("leaflet")

#setwd('../almond_pheno')


stations <- read.csv('data/weather_stations.csv') %>% 
  mutate(latitude  = Lat,
         longitude = Long) 

stations %>% 
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(lng = stations$longitude,
             lat = stations$latitude,
             label = stations$STATION.NAME)




library(chillR)
library(tidyverse)
library(LarsChill)
options(dplyr.summarise.inform = FALSE) #make summarize statement silent

zaragoza_cal <- read.csv('data/weather_raw/temp_zgz_1973-2022.csv')
stations <- rbind(stations,
                  data.frame(chillR_code = NA,
                             STATION.NAME = 'Zaragoza_cal',
                             CTRY = 'SP',
                             Lat = 41.65, 
                             Long = -0.88,
                             BEGIN = 19730101,
                             END  = 20220503,
                             distance = NA,
                             latitude = 41.65,
                             longitude = -0.88))

hist_sim_2019 <- load_temperature_scenarios('data/mediterranean_weather/', prefix = 'past')
observed_weather <- load_temperature_scenarios('data/mediterranean_weather/', 'fixed')

stat_names <- str_split(names(hist_sim_2019), pattern = '_') %>% 
  purrr::map_chr(1)

names(hist_sim_2019) <- stat_names

hist_sim_2019[['Zaragoza_cal']] <- observed_weather[['Zaragoza_cal']] <- zaragoza_cal


#load parameters for apricto 
source('code/utilities/load_save_fitting_results.R')

fit_list <- list()
for(i in 1:10){
  fit_list[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}


Tc = 36
theta_star = 279


#make ensemble prediction 
ensemble_prediction_df <- data.frame()
hist_sim_SeasonList <- list()
observed_SeasonList <- list()

#make season list of the historic simulations
for(i in 1:length(hist_sim_2019)){
  stat <- names(hist_sim_2019)[i]
  
  lat <- stations %>% 
    dplyr::filter(STATION.NAME == stat) %>% 
    pull(Lat)
  
  hourly_temp <- stack_hourly_temps(hist_sim_2019[[stat]], latitude = lat)
  obs_hourly_temp <- stack_hourly_temps(observed_weather[[stat]], latitude = lat)
  
  hist_sim_SeasonList[[stat]] <- genSeasonList(hourly_temp$hourtemps, years = (min(hist_sim_2019[[stat]]$Year)+1):max(hist_sim_2019[[stat]]$Year))
  observed_SeasonList[[stat]] <- genSeasonList(obs_hourly_temp$hourtemps, years = (min(observed_weather[[stat]]$Year)+1):max(observed_weather[[stat]]$Year))
  
}


pheno_ensemble_prediction <- function(par_list, confidence, temp, return_se = TRUE){
  
  predicted <- purrr::map(par_list, function(x){
    par <- x$xbest
    par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
    
    return_predicted_days(convert_parameters(par), 
                          modelfn = custom_PhenoFlex_GDHwrapper, 
                          SeasonList =temp)
  }) %>% 
    do.call('cbind', .) %>% 
    as.matrix()
  weights <- confidence / sum(confidence)
  
  weighted_pred <- as.vector(predicted %*% weights)
  
  sd_pred <- predicted %>% 
    t() %>% 
    as.data.frame() %>% 
    set_colnames( 1:nrow(predicted)) %>% 
    reshape2::melt(id.vars = NULL) %>% 
    group_by(variable) %>% 
    summarise(sd = sd(value)) %>% 
    pull(sd)
  #scale rpiq_values
  
  
  if(return_se){
    return(list(predicted = weighted_pred, sd = sd_pred,
                individual_pred = predicted))
  } else{
    return(weighted_pred)
  }
  
}


cultivars <- names(fit_list[[1]])
cult <- cultivars[1]

pred_df <- data.frame()
pb = txtProgressBar(min = 0, max = length(cultivars)*length(hist_sim_SeasonList), initial = 0, style = 3) 
stepi <- 0

test <- purrr::map(cultivars, function(cult){
  
  par_list <- purrr::map(fit_list, cult)
  
  #use fvlaue instead
  confidence <- purrr::map(fit_list, cult) %>% 
    purrr::map_dbl('fbest')
  
  confidence <- 1/confidence
  
  purrr::map(names(hist_sim_2019), function(loc){
    
    #out <- pheno_ensemble_prediction(par_list, confidence, temp = hist_sim_SeasonList[[loc]])
    
    out_obs <- pheno_ensemble_prediction(par_list, confidence, temp = observed_SeasonList[[loc]])
    
    out_df <- rbind.data.frame(
                     data.frame(cultivar = cult,
                                weather = 'observed_weather',
                                location = loc,
                                used_model = 'ensemble',
                                predicted = out_obs$predicted,
                                sd = out_obs$sd))
    
    return(out_df)
    
  }) %>% 
    bind_rows()
})

test_long <- test %>% 
  bind_rows()

mean_temp_df <- observed_weather %>% 
  bind_rows(.id = 'station') %>% 
  filter(Month %in% c(10:12, 1:4)) %>% 
  group_by(station) %>% 
  summarize(mean_Tmin = mean(Tmin),
            mean_Tmax = mean(Tmax))



test_long %>%
  filter(weather == 'observed_weather',
         cultivar %in% c('Canino', 'Harcot', 'Henderson')) %>% 
  merge.data.frame(mean_temp_df, by.x = 'location', by.y = 'station') %>% 
  ggplot(aes(y = reorder(location, mean_Tmin), x = predicted, fill = location)) + 
  ylab('Locations (ordered by decreasing Tmin in winter months)') +
  xlab('Predicted Bloom Date (doy)') +
  geom_boxplot() +
  facet_grid(~cultivar) +
  theme_bw(base_size = 15) + theme(legend.position = 'None')
ggsave('figures/Apricot_different_loc.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')




#create a polygon of temperature observations, 


find_hull <- function(df) df[chull(df$Tmin, df$Tmax), ]
hulls <- plyr::ddply(zaragoza_cal, "Month", find_hull)

hulls <- hulls %>% 
  filter(Month %in% c(10:12, 1:4)) %>% 
  mutate(Month = factor(Month, levels = c(10:12, 1:4)))



mon.labs <- month.name[c(10:12, 1:4)]
names(mon.labs) <-as.character(c(10:12, 1:4))

p <- observed_weather$Zaragoza_cal %>% 
  filter(Month %in% c(10:12, 1:4)) %>% 
  mutate(Month = factor(Month, levels = c(10:12, 1:4))) %>% 
  ggplot(aes ( x = Tmin, y = Tmax, col = 'Calibration: Zaragoza')) +
  geom_point() +
  facet_wrap(~Month, 
             labeller = labeller(Month = mon.labs))


#now add temperatures of a station which works well and of one which does not work well
#silfike bad, forli good

forli <- observed_weather$FORLI %>% 
  filter(Month %in% c(10:12, 1:4)) %>% 
  mutate(Month = factor(Month, levels = c(10:12, 1:4)))

silfike <- observed_weather$SILIFKE %>% 
  filter(Month %in% c(10:12, 1:4)) %>% 
  mutate(Month = factor(Month, levels = c(10:12, 1:4)))


p + 
  geom_point(data = forli, aes(x = Tmin, y = Tmax, col = 'Forli / Italy'), size = 0.7) + 
  geom_point(data = silfike, aes(x = Tmin, y = Tmax, col = 'Silfike / Türkiye'), size = 0.7) +
  scale_color_manual(values = c('black', 'deepskyblue', 'red')) +
  geom_polygon(data = hulls, alpha = 0.3) +
  ylab('Maximum Temperature (°C)') +
  xlab('Minimum Temperature (°C)') +
  theme_bw(base_size = 15)
ggsave('figures/overlap_temp.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')



#see if the same happens also to other species






#how to create a polygon from the dotplot
#for each temperature in Tmin, find highest and lowest temperature in Tmax


fit_list_almond <- list()
for(i in 1:10){
  fit_list_almond[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}


almond_ensemble <- purrr::map(names(fit_list_almond[[1]]), function(cult){
  
  par_list <- purrr::map(fit_list_almond, cult)
  
  #use fvlaue instead
  confidence <- purrr::map(fit_list_almond, cult) %>% 
    purrr::map_dbl('fbest')
  
  confidence <- 1/confidence
  
  purrr::map(names(hist_sim_2019), function(loc){
    
    #out <- pheno_ensemble_prediction(par_list, confidence, temp = hist_sim_SeasonList[[loc]])
    
    out_obs <- pheno_ensemble_prediction(par_list, confidence, temp = observed_SeasonList[[loc]])
    
    out_df <- data.frame(cultivar = cult,
               weather = 'observed_weather',
               location = loc,
               predicted = out_obs$predicted,
               sd = out_obs$sd)
    
    return(out_df)
    
  }) %>% 
    bind_rows()
})

almond_ensemble_long <- almond_ensemble %>% 
  bind_rows()

almond_ensemble_long %>%
  filter(cultivar %in% names(fit_list_almond[[1]])[11:20]) %>% 
  merge.data.frame(mean_temp_df, by.x = 'location', by.y = 'station') %>% 
  ggplot(aes(y = reorder(location, mean_Tmin), x = predicted, fill = location)) + 
  ylab('Locations (ordered by decreasing Tmin in winter months)') +
  xlab('Predicted Bloom Date (doy)') +
  geom_boxplot() +
  facet_grid(~cultivar) +
  theme_bw(base_size = 15) + theme(legend.position = 'None')

almond_ensemble_long %>%
  filter(cultivar %in% names(fit_list_almond[[1]])[1:10]) %>% 
  merge.data.frame(mean_temp_df, by.x = 'location', by.y = 'station') %>% 
  ggplot(aes(y = reorder(location, mean_Tmin), x = predicted, fill = location)) + 
  ylab('Locations (ordered by decreasing Tmin in winter months)') +
  xlab('Predicted Bloom Date (doy)') +
  geom_boxplot() +
  facet_grid(~cultivar) +
  theme_bw(base_size = 15) + theme(legend.position = 'None')

almond_ensemble_long %>%
  filter(cultivar %in% names(fit_list_almond[[1]])[21:30]) %>% 
  merge.data.frame(mean_temp_df, by.x = 'location', by.y = 'station') %>% 
  ggplot(aes(y = reorder(location, mean_Tmin), x = predicted, fill = location)) + 
  ylab('Locations (ordered by decreasing Tmin in winter months)') +
  xlab('Predicted Bloom Date (doy)') +
  geom_boxplot() +
  facet_grid(~cultivar) +
  theme_bw(base_size = 15) + theme(legend.position = 'None')

#visualize validation outcome

library(chillR)
library(tidyverse)
library(LarsChill)

source('code/utilities/load_save_fitting_results.R')

#load fitting results

#species <- c('data/fitting/')
#species <- c('almond', 'apricot', 'european_plum', 'japanese_plum',  'pistachio', 'sweet_cherry', 'pear')
species <- c('almond', 'apricot', 'european_plum', 'japanese_plum',  'pistachio', 'sweet_cherry')

#number of repititions
r <- 10

apricot_fit <- apricot_fit2 <- eplum_fit <- jplum_fit <- pistachio_fit <- cherry_fit <- almond_fit <- pear_fit <-  list()

for(i in 1:r){
  apricot_fit[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  apricot_fit2[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting_with_cieza/', prefix = paste0('repeat', i, '_'))
  eplum_fit[[i]] <- load_fitting_result('data/fitting/european_plum/', prefix = paste0('repeat', i, '_'))
  jplum_fit[[i]] <- load_fitting_result('data/fitting/japanese_plum/', prefix = paste0('repeat', i, '_'))
  pistachio_fit[[i]] <- load_fitting_result('data/fitting/pistachio/', prefix = paste0('repeat', i, '_'))
  cherry_fit[[i]] <- load_fitting_result('data/fitting/sweet_cherry/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  almond_fit[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  pear_fit[[i]] <- load_fitting_result('data/fitting/pear/', prefix = paste0('repeat', i, '_'))
}

  
names(apricot_fit2[[1]])
#add the cultivars which are not in apricot fit2 from apricot fit
apricot_zaragoza_only  <- names(apricot_fit[[1]])[!names(apricot_fit[[1]]) %in% names(apricot_fit2[[1]])]

#add cultivars which I had not to refit
for(i in 1:r){
  for(cult in apricot_zaragoza_only){
    apricot_fit2[[i]][[cult]] <- apricot_fit[[i]][[cult]]
  }

}


fit_list <- list('Apricot' = apricot_fit2,
                 'European Plum' = eplum_fit,
                 'Japanese Plum' = jplum_fit,
                 'Pistachio' = pistachio_fit,
                 'Sweet Cherry' = cherry_fit,
                 'Almond' = almond_fit,
                 'Pear' = pear_fit)


#need to load and recreate all the splits I did in the other scripts, because I forgot to save it


# #need zaragoza and klein altendorf
# cka <- read.csv('data/weather_ready/temp_cka_1958-2022.csv')
# zaragoza <- read.csv('data/weather_raw/temp_zgz_1973-2022.csv')
# sfax <- read.csv('data/weather_ready/sfax_1973-2021_fixed.csv')
# cieza <- readxl::read_xlsx('data/weather_raw/Cieza(95_22)Tmax&Tmin.xlsx')
# meknes <- read.csv('data/weather_ready/meknes-bassatine-fixed_1973-2022.csv')
# 
# #cieza needs to be modified a bit
# cieza <- cieza %>%
#   dplyr::select(-Hours) %>%
#   mutate(Month = lubridate::month(Date),
#          Day = lubridate::day(Date))
# 
# 
# str(cka)
# str(zaragoza)
# str(sfax)
# str(cieza)
# str(meknes)
# 
# #bring it to the same format
# 
# cka_clean <- cka %>%
#   mutate(Date = lubridate::ymd(DATE)) %>%
#   dplyr::select(Date, Year, Month, Day, Tmin, Tmax)
# 
# zaragoza_clean <- zaragoza %>%
#   mutate(Date = lubridate::ymd(paste0(Year, '-', Month, '-', Day))) %>%
#   dplyr::select(Date, Year, Month, Day, Tmin, Tmax)
# 
# sfax_clean <- sfax %>%
#   mutate(Date = lubridate::ymd_hms(DATE)) %>%
#   mutate(Date = as.Date.character(Date)) %>%
#   dplyr::select(Date, Year, Month, Day, Tmin, Tmax)
# 
# cieza_clean <- cieza %>%
#   mutate(Date = as.Date(Date) ) %>%
#   dplyr::select(Date, Year, Month, Day, Tmin, Tmax)
# 
# meknes_clean <- meknes %>%
#   mutate(Date = as.Date(DATE)) %>%
#   dplyr::select(Date, Year, Month, Day, Tmin, Tmax)
# 
# 
# write.csv(sfax_clean, 'data/weather_ready/sfax_clean.csv', row.names = FALSE)
# write.csv(zaragoza_clean, 'data/weather_ready/zaragoza_clean.csv', row.names = FALSE)
# write.csv(cka_clean, 'data/weather_ready/cka_clean.csv', row.names = FALSE)
# write.csv(cieza_clean, 'data/weather_ready/cieza_clean.csv', row.names = FALSE)
# write.csv(meknes_clean, 'data/weather_ready/meknes_clean.csv', row.names = FALSE)
# 
# #make hourly
# coord_zaragoza <- c(41.65, -0.88)
# coord_cka <- c(50.61, 6.99)
# coord_sfax <-c(34.75, 10.75)
# coord_cieza <-c(38.24, -1.41)
# coord_meknes <- c(33.88, -5.54)
# 
# 
# stations_pheno_observations <- data.frame(station_name = c('Zaragoza', 'Klein-Altendorf', 'Sfax', 'Cieza', 'Meknes'),
#            country = c('Spain', 'Germany', 'Tunisia', 'Spain', 'Morocco'),
#            latitude = c(coord_zaragoza[1], coord_cka[1], coord_sfax[1], coord_cieza[1], coord_meknes[1]),
#            longitude = c(coord_zaragoza[2], coord_cka[2], coord_sfax[2], coord_cieza[2], coord_meknes[2]))
# 
# write.csv(stations_pheno_observations, 'data/weather_ready/weather_station_phenological_observations.csv', row.names = FALSE)

cka <- read.csv('data/weather_ready/cka_clean.csv')
zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv')
sfax <- read.csv('data/weather_ready/sfax_clean.csv')
meknes <- read.csv('data/weather_ready/meknes_clean.csv')
cieza <- read.csv('data/weather_ready/cieza_clean.csv')

weather_stations <- read.csv('data/weather_ready/weather_station_phenological_observations.csv')



cka_hourly <- stack_hourly_temps(cka, latitude = weather_stations$latitude[weather_stations$station_name == 'Klein-Altendorf'])
zaragoza_hourly <- stack_hourly_temps(zaragoza, latitude =  weather_stations$latitude[weather_stations$station_name == 'Zaragoza'])
sfax_hourly <- stack_hourly_temps(sfax, latitude =  weather_stations$latitude[weather_stations$station_name == 'Sfax'])
cieza_hourly <- stack_hourly_temps(cieza, latitude =  weather_stations$latitude[weather_stations$station_name == 'Cieza'])
meknes_hourly <- stack_hourly_temps(meknes, latitude =  weather_stations$latitude[weather_stations$station_name == 'Meknes'])

cka_hourly <- cka_hourly$hourtemps
zaragoza_hourly <- zaragoza_hourly$hourtemps
sfax_hourly <- sfax_hourly$hourtemps
cieza_hourly <- cieza_hourly$hourtemps
meknes_hourly <- meknes_hourly$hourtemps

#make seasonal list
cka_season <- genSeasonList(cka_hourly, years = min(cka$Year):max(cka$Year))
zaragoza_season <- genSeasonList(zaragoza_hourly, years = min(zaragoza$Year):max(zaragoza$Year))
sfax_season <- genSeasonList(sfax_hourly, years = min(sfax$Year):max(sfax$Year))
cieza_season <- genSeasonList(cieza_hourly, years = min(cieza$Year):max(cieza$Year))
meknes_season <- genSeasonList(meknes_hourly, years = min(meknes$Year):max(meknes$Year))


names(cka_season) <- min(cka$Year):max(cka$Year)
names(zaragoza_season) <- min(zaragoza$Year):max(zaragoza$Year)
names(sfax_season) <- min(sfax$Year):max(sfax$Year)
names(cieza_season) <-  min(cieza$Year):max(cieza$Year)
names(meknes_season) <- min(meknes$Year):max(meknes$Year)

SeasonList <- list('Zaragoza' = zaragoza_season, 
                   'Klein-Altendorf' = cka_season,
                   'Sfax' = sfax_season,
                   'Cieza' = cieza_season,
                   'Meknes' = meknes_season)


rm(meknes, meknes_season, sfax, sfax_season, cka, cka_season, zaragoza, zaragoza_season, cieza, cieza_season)


#need to do the splitting again and then safe the results!!!!
#read master pheno file

master_pheno_split <- read.csv('data/master_phenology_repeated_splits.csv')

#now I should be able to make forecast for each data point in the master pheno file



Tc = 36
theta_star = 279


sub_SeasonList <- list()
sub <- NULL
prediction_df <- data.frame()



for(spec in names(fit_list)){
  for(i in 1:r){
    
    cultivars <- names(fit_list[[spec]][[i]])
    for(cult in cultivars){
      
      
      
      #extract parameters
      par <- fit_list[[spec]][[i]][[cult]]$xbest 
      #add fixed parameters
      par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
      
      #subset master file
      sub <- master_pheno_split %>% 
        filter(species == spec,
               cultivar == cult,
               repetition == i)
      
      #generate seasonlist
      sub_SeasonList <-  purrr::map2(sub$location, sub$year, function(loc, yr) SeasonList[[loc]][[as.character(yr)]])
      
      #predict bloom days
      sub$pred <- return_predicted_days(convert_parameters(par), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList =sub_SeasonList)
      
      prediction_df <- rbind.data.frame(prediction_df,
                                        sub)
      

      
    }
  }
}
#maybe different order of data??

#add iqr to prediction data.frame
iqr_df <- prediction_df %>% 
  group_by(species, cultivar, repetition) %>% 
  summarise(iqr = IQR(pheno))


#now I can generate performance data
performance <- prediction_df %>% 
  merge.data.frame(iqr_df, by = c('species', 'cultivar', 'repetition')) %>% 
  dplyr::group_by(species, cultivar, repetition, split, iqr) %>% 
  dplyr::summarise(rmse = RMSEP(predicted = pred, observed = pheno),
            mean_bias = mean(pred - pheno)) %>% 
  mutate(rpiq_adj = iqr / rmse)


performance %>% 
  filter(species == 'Almond') %>% 
  ggplot(aes(y = cultivar, fill = split, x = rpiq_adj)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  scale_y_discrete(limits=rev)


performance %>% 
  filter(species == 'Apricot') %>% 
  ggplot(aes(y = cultivar, fill = split, x = rpiq_adj)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  scale_y_discrete(limits=rev)

performance %>% 
  filter(species == 'Sweet Cherry') %>% 
  ggplot(aes(y = cultivar, fill = split, x = rpiq_adj)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  scale_y_discrete(limits=rev)

performance %>% 
  filter(species == 'Japanese Plum') %>% 
  ggplot(aes(y = cultivar, fill = split, x = rpiq_adj)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  scale_y_discrete(limits=rev)

performance %>% 
  filter(species == 'European Plum') %>% 
  ggplot(aes(y = cultivar, fill = split, x = rpiq_adj)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  scale_y_discrete(limits=rev)

performance %>% 
  filter(species == 'Pistachio') %>% 
  ggplot(aes(y = cultivar, fill = split, x = rpiq_adj)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  scale_y_discrete(limits=rev)

performance %>% 
  filter(species == 'Pear') %>% 
  ggplot(aes(y = cultivar, fill = split, x = rpiq_adj)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  scale_y_discrete(limits=rev)



median_df <- performance %>% 
  filter(split == 'Calibration') %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj),
            median_rmse = median(rmse),
            median_bias = median(mean_bias))

performance %>% 
  merge.data.frame(median_df, by = c('species', 'cultivar')) %>% 
  ggplot(aes(y = reorder(cultivar, median_rmse), fill = split, x = rmse)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 5, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  scale_y_discrete(limits=rev) +
  ylab('Cultivar (ordered by incrasing RMSE of calibration data)') +
  xlab('Root Mean Square Error in Bloom Prediction (Days)') +
  facet_grid(rows = vars(species), scales = 'free_y', space = "free")
ggsave('figures/rmse_all_cult.jpeg', device = 'jpeg',
       height = 40, width = 20, units = 'cm')

performance %>% 
  merge.data.frame(median_df, by = c('species', 'cultivar')) %>% 
  ggplot(aes(y = reorder(cultivar, median_rpiq), fill = split, x = rpiq_adj)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  ylab('Cultivar (ordered by decrasing RPIQ of calibration data)') +
  xlab('Ratio of Performance to Interquartile Distance') +
  facet_grid(rows = vars(species), scales = 'free_y', space = "free")
ggsave('figures/rpiq_all_cult.jpeg', device = 'jpeg',
       height = 40, width = 20, units = 'cm')

performance %>% 
  merge.data.frame(median_df, by = c('species', 'cultivar')) %>% 
  ggplot(aes(y = reorder(cultivar, median_bias), fill = split, x = mean_bias)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  geom_vline(xintercept = c(-1, 1), linetype = 'dashed') +
  theme_bw() +
  ylab('Cultivar (ordered by decreasing mean bias of calibration data)') +
  xlab('Mean Bias (Days)') +
  theme_bw(base_size = 15) +
  facet_grid(rows = vars(species), scales = 'free_y', space = "free")
ggsave('figures/mean_bias_cult.jpeg', device = 'jpeg',
       height = 40, width = 20, units = 'cm')


#can I use the ensemble to make predictions? maybe use the rpiq of validation data as a weight for the prediction?

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
    return(list(predicted = weighted_pred, sd = sd_pred))
  } else{
    return(weighted_pred)
  }
  
}




ensemble_prediction_df <- data.frame()

for(spec in names(fit_list)){

    cultivars <- names(fit_list[[spec]][[1]])
    for(cult in cultivars){
      
      par_list <- purrr::map(fit_list[[spec]], cult)
     
      sub <- master_pheno_split %>% 
        filter(species == spec, cultivar == cult, repetition == 1)
      
      sub_SeasonList <-  purrr::map2(sub$location, sub$year, function(loc, yr) SeasonList[[loc]][[as.character(yr)]])
      
      confidence <- performance %>%
        filter(species == spec, cultivar == cult, split == 'Validation') %>% 
        pull(rpiq_adj)
        
      
      pred_out <- pheno_ensemble_prediction(par_list, confidence, temp = sub_SeasonList)

      
      sub$pred <- pred_out$predicted
      sub$sd <- pred_out$sd
      
      ensemble_prediction_df <- rbind.data.frame(ensemble_prediction_df,
                                                 sub)
      

    }

}

ensemble_prediction_df %>% 
  filter(species == 'Apricot') %>% 
  ggplot(aes(x = pheno, y = pred)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + 
  geom_errorbar(aes(ymin = pred - sd, ymax = pred + sd)) +
  facet_wrap(~cultivar) +
  ylab('Predicted Bloom (Day of Year)') +
  xlab('Observed Bloom (Day of Year)') +
  theme_bw(base_size = 15) 
ggsave('figures/ensemble_prediction_apricot.jpeg',
       height = 15, width = 20, units = 'cm', device = 'jpeg')
  

ensemble_prediction_df %>% 
  filter(species == 'Almond') %>% 
  ggplot(aes(x = pheno, y = pred)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + 
  geom_errorbar(aes(ymin = pred - sd, ymax = pred + sd)) +
  facet_wrap(~cultivar) +
  ylab('Predicted Bloom (Day of Year)') +
  xlab('Observed Bloom (Day of Year)') +
  theme_bw(base_size = 15) 
ggsave('figures/ensemble_prediction_almond.jpeg',
       height = 20, width = 30, units = 'cm', device = 'jpeg')

ensemble_prediction_df %>% 
  filter(species == 'Pear') %>% 
  ggplot(aes(x = pheno, y = pred)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + 
  geom_errorbar(aes(ymin = pred - sd, ymax = pred + sd)) +
  facet_wrap(~cultivar) +
  ylab('Predicted Bloom (Day of Year)') +
  xlab('Observed Bloom (Day of Year)') +
  theme_bw(base_size = 15) 
ggsave('figures/ensemble_prediction_pear.jpeg',
       height = 15, width = 25, units = 'cm', device = 'jpeg')


ensemble_prediction_df %>% 
  filter(species == 'Sweet Cherry') %>% 
  ggplot(aes(x = pheno, y = pred)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + 
  geom_errorbar(aes(ymin = pred - sd, ymax = pred + sd)) +
  facet_wrap(~cultivar) +
  ylab('Predicted Bloom (Day of Year)') +
  xlab('Observed Bloom (Day of Year)') +
  theme_bw(base_size = 15) 
ggsave('figures/ensemble_prediction_cherry.jpeg',
       height = 20, width = 30, units = 'cm', device = 'jpeg')

ensemble_prediction_df %>% 
  filter(species == 'Pistachio') %>% 
  ggplot(aes(x = pheno, y = pred)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + 
  geom_errorbar(aes(ymin = pred - sd, ymax = pred + sd)) +
  facet_wrap(~cultivar) +
  ylab('Predicted Bloom (Day of Year)') +
  xlab('Observed Bloom (Day of Year)') +
  theme_bw(base_size = 15) 
ggsave('figures/ensemble_prediction_pistachio.jpeg',
       height = 10, width = 15, units = 'cm', device = 'jpeg')

ensemble_prediction_df %>% 
  filter(species == 'European Plum') %>% 
  ggplot(aes(x = pheno, y = pred)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + 
  geom_errorbar(aes(ymin = pred - sd, ymax = pred + sd)) +
  facet_wrap(~cultivar) +
  ylab('Predicted Bloom (Day of Year)') +
  xlab('Observed Bloom (Day of Year)') +
  theme_bw(base_size = 15) 
ggsave('figures/ensemble_prediction_european_plum.jpeg',
       height = 10, width = 15, units = 'cm', device = 'jpeg')

ensemble_prediction_df %>% 
  filter(species == 'Japanese Plum') %>% 
  ggplot(aes(x = pheno, y = pred)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + 
  geom_errorbar(aes(ymin = pred - sd, ymax = pred + sd)) +
  facet_wrap(~cultivar) +
  ylab('Predicted Bloom (Day of Year)') +
  xlab('Observed Bloom (Day of Year)') +
  theme_bw(base_size = 15) 
ggsave('figures/ensemble_prediction_japanese_plum.jpeg',
       height = 10, width = 15, units = 'cm', device = 'jpeg')


iqr_df <- ensemble_prediction_df %>% 
  group_by(species, cultivar) %>% 
  summarise(iqr = IQR(pheno))


#now I can generate performance data
ensemble_performance <- ensemble_prediction_df %>% 
  merge.data.frame(iqr_df, by = c('species', 'cultivar')) %>% 
  dplyr::group_by(species, cultivar, repetition, iqr) %>% 
  dplyr::summarise(rmse = RMSEP(predicted = pred, observed = pheno),
                   mean_bias = mean(pred - pheno)) %>% 
  mutate(rpiq_adj = iqr / rmse)


ensemble_performance %>% 
  ggplot(aes(y = reorder(cultivar, rmse), x = rmse)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 5, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  scale_y_discrete(limits=rev) +
  ylab('Cultivar') +
  xlab('Root Mean Square Error (Days)') +
  facet_grid(rows = vars(species), scales = 'free_y', space = "free")
ggsave('figures/rmse_ensemble.jpeg', device = 'jpeg',
       height = 40, width = 20, units = 'cm')

ensemble_performance %>% 
  ggplot(aes(y = reorder(cultivar, rpiq_adj), x = rpiq_adj)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  ylab('Cultivar') +
  xlab('Ratio of Performance to Interquartile Distance') +
  facet_grid(rows = vars(species), scales = 'free_y', space = "free")
ggsave('figures/rpiq_ensemble.jpeg', device = 'jpeg',
       height = 40, width = 20, units = 'cm')



#compare estimated model parameters for almonds

par_df <- purrr::map(names(fit_list), function(spec){
  purrr::map(1:10, function(i){
    purrr::map(fit_list[[spec]][[i]], 'xbest') %>% 
      bind_rows() %>% 
      t() %>% 
      data.frame() %>% 
      mutate(round = i)
  }) %>% 
    bind_rows() %>% 
    mutate(species = spec)
}) %>% 
  bind_rows()




par_df$cultivar <- purrr::map_chr(str_split(row.names(par_df), pattern = '\\...'), 1)

rownames(par_df) <- NULL
colnames(par_df)[1:10] <- c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope')


boundary_df <- data.frame(value = c(20,    100,    0.1,    15,     284,       16,      24,     2,     2,     1.2, 80,    500,    1.0,    30,     287,       48,      50,    10,    10,     5.00), 
                          variable = rep(c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope')),2) 

par_df %>% 
  filter(species == 'Almond') %>% 
  reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
  ggplot(aes(x = cultivar, y = value)) +
  geom_boxplot() +
  geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
  facet_wrap(~variable, scales = 'free_y')


par_df %>% 
  filter(species == 'Apricot') %>% 
  reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
  ggplot(aes(x = cultivar, y = value)) +
  geom_boxplot() +
  geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
  facet_wrap(~variable, scales = 'free_y')

par_df %>% 
  filter(species == 'Pear') %>% 
  reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
  ggplot(aes(x = cultivar, y = value)) +
  geom_boxplot() +
  geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
  facet_wrap(~variable, scales = 'free_y')

par_df %>% 
  filter(species == 'Sweet Cherry') %>% 
  reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
  ggplot(aes(x = cultivar, y = value)) +
  geom_boxplot() +
  geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
  facet_wrap(~variable, scales = 'free_y')
  

par_df %>% 
  filter(species == 'European Plum') %>% 
  reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
  ggplot(aes(x = cultivar, y = value)) +
  geom_boxplot() +
  geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
  facet_wrap(~variable, scales = 'free_y')

par_df %>% 
  filter(species == 'Japanese Plum') %>% 
  reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
  ggplot(aes(x = cultivar, y = value)) +
  geom_boxplot() +
  geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
  facet_wrap(~variable, scales = 'free_y')

par_df %>% 
  filter(species == 'Pistachio') %>% 
  reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
  ggplot(aes(x = cultivar, y = value)) +
  geom_boxplot() +
  geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
  facet_wrap(~variable, scales = 'free_y')


#maybe lower yc and higher zc for almond and pistachio


#run again with staring points and adjusted search space

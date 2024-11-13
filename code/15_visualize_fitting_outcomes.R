#visualize validation outcome

library(chillR)
library(tidyverse)
#library(LarsChill)

# source('code/utilities/save_fitting_list.R')
# source('code/utilities/load_fitting_result.R')


#load fitting results

#species <- c('data/fitting/')
#species <- c('almond', 'apricot', 'european_plum', 'japanese_plum',  'pistachio', 'sweet_cherry', 'pear')
species <- c('almond', 'apple', 'apricot', 'european_plum', 'japanese_plum',  'pistachio', 'sweet_cherry')

#number of repititions
r <- 10

apricot_fit <- apple_fit <- eplum_fit <- jplum_fit <- pistachio_fit <- cherry_fit <- almond_fit <- pear_fit <- almond_fit_old <- almond_with_sanotmera <- almond_with_sanotmera_v2 <-  list()

for(i in 1:r){
  apricot_fit[[i]] <- LarsChill::load_fitting_result('data/fitting/apricot/repeated_fitting_clean/', prefix = paste0('repeat', i, '_'))
  eplum_fit[[i]] <- LarsChill::load_fitting_result('data/fitting/european_plum/', prefix = paste0('repeat', i, '_'))
  jplum_fit[[i]] <- LarsChill::load_fitting_result('data/fitting/japanese_plum/', prefix = paste0('repeat', i, '_'))
  pistachio_fit[[i]] <- LarsChill::load_fitting_result('data/fitting/pistachio/', prefix = paste0('repeat', i, '_'))
  cherry_fit[[i]] <- LarsChill::load_fitting_result('data/fitting/sweet_cherry/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  almond_fit_old[[i]]  <- LarsChill::load_fitting_result(path = 'data/fitting/almond/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  almond_fit[[i]] <- LarsChill::load_fitting_result(path = 'data/fitting/almond/repeated_fitting_new_bounds', prefix = paste0('repeat', i, '_'))
  almond_with_sanotmera_v2[[i]] <- LarsChill::load_fitting_result(path = 'data/fitting/almond/repeated_fitting_santomera_v2_cleanly_saved/', prefix = paste0('repeat', i, '_'))
  almond_with_sanotmera[[i]]  <- LarsChill::load_fitting_result(path = 'data/fitting/almond/repeated_fitting_santomera_cleanly_saved/', prefix = paste0('repeat', i, '_'))
  pear_fit[[i]] <- LarsChill::load_fitting_result('data/fitting/pear/', prefix = paste0('repeat', i, '_'))
  apple_fit[[i]] <- LarsChill::load_fitting_result('data/fitting/apple/', prefix = paste0('repeat', i, '_'))
}

  

fit_list <- list('Apricot' = apricot_fit,
                 'Apple' = apple_fit,
                 'European Plum' = eplum_fit,
                 'Japanese Plum' = jplum_fit,
                 'Pistachio' = pistachio_fit,
                 'Sweet Cherry' = cherry_fit,
                 'Almond' = almond_with_sanotmera_v2,
                 'Pear' = pear_fit)


######################################
#read weather data, create season list
######################################

cka <- read.csv('data/weather_ready/cka_clean.csv')
zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv')
sfax <- read.csv('data/weather_ready/sfax_clean.csv')
meknes <- read.csv('data/weather_ready/meknes_clean.csv')
cieza <- read.csv('data/weather_ready/cieza_clean_patched.csv')
santomera <- read.csv('data/weather_ready/murcia_clean.csv')

weather_stations <- read.csv('data/weather_ready/weather_station_phenological_observations.csv')


#make hourly
cka_hourly <- stack_hourly_temps(cka, latitude = weather_stations$latitude[weather_stations$station_name == 'Klein-Altendorf'])
zaragoza_hourly <- stack_hourly_temps(zaragoza, latitude =  weather_stations$latitude[weather_stations$station_name == 'Zaragoza'])
sfax_hourly <- stack_hourly_temps(sfax, latitude =  weather_stations$latitude[weather_stations$station_name == 'Sfax'])
cieza_hourly <- stack_hourly_temps(cieza, latitude =  weather_stations$latitude[weather_stations$station_name == 'Cieza'])
meknes_hourly <- stack_hourly_temps(meknes, latitude =  weather_stations$latitude[weather_stations$station_name == 'Meknes'])
santomera_hourly <- stack_hourly_temps(santomera, latitude =  weather_stations$latitude[weather_stations$station_name == 'Santomera'])


cka_hourly <- cka_hourly$hourtemps
zaragoza_hourly <- zaragoza_hourly$hourtemps
sfax_hourly <- sfax_hourly$hourtemps
cieza_hourly <- cieza_hourly$hourtemps
meknes_hourly <- meknes_hourly$hourtemps
santomera_hourly <- santomera_hourly$hourtemps

#make seasonal list
cka_season <- genSeasonList(cka_hourly, years = (min(cka$Year)+1):max(cka$Year))
zaragoza_season <- genSeasonList(zaragoza_hourly, years = (min(zaragoza$Year)+1):max(zaragoza$Year))
sfax_season <- genSeasonList(sfax_hourly, years = (min(sfax$Year)+1):max(sfax$Year))
cieza_season <- genSeasonList(cieza_hourly, years = (min(cieza$Year)+1):max(cieza$Year))
meknes_season <- genSeasonList(meknes_hourly, years = (min(meknes$Year)+1):max(meknes$Year))
santomera_season <- genSeasonList(santomera_hourly, years = (min(santomera$Year)+1):max(santomera$Year))


names(cka_season) <- (min(cka$Year)+1):max(cka$Year)
names(zaragoza_season) <- (min(zaragoza$Year)+1):max(zaragoza$Year)
names(sfax_season) <- (min(sfax$Year)+1):max(sfax$Year)
names(cieza_season) <-  (min(cieza$Year)+1):max(cieza$Year)
names(meknes_season) <- (min(meknes$Year)+1):max(meknes$Year)
names(santomera_season) <- (min(santomera$Year)+1):max(santomera$Year)

SeasonList <- list('Zaragoza' = zaragoza_season, 
                   'Klein-Altendorf' = cka_season,
                   'Sfax' = sfax_season,
                   'Cieza' = cieza_season,
                   'Meknes' = meknes_season,
                   'Santomera' = santomera_season)


rm(meknes, meknes_season, sfax, sfax_season, cka, cka_season, zaragoza, zaragoza_season, cieza, cieza_season, santomera, santomera_season)


#need to do the splitting again and then safe the results!!!!
#read master pheno file

master_pheno_split <- read.csv('data/master_phenology_repeated_splits.csv')

master_pheno_split <- master_pheno_split %>% 
  filter(!(species == 'Apple' & year == 1958),
         !(species == 'Pear' & year == 1958))

master_almond_split_old <- read.csv('data/master_phenology_repeated_splits_old_almond.csv')

#now I should be able to make forecast for each data point in the master pheno file



Tc = 36
theta_star = 279


sub_SeasonList <- list()
sub <- NULL
prediction_df <- data.frame()


#generate predictions
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
      sub$pred <- LarsChill::return_predicted_days(par = LarsChill::convert_parameters(par), 
                                          modelfn = LarsChill::custom_PhenoFlex_GDHwrapper, 
                                          SeasonList =sub_SeasonList)
      
      prediction_df <- rbind.data.frame(prediction_df,
                                        sub)
      

      
    }
  }
}

prediction_df$residual <- prediction_df$pred - prediction_df$pheno
#maybe different order of data??


write.csv(prediction_df, file = 'data/prediction_vs_observed.csv', row.names = FALSE)

#add iqr to prediction data.frame
iqr_df <- prediction_df %>% 
  group_by(species, cultivar, repetition) %>% 
  summarise(iqr = IQR(pheno))

mean_obs_df <- prediction_df  %>% 
  group_by(species, cultivar, repetition) %>% 
  summarise(mean_pheno = mean(pheno))


#now I can generate performance data
performance <- prediction_df %>% 
  merge.data.frame(iqr_df, by = c('species', 'cultivar', 'repetition')) %>% 
  merge.data.frame(mean_obs_df, by = c('species', 'cultivar', 'repetition')) %>% 
  dplyr::group_by(species, cultivar, repetition, split, iqr, mean_pheno) %>% 
  dplyr::summarise(rmse = RMSEP(predicted = pred, observed = pheno),
            mean_bias = mean(pred - pheno),
            nse = 1 - (sum( pheno - pred)^2 ) / sum( (pheno - mean(mean_pheno))^2 ) ) %>% 
  mutate(rpiq_adj = iqr / rmse)

write.csv(performance, 'data/performance_fitted_models.csv', row.names = FALSE)


performance %>% 
  filter(split == 'Validation') %>% 
  ggplot(aes(y = species, x = nse)) +
  geom_boxplot(aes(fill = species), show.legend = FALSE)+
  scale_y_discrete(limits = rev)+
  ylab('') +
  xlab('Nash-Sutcliff model efficiency coefficient') + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  coord_cartesian(xlim = c(-1, 1))+
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw()



performance %>% 
  filter(split == 'Validation') %>% 
  ggplot(aes(y = species, x = rpiq_adj)) +
  geom_boxplot(aes(fill = species), show.legend = FALSE)+
  scale_y_discrete(limits = rev)+
  ylab('') +
  xlab('Ratio of Performance to Interquartile Distance  (RPIQ) for Validation Data') + 
  geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw()

performance %>% 
  filter(split == 'Validation') %>% 
  group_by(species) %>% 
  summarise(rpiq_low = sum(rpiq_adj < 1) / n())



########
#table for publication
########

cult_drop <- master_pheno_split %>% 
  filter(repetition == 1) %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n()) %>% 
  filter(n < 20)


performance %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species) %>% 
  summarise(iqr_5_rpiq_adj = quantile(rpiq_adj, 0.05),
            median_rpiq = median(rpiq_adj),
            iqr_95_rpiq_adj = quantile(rpiq_adj, 0.95),
            iqr_5_rmse = quantile(rmse, 0.05),
            median_rmse = median(rmse),
            iqr_95_rmse = quantile(rmse, 0.95),
            share_small_rpiq = (sum(rpiq_adj < 1) / n())*100,
            # iqr_5_nse = quantile(nse, 0.05),
            # median_nse = median(nse),
            # iqr_95_nse = quantile(nse, 0.95),
            # share_small_nse = (sum(nse < 0) / n()) * 100,
            iqr_5_bias = quantile(abs(mean_bias), 0.05),
            median_bias = median(abs(mean_bias)),
            iqr_95_bias = quantile(abs(mean_bias), 0.95),
            n = n()
            )


#-------------------------------#


#############################
#comparison within a species
############################


performance %>% 
  filter(species == 'Almond') %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj)) %>% 
  filter(median_rpiq >= 1.5)

performance %>% 
  filter(species == 'Almond') %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj)) %>% 
  filter(median_rpiq < 1.5 & median_rpiq >= 1)

performance %>% 
  filter(species == 'Almond') %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj)) %>% 
  filter(median_rpiq < 1)

performance %>% 
  filter(species == 'Almond') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(species == 'Apple') %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar))


performance %>% 
  filter(species == 'Apple') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj)) %>% 
  filter(median_rpiq < 1)

performance %>% 
  filter(species == 'Apricot') %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj)) %>% 
  filter(median_rpiq < 1.5 & median_rpiq >= 1)

performance %>% 
  filter(species == 'Apricot') %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj)) %>% 
  filter(median_rpiq >= 1.5)

performance %>% 
  filter(species == 'Pear') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj))

performance %>% 
  filter(species == 'Pistachio') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj))

performance %>% 
  filter(species == 'Sweet Cherry') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj)) %>% 
  filter(median_rpiq < 1.5)


performance %>% 
  filter(species == 'Sweet Cherry') %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  pull(cultivar) %>% 
  unique() %>% 
  length()

bad_cult <- performance %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj)) %>% 
  filter(median_rpiq < 1) %>% 
  pull(cultivar)

medium_cult <- performance %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj)) %>% 
  filter(median_rpiq >= 1 & median_rpiq < 1.5) %>% 
  pull(cultivar)

good_cult <- performance %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj)) %>% 
  filter(median_rpiq >= 1.5) %>% 
  pull(cultivar)
  

master_pheno_split %>% 
  filter(repetition == 1) %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(n_loc = length(unique(location))) %>% 
  mutate(cult_type = ifelse(cultivar %in% good_cult, 
                            yes = 'good_cult', 
                            no = ifelse(cultivar %in% medium_cult, 
                                        yes = 'medium_cult', 
                                        no = 'bad_cult'))) %>% 
  group_by(cult_type) %>% 
  summarise(sum(n_loc > 1) / n())

test <- master_pheno_split %>% 
  filter(repetition == 1) %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(n_loc = length(unique(location))) %>% 
  mutate(cult_type = ifelse(cultivar %in% good_cult, 
                            yes = 'good_cult', 
                            no = ifelse(cultivar %in% medium_cult, 
                                        yes = 'medium_cult', 
                                        no = 'bad_cult'))) %>% 
  summarise(sum(n_loc > 1) / n())




##############################################
#figures of rpiq etc on a cultivar level
#############################################


median_df <- performance %>% 
  filter(split == 'Validation') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj),
            median_rmse = median(rmse),
            median_bias = median(mean_bias))

performance %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  ylab('Cultivar (ordered by incrasing RMSE of validation data)') +
  xlab('Root Mean Square Error in Bloom Prediction (Days)') +
  facet_grid(rows = vars(species), scales = 'free_y', space = "free")
ggsave('figures/rmse_all_cult.jpeg', device = 'jpeg',
       height = 40, width = 20, units = 'cm')

performance %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  merge.data.frame(median_df, by = c('species', 'cultivar')) %>% 
  ggplot(aes(y = reorder(cultivar, median_rpiq), fill = split, x = rpiq_adj)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  theme_bw(base_size = 15) +
  ylab('Cultivar (ordered by decrasing RPIQ of validation data)') +
  xlab('Ratio of Performance to Interquartile Distance') +
  facet_grid(rows = vars(species), scales = 'free_y', space = "free")
ggsave('figures/rpiq_all_cult.jpeg', device = 'jpeg',
       height = 40, width = 20, units = 'cm')

performance %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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



#-----------------------------------------------------#



#################################################
#ensemble predictions
################################################

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
    
    
    pred_out <- LarsChill::pheno_ensemble_prediction(par_list, confidence, temp = sub_SeasonList)
    
    
    sub$pred <- pred_out$predicted
    sub$sd <- pred_out$sd
    
    ensemble_prediction_df <- rbind.data.frame(ensemble_prediction_df,
                                               sub)
    
    
  }
  
}

ensemble_prediction_df %>% 
  filter(species == 'Apricot') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(species == 'Apple') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
  ggplot(aes(x = pheno, y = pred)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + 
  geom_errorbar(aes(ymin = pred - sd, ymax = pred + sd)) +
  facet_wrap(~cultivar) +
  ylab('Predicted Bloom (Day of Year)') +
  xlab('Observed Bloom (Day of Year)') +
  theme_bw(base_size = 15) 
ggsave('figures/ensemble_prediction_apple.jpeg',
       height = 15, width = 20, units = 'cm', device = 'jpeg')


ensemble_prediction_df %>% 
  filter(species == 'Almond') %>% 
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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
  filter(!(cultivar %in% cult_drop$cultivar)) %>% 
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

































































almond_fit_old
#generate predictions

for(i in 1:r){
  
  cultivars <- names(almond_fit_old[[i]])
  for(cult in cultivars){
    
    
    
    #extract parameters
    par <- almond_fit_old[[i]][[cult]]$xbest 
    #add fixed parameters
    par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
    
    #subset master file
    sub <- master_almond_split_old %>% 
      filter(cultivar == cult,
             repetition == i) %>% 
      mutate(species = 'almond_old') %>% 
      rename(measurement_type  = flowering_type)
    
    #generate seasonlist
    sub_SeasonList <-  purrr::map2(sub$location, sub$year, function(loc, yr) SeasonList[[loc]][[as.character(yr)]])
    
    #predict bloom days
    sub$pred <- LarsChill::return_predicted_days(par = LarsChill::convert_parameters(par), 
                                                 modelfn = LarsChill::custom_PhenoFlex_GDHwrapper, 
                                                 SeasonList =sub_SeasonList)
    
    sub$residual <- sub$pred - sub$pheno
    
    prediction_df <- rbind.data.frame(prediction_df,
                                      sub)

  }
}

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
  filter(split == 'Validation') %>% 
  ggplot(aes(y = species, x = rpiq_adj)) +
  geom_boxplot(aes(fill = species), show.legend = FALSE)+
  scale_y_discrete(limits = rev)+
  ylab('') +
  xlab('Ratio of Performance to Interquartile Distance  (RPIQ) for Validation Data') + 
  geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw()

prediction_df %>% 
  dplyr::filter(species == 'Almond',
                repetition ==  1) %>% 
  ggplot(aes(x = pheno, y = pred, shape = split, col = location)) +
  geom_point() +
  geom_abline(slope = 1, linetype = 'dashed') +
  facet_wrap(~cultivar)

prediction_df %>% 
  dplyr::filter(species == 'almond_old',repetition == 1) %>% 
  ggplot(aes(x = pheno, y = pred, shape = split, col = location)) +
  geom_point() +
  geom_abline(slope = 1, linetype = 'dashed') +
  facet_wrap(~cultivar)



fbest_df_almond_new <- purrr::map(almond_with_sanotmera, function(x){
  
  purrr::map_dbl(x, 'fbest') %>% 
    unname() %>% 
    data.frame() %>% 
    setNames('fbest') %>% 
    mutate(cultivar = names(x))
}) %>% 
  bind_rows(.id = 'repetition') %>% 
  mutate(species = 'Almond')

fbest_df_almond_old <- purrr::map(almond_fit_old, function(x){
  
  purrr::map_dbl(x, 'fbest') %>% 
    unname() %>% 
    data.frame() %>% 
    setNames('fbest') %>% 
    mutate(cultivar = names(x))
}) %>% 
  bind_rows(.id = 'repetition') %>% 
  mutate(species = 'almond_old')

performance %>% 
  filter(species %in% c('Almond', 'almond_old')) %>% 
  merge(rbind(fbest_df_almond_new,fbest_df_almond_old) , by = c('species', 'cultivar', 'repetition')) %>% 
  #filter(split == 'Validation') %>% 
  ggplot(aes(x = fbest, y = rpiq_adj, col = split)) +
  geom_point() +
  facet_grid(species~split)
#text book example of overfitting


#--> the only cultivars affected should be 
#Achaak
#Desmayo
#Ferragnes
#Malaguena
#Marcona
#Tuono

#everything else should be the same

performance %>% 
  filter(species %in% c('Almond', 'almond_old'), 
         split == 'Validation') %>%
  ggplot(aes(x = rpiq_adj,  y = cultivar, fill = species)) +
  geom_boxplot() +
  scale_y_discrete(limits = rev)+
  ylab('') +
  xlab('Ratio of Performance to Interquartile Distance  (RPIQ) for Validation Data') + 
  geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw()







#can I use the ensemble to make predictions? maybe use the rpiq of validation data as a weight for the prediction?





# #compare estimated model parameters for almonds
# 
# par_df <- purrr::map(names(fit_list), function(spec){
#   purrr::map(1:10, function(i){
#     purrr::map(fit_list[[spec]][[i]], 'xbest') %>% 
#       bind_rows() %>% 
#       t() %>% 
#       data.frame() %>% 
#       mutate(round = i)
#   }) %>% 
#     bind_rows() %>% 
#     mutate(species = spec)
# }) %>% 
#   bind_rows()
# 
# 
# 
# 
# par_df$cultivar <- purrr::map_chr(str_split(row.names(par_df), pattern = '\\...'), 1)
# 
# rownames(par_df) <- NULL
# colnames(par_df)[1:10] <- c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope')
# 
# 
# boundary_df <- data.frame(value = c(20,    100,    0.1,    15,     284,       16,      24,     2,     2,     1.2, 80,    500,    1.0,    30,     287,       48,      50,    10,    10,     5.00), 
#                           variable = rep(c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope')),2) 
# 
# par_df %>% 
#   filter(species == 'Almond') %>% 
#   reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
#   ggplot(aes(x = cultivar, y = value)) +
#   geom_boxplot() +
#   geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
#   facet_wrap(~variable, scales = 'free_y')
# 
# 
# par_df %>% 
#   filter(species == 'Apricot') %>% 
#   reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
#   ggplot(aes(x = cultivar, y = value)) +
#   geom_boxplot() +
#   geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
#   facet_wrap(~variable, scales = 'free_y')
# 
# par_df %>% 
#   filter(species == 'Pear') %>% 
#   reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
#   ggplot(aes(x = cultivar, y = value)) +
#   geom_boxplot() +
#   geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
#   facet_wrap(~variable, scales = 'free_y')
# 
# par_df %>% 
#   filter(species == 'Sweet Cherry') %>% 
#   reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
#   ggplot(aes(x = cultivar, y = value)) +
#   geom_boxplot() +
#   geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
#   facet_wrap(~variable, scales = 'free_y')
#   
# 
# par_df %>% 
#   filter(species == 'European Plum') %>% 
#   reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
#   ggplot(aes(x = cultivar, y = value)) +
#   geom_boxplot() +
#   geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
#   facet_wrap(~variable, scales = 'free_y')
# 
# par_df %>% 
#   filter(species == 'Japanese Plum') %>% 
#   reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
#   ggplot(aes(x = cultivar, y = value)) +
#   geom_boxplot() +
#   geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
#   facet_wrap(~variable, scales = 'free_y')
# 
# par_df %>% 
#   filter(species == 'Pistachio') %>% 
#   reshape2::melt(id.vars = c('round', 'cultivar', 'species')) %>% 
#   ggplot(aes(x = cultivar, y = value)) +
#   geom_boxplot() +
#   geom_hline(data = boundary_df, aes(yintercept = value), linetype = 'dashed') +
#   facet_wrap(~variable, scales = 'free_y')
# 
# 
# #maybe lower yc and higher zc for almond and pistachio
# 
# 
# #run again with staring points and adjusted search space
# 
# 
# 
# 
# 
# almond_fit_list <- list('Almond old' = almond_fit_old,
#                         'Almond new bound' = almond_fit)
# 
# almond_prediction_df <- data.frame()
# for(spec in names(almond_fit_list)){
#   for(i in 1:r){
#     
#     cultivars <- names(almond_fit_list[[spec]][[i]])
#     for(cult in cultivars){
#       
#       
#       
#       #extract parameters
#       par <- almond_fit_list[[spec]][[i]][[cult]]$xbest 
#       #add fixed parameters
#       par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
#       
#       #subset master file
#       sub <- master_pheno_split %>% 
#         filter(species == 'Almond',
#                cultivar == cult,
#                repetition == i)
#       
#       #generate seasonlist
#       sub_SeasonList <-  purrr::map2(sub$location, sub$year, function(loc, yr) SeasonList[[loc]][[as.character(yr)]])
#       
#       #predict bloom days
#       sub$pred <- return_predicted_days(convert_parameters(par), 
#                                         modelfn = custom_PhenoFlex_GDHwrapper, 
#                                         SeasonList =sub_SeasonList)
#       
#       sub$version <- spec
#       
#       almond_prediction_df <- rbind.data.frame(almond_prediction_df,
#                                         sub)
#       
#       
#       
#     }
#   }
# }
# 
# almond_prediction_df$residual <- almond_prediction_df$pred - almond_prediction_df$pheno
# 
# 
# almond_iqr_df <- almond_prediction_df %>% 
#   group_by(version, cultivar, repetition) %>% 
#   summarise(iqr = IQR(pheno))
# 
# 
# #now I can generate performance data
# almond_performance <- almond_prediction_df %>% 
#   merge.data.frame(almond_iqr_df, by = c('version', 'cultivar', 'repetition')) %>% 
#   dplyr::group_by(version, cultivar, repetition, split, iqr) %>% 
#   dplyr::summarise(rmse = RMSEP(predicted = pred, observed = pheno),
#                    mean_bias = mean(pred - pheno)) %>% 
#   mutate(rpiq_adj = iqr / rmse)
# 
# 
# almond_performance %>% 
#   ggplot(aes(y = cultivar, fill = version, x = rpiq_adj)) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   geom_boxplot() +
#   theme_bw() +
#   #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
#   #coord_cartesian(xlim = c(0, 10)) +
#   geom_vline(xintercept = 1, linetype = 'dashed') +
#   theme_bw(base_size = 15) +
#   scale_y_discrete(limits=rev) +
#   facet_grid(~split)
# 
# almond_performance %>% 
#   ggplot(aes(y = cultivar, fill = version, x = rmse)) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   geom_boxplot() +
#   theme_bw() +
#   #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
#   #coord_cartesian(xlim = c(0, 10)) +
#   geom_vline(xintercept = 5, linetype = 'dashed') +
#   theme_bw(base_size = 15) +
#   scale_y_discrete(limits=rev) +
#   facet_grid(~split)
# 
# 'NO IMPROVEMENT AT ALL'



#did overfitting happen in the almond data?

#read almond fit data, make predictions for the intermediate parameters, for calibration and validation+

library(tidyverse)
library(LarsChill)
library(chillR)

#read almond data
almond_fit <-  list()

for (i in 1:10) {
  almond_fit[[i]]  <-
    load_fitting_result(path = 'data/fitting/almond/repeated_fitting_santomera_v2_cleanly_saved/', prefix = paste0('repeat', i, '_'))
  
}


#now create an enormous matrix which contains the paramters of the intermediate steps and the
#final result

test <-
  purrr::map(almond_fit, function(rep)
    purrr::map(rep, function(i)
      rbind(i$x, i$xbest)) %>%
      do.call(rbind, .)) %>%
  do.call(rbind, .)

#have a data.frame which keeps track to which cultivar, repetition. etc this belongs to
overview_df <- data.frame()
for (i in 1:length(almond_fit)) {
  for (j in 1:length(almond_fit[[i]])) {
    overview_df <- rbind(overview_df,
                         data.frame(
                           par_type = c(rep(
                             'intermediate', nrow(almond_fit[[i]][[j]]$x)
                           ), 'final'),
                           step = 1:(nrow(almond_fit[[i]][[j]]$x) +
                                       1),
                           cultivar = names(almond_fit[[i]])[j],
                           eval = c(almond_fit[[i]][[j]]$neval[1:nrow(almond_fit[[i]][[j]]$x)], almond_fit[[i]][[j]]$numeval), 
                           repeated = i
                         ))
  }
}


#make predicitons for the season list
######################################
#read weather data, create season list
######################################

sfax <- read.csv('data/weather_ready/sfax_clean.csv')
meknes <- read.csv('data/weather_ready/meknes_clean.csv')
santomera <- read.csv('data/weather_ready/murcia_clean.csv')

weather_stations <-
  read.csv('data/weather_ready/weather_station_phenological_observations.csv')


#make hourly
sfax_hourly <-
  stack_hourly_temps(sfax, latitude =  weather_stations$latitude[weather_stations$station_name == 'Sfax'])
meknes_hourly <-
  stack_hourly_temps(meknes, latitude =  weather_stations$latitude[weather_stations$station_name == 'Meknes'])
santomera_hourly <-
  stack_hourly_temps(santomera, latitude =  weather_stations$latitude[weather_stations$station_name == 'Santomera'])


sfax_hourly <- sfax_hourly$hourtemps
meknes_hourly <- meknes_hourly$hourtemps
santomera_hourly <- santomera_hourly$hourtemps

#make seasonal list
sfax_season <-
  genSeasonList(sfax_hourly, years = (min(sfax$Year) + 1):max(sfax$Year))
meknes_season <-
  genSeasonList(meknes_hourly, years = (min(meknes$Year) + 1):max(meknes$Year))
santomera_season <-
  genSeasonList(santomera_hourly, years = (min(santomera$Year) + 1):max(santomera$Year))


names(sfax_season) <- (min(sfax$Year) + 1):max(sfax$Year)
names(meknes_season) <- (min(meknes$Year) + 1):max(meknes$Year)
names(santomera_season) <-
  (min(santomera$Year) + 1):max(santomera$Year)

SeasonList <- list('Sfax' = sfax_season,
                   'Meknes' = meknes_season,
                   'Santomera' = santomera_season)


rm(
  meknes,
  meknes_season,
  sfax,
  sfax_season,
  cka,
  cka_season,
  zaragoza,
  zaragoza_season,
  cieza,
  cieza_season,
  santomera,
  santomera_season
)


#need to do the splitting again and then safe the results!!!!
#read master pheno file

master_pheno_split <-
  read.csv('data/master_phenology_repeated_splits.csv')

master_pheno_split <- master_pheno_split %>%
  filter(!(species == 'Apple' & year == 1958),!(species == 'Pear' &
                                                  year == 1958))

#now I should be able to make forecast for each data point in the master pheno file

Tc = 36
theta_star = 279


sub_SeasonList <- list()
sub <- NULL
prediction_df <- data.frame()

pb <-
  txtProgressBar(
    min = 0,
    # Minimum value of the progress bar
    max = nrow(test),
    # Maximum value of the progress bar
    style = 3,
    # Progress bar style (also available style = 1 and style = 2)
    width = 50,
    # Progress bar width. Defaults to getOption("width")
    char = "="
  )   # Character used to create the bar


#generate predictions
#go over the entries in the matrix and make the predictions
for (i in 1:nrow(test)) {
  # i<- 1
  cult <- overview_df$cultivar[i]
  r <- overview_df$repeated[i]
  
  par <- test[i, ]
  #add fixed parameters
  par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
  
  #subset master file
  sub <- master_pheno_split %>%
    filter(species == 'Almond',
           cultivar == cult,
           repetition == r)
  
  #generate seasonlist
  sub_SeasonList <-
    purrr::map2(sub$location, sub$year, function(loc, yr)
      SeasonList[[loc]][[as.character(yr)]])
  
  tmp <- try(LarsChill::convert_parameters(par))
  
  if (inherits(tmp, "try-error")) {
    sub$pred <- NA
  } else {
    #predict bloom days
    sub$pred <-
      LarsChill::return_predicted_days(
        par = tmp,
        modelfn = LarsChill::custom_PhenoFlex_GDHwrapper,
        SeasonList = sub_SeasonList
      )
  }
  

  
  sub$step <- overview_df$step[i]
  sub$par_type <- overview_df$par_type[i]
  
  prediction_df <- rbind.data.frame(prediction_df,
                                    sub)
  
  setTxtProgressBar(pb, i)
  
}
close(pb)


#add number of evaluations to

#add the observations, then calculate the rmse, rpiq, how it changes with ongoing fitting

iqr_df <- prediction_df %>% 
  filter(repetition == 1, step == 1) %>% 
  group_by(cultivar) %>% 
  summarise(iqr = IQR(pheno))
  
performance_df <- prediction_df %>% 
  na.omit() %>% 
  group_by(cultivar, repetition, step, split) %>% 
  summarise(rmse = RMSEP(pred, pheno)) %>% 
  merge(iqr_df, by = c('cultivar')) %>% 
  mutate(rpiq_adj = iqr / rmse) %>% 
  merge(overview_df, by.x = c('cultivar', 'repetition', 'step'), by.y = c('cultivar', 'repeated', 'step'))

performance_df %>% 
  filter(cultivar == 'Montrone') %>% 
  ggplot(aes(x = eval, y = rmse, col = split)) +
  geom_point()


cultivar <- names(almond_fit[[1]])
plot_list <- list()
for(cult in cultivar){
  plot_list[[cult]] <- performance_df %>% 
    filter(cultivar == cult) %>% 
    mutate(eval_group = cut(eval, breaks = seq(0, 52000, by = 2000))) %>% 
    group_by(eval_group, split) %>% 
    summarise(mean_rmse = mean(rmse), 
              sd_rmse = sd(rmse),
              mean_rpiq = mean(rpiq_adj),
              sd_rpiq = sd(rpiq_adj)) %>% 
    ggplot(aes(x = eval_group, y = mean_rmse, col = split)) +
    geom_point() +
    geom_line(aes(group = split)) +
    geom_errorbar(aes(ymin = mean_rmse - sd_rmse, ymax = mean_rmse + sd_rmse)) +
    ggtitle(cult) +
    theme_bw()
}

window_width <- 200
cult <- cultivar[35]

performance_df %>% 
  na.omit() %>% 
  filter(cultivar == cult) %>% 
  group_by(split, eval) %>% 
  summarise(mean_rmse = mean(rmse), 
            mean_rpiq = mean(rpiq_adj)) %>% 
  ungroup() %>% 
  group_by(split) %>% 
  mutate(run_mean_rmse = chillR::runn_mean(mean_rmse, runn_mean = window_width), 
         run_mean_rpiq = chillR::runn_mean(mean_rpiq, runn_mean = window_width),
         run_sd_rmse = chillR::runn_mean(mean_rmse, runn_mean = window_width, FUN = sd),
         run_sd_rpiq = chillR::runn_mean(mean_rpiq, runn_mean = window_width, FUN = sd)) %>% 
  ungroup() %>% 
  ggplot(aes(x = eval, y = run_mean_rmse, col = split, fill = split)) +
  geom_ribbon(aes(ymin = run_mean_rmse - run_sd_rmse, ymax = run_mean_rmse + run_sd_rmse), alpha = 0.4) +
  geom_line(aes(group = split), lwd = 2) +
  ggtitle(cult) +
  theme_bw()



#make running mean over the mean rmse per evaluation per cultivar





plot_list[[36]]


50000 / 1500
50000 / 2000

library(LarsChill)
library(tidyverse)


#devtools::install_github('https://github.com/larscaspersen/addition_chillR')
#this script is needed to save / run fitted phenoflex models
source('code/utilities/load_save_fitting_results.R')

#number of repititions
r <- 10

apricot_fit <- apricot_fit2 <- eplum_fit <- jplum_fit <- pistachio_fit <- cherry_fit <- almond_fit <- pear_fit <-  list()



#I write the saving fitting function without the idea of the ensemble,
#so I have to put a loop around it to read the individual runs

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

#add cultivars which I had not to refit because I messed up something in the apricot data
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





#how to make predictions with an individual model fit and with ensembles
#read weather data, make season list

cka <- read.csv('data/weather_ready/cka_clean.csv')
zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv')
sfax <- read.csv('data/weather_ready/sfax_clean.csv')
meknes <- read.csv('data/weather_ready/meknes_clean.csv')
cieza <- read.csv('data/weather_ready/cieza_clean.csv')

#contains latitude and longitude (very raw, just copy pasted from google)
weather_stations <- read.csv('data/weather_ready/weather_station_phenological_observations.csv')


#make hourly weather data
cka_hourly <- stack_hourly_temps(cka, latitude = weather_stations$latitude[weather_stations$station_name == 'Klein-Altendorf'])
zaragoza_hourly <- stack_hourly_temps(zaragoza, latitude =  weather_stations$latitude[weather_stations$station_name == 'Zaragoza'])
sfax_hourly <- stack_hourly_temps(sfax, latitude =  weather_stations$latitude[weather_stations$station_name == 'Sfax'])
cieza_hourly <- stack_hourly_temps(cieza, latitude =  weather_stations$latitude[weather_stations$station_name == 'Cieza'])
meknes_hourly <- stack_hourly_temps(meknes, latitude =  weather_stations$latitude[weather_stations$station_name == 'Meknes'])

#just take the weather data, ignore quality check (QC)
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

#set names equal to the years
names(cka_season) <- min(cka$Year):max(cka$Year)
names(zaragoza_season) <- min(zaragoza$Year):max(zaragoza$Year)
names(sfax_season) <- min(sfax$Year):max(sfax$Year)
names(cieza_season) <-  min(cieza$Year):max(cieza$Year)
names(meknes_season) <- min(meknes$Year):max(meknes$Year)

#bind everything to big list
SeasonList <- list('Zaragoza' = zaragoza_season, 
                   'Klein-Altendorf' = cka_season,
                   'Sfax' = sfax_season,
                   'Cieza' = cieza_season,
                   'Meknes' = meknes_season)

#remove not need intermediate objects
rm(meknes, meknes_season, sfax, sfax_season, cka, cka_season, zaragoza, zaragoza_season, cieza, cieza_season)




#these parameters have been kept constant
Tc <- 36
theta_star <- 279

source('code/utilities/ensemble_prediction.R')

#for the ensemble predictions you need a list with the fitted models as elements
#also you need a list of weather data
#most importantly you need values for the model weights (I called them confidence for now)
#--> I haven't really settled on a default way to define the weights, for now I will use the model performance value, which is also stored in the fitted model, but something like evaluation performance would be better

#this list is currently not well organized, because it follows the certain structure:
#level 1: species, level 2: run, level 3: cultivar
#would be better to have it in the structure: species, cultivar, run

#create object in whihc the reorganized models should be stored
fit_list_organized <- list()
#iterate over the species
for(spec in names(fit_list)){
  fit_list_organized[[spec]] <- list()
  
  #iterate over the cultivars of that species
  for(cult in names(fit_list[[spec]][[1]])){
    
    #extract the cultivar models and save them to the new object
    fit_list_organized[[spec]][[cult]] <- purrr::map(fit_list[[spec]], function(x) x[[cult]])
  }
  
}
#now it has the correct structure, easier to work with, should in future ajust the function reading the models




pred_zaragoza <- pheno_ensemble_prediction(fit_list_organized$Apricot$Búlida, 
                          confidence = purrr::map_dbl(fit_list_organized$Apricot$Búlida, 'fbest'),
                          temp = SeasonList$Zaragoza, 
                          theta_star = theta_star, Tc = Tc)
str(pred_zaragoza)
#output contains:
#1) predicted: weighted ensemble prediction
#2) sd: unweighted standard deviation of the individual ensemble predictions
#3) individual_pred: matrix with the individual predictions of the ensemble members, rows for each season, columns for the ensemble members



#making individual predictions with a fitted model is also quite easy
#but for that I need to bring the parameters back to the original length of 12 (so I have to include the Tc and theta_star value)

#you need to give the parameter values as a vector, 
#the model we use (this is a remnant when I started to learn how the original phenoflex was coded)
#and the season list for an individual station
par <- fit_list_organized$Apricot$Búlida[[1]]$xbest
par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])

LarsChill::return_predicted_days(convert_parameters(par), 
                      modelfn = custom_PhenoFlex_GDHwrapper, 
                      SeasonList = SeasonList$Zaragoza)


adamedor

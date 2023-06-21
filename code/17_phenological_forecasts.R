library(tidyverse)
library(ggplot2)
library(readr)
library(chillR)
library(devtools)
# install_github("larscaspersen/addition_chillR")
library(LarsChill)

### Read all the cultivars located in each single location

## Raw data
stations<-read.csv("data/combined_phenological_data_adamedor_clean.csv", fileEncoding="ISO-8859-1" ) %>% 
  as.data.frame() %>% 
  count(.,species, cultivar)%>%
  ungroup()
## Data filtered by Lars
filtered<-read.csv("data/overiew_pheno.csv", fileEncoding="ISO-8859-1" ) %>% 
  as.data.frame() 

#Function to make ensemble predictions with the
source('code/utilities/pheno_ensemble_prediction.R')
#Function to load parameters for apricot 
source('code/utilities/load_save_fitting_results.R')


## Find the combinations of models, cities and years we have in our database
files<-list.files("./data/Future-sim-weather")

List_models<-data.frame()
for(a in files){
  First_split<-str_split(a, pattern = '_')%>%unlist(.)
  Second_split<-str_split_fixed(First_split[3], pattern = "\\.", 4)
  Newrow<-data.frame(Code=First_split[1],
                     Location=First_split[2],
                     Scenario=Second_split[1],
                     Model=Second_split[2],
                     Year=Second_split[3])
  List_models<-rbind.data.frame(List_models, Newrow)
}

### I couldnt find the sampling locations in the weather_stations_fernandez file
### so I manually included them

Lat<-data.frame(
  Locations=c("Meknes","Sfax","Zaragoza"),
  Lat=c(33.870, 34.739, 41.666)
)

#These two parameters are fixed for all the models
Tc = 36
theta_star = 279

#########################################################
###### Apricot
########################################################

## Load all the fitting results from the 10 repetitions
fit_list <- list()
for(i in 1:10){
  fit_list[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}

## Extract the names of the cultivars selected by Lars
cultivars <- names(fit_list[[1]])

Results_apricot<-data.frame()
for (a in cultivars){
  #Filter the locations in which a certain cultivar is present
  Locations<-filter(filtered, species=="Apricot" & cultivar==a)%>%
    pull(4)%>%
    str_split(., pattern=", ")%>%
    unlist()
  # Control for missing locations (names that dont match)
  if(length(Locations)<1){print(paste("Not matching name for:", a, "!!!!!!"))
    next}
  # Loop to determine the scenarios included for that sampling location
  for(b in Locations){
    Scenarios<-filter(List_models, Location==b)
    if(nrow(Scenarios)==0){print(paste("No temperature data for:", a, b))
      next}
    SSP<-unique(Scenarios$Scenario)
    # Loop to determine the models applied to this scenario
    for(c in SSP){
      Models<-filter(Scenarios, Scenario==c)
      
      Result<-purrr::map(Models$Model, function(d){
        Code_2050<-filter(Models, Model==d & Year==2050)
        Code_2085<-filter(Models, Model==d & Year==2085)
        
        File_code_2050<-paste0(Code_2050$Code,
                               "_", 
                               Code_2050$Location)
        
        File_code_2085<-paste0(Code_2085$Code,
                               "_", 
                               Code_2085$Location)
        
        Meteo_2050<-read.csv(paste0("./data/Future-sim-weather/", 
                                    files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]), 
                             fileEncoding = "ISO-8859-1")
        Meteo_2085<-read.csv(paste0("./data/Future-sim-weather/", 
                                    files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]), 
                             fileEncoding = "ISO-8859-1")
        Latitude<-Lat%>%
          dplyr::filter(Locations==b)%>%
          pull(Lat)
        Meteo_2050<-stack_hourly_temps(Meteo_2050, latitude = Latitude )
        Meteo_2085<-stack_hourly_temps(Meteo_2085, latitude = Latitude )
        Meteo_2050<-genSeasonList(Meteo_2050$hourtemps, years=c(2000:2100))
        Meteo_2085<-genSeasonList(Meteo_2085$hourtemps, years=c(2000:2100))
        
        par_list<-purrr::map(fit_list, a)
        #use fvlaue instead
        confidence <- purrr::map(fit_list, a) %>% 
          purrr::map_dbl('fbest')
        confidence <- 1/confidence
        
        out_obs_2050 <- pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2050)
        
        out_df_2050 <- data.frame(cultivar = a,
                                  Location=b,
                                  Scenario=c,
                                  Model=d,
                                  Year=2050,
                                  predicted = out_obs_2050$predicted,
                                  sd = out_obs_2050$sd)
        
        out_obs_2085 <- pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2085)
        
        out_df_2085 <- data.frame(cultivar = a,
                                  Location=b,
                                  Scenario=c,
                                  Model=d,
                                  Year=2085,
                                  predicted = out_obs_2085$predicted,
                                  sd = out_obs_2085$sd)
        Results<-rbind.data.frame(out_df_2050, out_df_2085)
        return(Results)
      }, .progress = TRUE)%>% 
        bind_rows()
      Results_apricot<-rbind(Results_apricot, Result)
      print(paste("Completed:", a, b, c))
    }
  }
}

save(Results_apricot, file="data/Results_apricot_future.RData")


# load("data/Results_apricot_future.RData")


# ggplot(Results_apricot, aes(x=cultivar, y=predicted, fill=Scenario))+
#   geom_boxplot()+
#   facet_grid(rows = vars(Year), cols = vars(Location), scales = "free_y")

############################################################
### Almond
#########################################################

## Load all the fitting results from the 10 repetitions
fit_list <- list()
for(i in 1:10){
  fit_list[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}

cultivars <- names(fit_list[[1]])

Results_almond<-data.frame()
for (a in cultivars){
  #Filter the locations in which a certain cultivar is present
  Locations<-filter(filtered, species=="Almond" & cultivar==a)%>%
    pull(4)%>%
    str_split(., pattern=", ")%>%
    unlist()
  # Control for missing locations (names that dont match)
  if(length(Locations)<1){print(paste("Not matching name for:", a, "!!!!!!"))
    next}
  # Loop to determine the scenarios included for that sampling location
  for(b in Locations){
    Scenarios<-filter(List_models, Location==b)
    if(nrow(Scenarios)==0){print(paste("No temperature data for:", a, b))
      next}
    SSP<-unique(Scenarios$Scenario)
    # Loop to determine the models applied to this scenario
    for(c in SSP){
      Models<-filter(Scenarios, Scenario==c)
      
      Result<-purrr::map(Models$Model, function(d){
        Code_2050<-filter(Models, Model==d & Year==2050)
        Code_2085<-filter(Models, Model==d & Year==2085)
        
        File_code_2050<-paste0(Code_2050$Code,
                               "_", 
                               Code_2050$Location)
        
        File_code_2085<-paste0(Code_2085$Code,
                               "_", 
                               Code_2085$Location)
        
        Meteo_2050<-read.csv(paste0("./data/Future-sim-weather/", 
                                    files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]), 
                             fileEncoding = "ISO-8859-1")
        Meteo_2085<-read.csv(paste0("./data/Future-sim-weather/", 
                                    files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]), 
                             fileEncoding = "ISO-8859-1")
        Latitude<-Lat%>%
          dplyr::filter(Locations==b)%>%
          pull(Lat)
        Meteo_2050<-stack_hourly_temps(Meteo_2050, latitude = Latitude )
        Meteo_2085<-stack_hourly_temps(Meteo_2085, latitude = Latitude )
        Meteo_2050<-genSeasonList(Meteo_2050$hourtemps, years=c(2000:2100))
        Meteo_2085<-genSeasonList(Meteo_2085$hourtemps, years=c(2000:2100))
        
        par_list<-purrr::map(fit_list, a)
        #use fvlaue instead
        confidence <- purrr::map(fit_list, a) %>% 
          purrr::map_dbl('fbest')
        confidence <- 1/confidence
        
        out_obs_2050 <- pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2050)
        
        out_df_2050 <- data.frame(cultivar = a,
                                  Location=b,
                                  Scenario=c,
                                  Model=d,
                                  Year=2050,
                                  predicted = out_obs_2050$predicted,
                                  sd = out_obs_2050$sd)
        
        out_obs_2085 <- pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2085)
        
        out_df_2085 <- data.frame(cultivar = a,
                                  Location=b,
                                  Scenario=c,
                                  Model=d,
                                  Year=2085,
                                  predicted = out_obs_2085$predicted,
                                  sd = out_obs_2085$sd)
        Results<-rbind.data.frame(out_df_2050, out_df_2085)
        return(Results)
      }, .progress = TRUE)%>% 
        bind_rows()
      Results_almond<-rbind(Results_almond, Result)
      print(paste("Completed:", a, b, c))
    }
  }
}

save(Results_almond, file="data/Results_almond_future.RData")


############################################################
### Sweet cherry
#########################################################

## Load all the fitting results from the 10 repetitions
fit_list <- list()
for(i in 1:10){
  fit_list[[i]] <- load_fitting_result('data/fitting/sweet_cherry/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}

cultivars <- names(fit_list[[1]])

Results_sweet_cherry<-data.frame()
for (a in cultivars){
  #Filter the locations in which a certain cultivar is present
  Locations<-filter(filtered, species=="Sweet Cherry" & cultivar==a)%>%
    pull(4)%>%
    str_split(., pattern=", ")%>%
    unlist()
  # Control for missing locations (names that dont match)
  if(length(Locations)<1){print(paste("Not matching name for:", a, "!!!!!!"))
    next}
  # Loop to determine the scenarios included for that sampling location
  for(b in Locations){
    Scenarios<-filter(List_models, Location==b)
    if(nrow(Scenarios)==0){print(paste("No temperature data for:", a, b))
      next}
    SSP<-unique(Scenarios$Scenario)
    # Loop to determine the models applied to this scenario
    for(c in SSP){
      Models<-filter(Scenarios, Scenario==c)
      
      Result<-purrr::map(Models$Model, function(d){
        Code_2050<-filter(Models, Model==d & Year==2050)
        Code_2085<-filter(Models, Model==d & Year==2085)
        
        File_code_2050<-paste0(Code_2050$Code,
                               "_", 
                               Code_2050$Location)
        
        File_code_2085<-paste0(Code_2085$Code,
                               "_", 
                               Code_2085$Location)
        
        Meteo_2050<-read.csv(paste0("./data/Future-sim-weather/", 
                                    files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]), 
                             fileEncoding = "ISO-8859-1")
        Meteo_2085<-read.csv(paste0("./data/Future-sim-weather/", 
                                    files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]), 
                             fileEncoding = "ISO-8859-1")
        Latitude<-Lat%>%
          dplyr::filter(Locations==b)%>%
          pull(Lat)
        Meteo_2050<-stack_hourly_temps(Meteo_2050, latitude = Latitude )
        Meteo_2085<-stack_hourly_temps(Meteo_2085, latitude = Latitude )
        Meteo_2050<-genSeasonList(Meteo_2050$hourtemps, years=c(2000:2100))
        Meteo_2085<-genSeasonList(Meteo_2085$hourtemps, years=c(2000:2100))
        
        par_list<-purrr::map(fit_list, a)
        #use fvlaue instead
        confidence <- purrr::map(fit_list, a) %>% 
          purrr::map_dbl('fbest')
        confidence <- 1/confidence
        
        out_obs_2050 <- pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2050)
        
        out_df_2050 <- data.frame(cultivar = a,
                                  Location=b,
                                  Scenario=c,
                                  Model=d,
                                  Year=2050,
                                  predicted = out_obs_2050$predicted,
                                  sd = out_obs_2050$sd)
        
        out_obs_2085 <- pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2085)
        
        out_df_2085 <- data.frame(cultivar = a,
                                  Location=b,
                                  Scenario=c,
                                  Model=d,
                                  Year=2085,
                                  predicted = out_obs_2085$predicted,
                                  sd = out_obs_2085$sd)
        Results<-rbind.data.frame(out_df_2050, out_df_2085)
        return(Results)
      }, .progress = TRUE)%>% 
        bind_rows()
      Results_sweet_cherry<-rbind(Results_sweet_cherry, Result)
      print(paste("Completed:", a, b, c))
    }
  }
}

save(Results_sweet_cherry, file="data/Results_sweet_cherry_future.RData")



############################################################
### European plum // NO DATA
#########################################################

## Load all the fitting results from the 10 repetitions
fit_list <- list()
for(i in 1:10){
  fit_list[[i]] <- load_fitting_result('data/fitting/european_plum/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}

cultivars <- names(fit_list[[1]])

Results_european_plum<-data.frame()
for (a in cultivars){
  #Filter the locations in which a certain cultivar is present
  Locations<-filter(filtered, species=="European Plum" & cultivar==a)%>%
    pull(4)%>%
    str_split(., pattern=", ")%>%
    unlist()
  # Control for missing locations (names that dont match)
  if(length(Locations)<1){print(paste("Not matching name for:", a, "!!!!!!"))
    next}
  # Loop to determine the scenarios included for that sampling location
  for(b in Locations){
    Scenarios<-filter(List_models, Location==b)
    if(nrow(Scenarios)==0){print(paste("No temperature data for:", a, b))
      next}
    SSP<-unique(Scenarios$Scenario)
    # Loop to determine the models applied to this scenario
    for(c in SSP){
      Models<-filter(Scenarios, Scenario==c)
      
      Result<-purrr::map(Models$Model, function(d){
        Code_2050<-filter(Models, Model==d & Year==2050)
        Code_2085<-filter(Models, Model==d & Year==2085)
        
        File_code_2050<-paste0(Code_2050$Code,
                               "_", 
                               Code_2050$Location)
        
        File_code_2085<-paste0(Code_2085$Code,
                               "_", 
                               Code_2085$Location)
        
        Meteo_2050<-read.csv(paste0("./data/Future-sim-weather/", 
                                    files[which(substr(files, 1, nchar(File_code_2050)) == File_code_2050)]), 
                             fileEncoding = "ISO-8859-1")
        Meteo_2085<-read.csv(paste0("./data/Future-sim-weather/", 
                                    files[which(substr(files, 1, nchar(File_code_2085)) == File_code_2085)]), 
                             fileEncoding = "ISO-8859-1")
        Latitude<-Lat%>%
          dplyr::filter(Locations==b)%>%
          pull(Lat)
        Meteo_2050<-stack_hourly_temps(Meteo_2050, latitude = Latitude )
        Meteo_2085<-stack_hourly_temps(Meteo_2085, latitude = Latitude )
        Meteo_2050<-genSeasonList(Meteo_2050$hourtemps, years=c(2000:2100))
        Meteo_2085<-genSeasonList(Meteo_2085$hourtemps, years=c(2000:2100))
        
        par_list<-purrr::map(fit_list, a)
        #use fvlaue instead
        confidence <- purrr::map(fit_list, a) %>% 
          purrr::map_dbl('fbest')
        confidence <- 1/confidence
        
        out_obs_2050 <- pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2050)
        
        out_df_2050 <- data.frame(cultivar = a,
                                  Location=b,
                                  Scenario=c,
                                  Model=d,
                                  Year=2050,
                                  predicted = out_obs_2050$predicted,
                                  sd = out_obs_2050$sd)
        
        out_obs_2085 <- pheno_ensemble_prediction(par_list, confidence, temp = Meteo_2085)
        
        out_df_2085 <- data.frame(cultivar = a,
                                  Location=b,
                                  Scenario=c,
                                  Model=d,
                                  Year=2085,
                                  predicted = out_obs_2085$predicted,
                                  sd = out_obs_2085$sd)
        Results<-rbind.data.frame(out_df_2050, out_df_2085)
        return(Results)
      }, .progress = TRUE)%>% 
        bind_rows()
      Results_european_plum<-rbind(Results_european_plum, Result)
      print(paste("Completed:", a, b, c))
    }
  }
}

save(Results_european_plum, file="data/Results_european_plum_future.RData")



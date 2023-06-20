library(tidyverse)
library(ggplot2)
library(readr)
library(chillR)
library(devtools)
# install_github("larscaspersen/addition_chillR")
library(LarsChill)

### Read all the cultivars located in each single location

## Raw data
stations<-read.csv("data/combined_phenological_data_adamedor_clean.csv", fileEncoding="ISO-8859-1" ) %>% as.data.frame() %>% 
  count(.,species, cultivar)%>%ungroup()
## Filtered by Lars
filtered<-read.csv("data/overiew_pheno.csv", fileEncoding="ISO-8859-1" ) %>% as.data.frame() 

#Function to make ensemble predictions

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

#load parameters for apricot 
source('code/utilities/load_save_fitting_results.R')

## Load all the fitting results from the 10 repetitions
fit_list <- list()
for(i in 1:10){
  fit_list[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}

#These two parameters are fixed for all the models
Tc = 36
theta_star = 279
## Extract the names of the cultivars selected by Lars
cultivars <- names(fit_list[[1]])


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

Lat<-data.frame(
  Locations=c("Meknes","Sfax","Zaragoza"),
  Lat=c(33.870, 34.739, 41.666)
)


Results_apricot<-data.frame()
for (a in cultivars){
  #Filter the locations in which a certain cultivar is present
  Locations<-filtered[which(filtered$species=="Apricot" & filtered$cultivar==a),4]%>%str_split(., pattern=", ")%>%unlist()
  if(length(Locations)<1){print(paste("Not matching name for", a, "!!!!!!"))
    next}
  # Loop to determine the scenarios included for that sampling location
  for(b in Locations){
    Scenarios<-List_models[which(List_models$Location==b),]
    if(nrow(Scenarios)==0){print(paste("No temperature data for", b))
      next}
    SSP<-unique(Scenarios$Scenario)
    # Loop to determine the models applied to this scenario
    for(c in SSP){
      Models<-Scenarios[which(Scenarios$Scenario==c),]
      for(d in unique(Models$Model)){
        ## For each model, I have to calculate bloom dates for 2050 and 2085
        File_code_2050<-paste0(Models[which(Models$Model==d & Models$Year==2050),"Code"],"_", Models[which(Models$Model==d & Models$Year==2050),"Location"])
        File_code_2085<-paste0(Models[which(Models$Model==d & Models$Year==2085),"Code"],"_", Models[which(Models$Model==d & Models$Year==2085),"Location"])
        Meteo_2050<-read.csv(paste0("./data/Future-sim-weather/", files[which(substr(files, 1, nchar(File_code_2050))==File_code_2050)]), fileEncoding = "ISO-8859-1")
        Meteo_2085<-read.csv(paste0("./data/Future-sim-weather/", files[which(substr(files, 1, nchar(File_code_2085))==File_code_2085)]), fileEncoding = "ISO-8859-1")
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
        Results_apricot<-rbind.data.frame(Results_apricot, out_df_2050, out_df_2085)
        print(d)
      }
      print(c)
    }
    print(b)
  }
  print(a)
}


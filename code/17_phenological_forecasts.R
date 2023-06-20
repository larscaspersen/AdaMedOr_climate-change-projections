library(tidyverse)
library(ggplot2)
library(readr)
library(chillR)
library(devtools)
# install_github("larscaspersen/addition_chillR")
library(LarsChill)


stations<-read.csv("data/combined_phenological_data_adamedor_clean.csv", fileEncoding="ISO-8859-1" ) %>% as.data.frame() %>% 
  count(.,species, cultivar)%>%ungroup()

filtered<-read.csv("data/overiew_pheno.csv", fileEncoding="ISO-8859-1" ) %>% as.data.frame() 



#make ensemble prediction 
ensemble_prediction_df <- data.frame()

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

fit_list <- list()
for(i in 1:10){
  fit_list[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}

Tc = 36
theta_star = 279

cultivars <- names(fit_list[[1]])
cult <- cultivars[1]

files<-list.files("./data/Future-sim-weather")

for(a in files){
  First_split<-str_split(a, pattern = '_')%>%unlist(.)
  Second_split<-str_split_fixed(First_split[3], pattern = ".", 4)
}



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

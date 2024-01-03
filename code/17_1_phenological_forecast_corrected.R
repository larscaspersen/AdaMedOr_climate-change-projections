library(tidyverse)
library(ggplot2)
#library(readr)
library(chillR)
#library(devtools)
# install_github("larscaspersen/addition_chillR")
library(LarsChill)

### Read all the cultivars located in each single location

#setwd('../fruittree_portfolio/')

## Raw data
stations <-
  read.csv("data/weather_ready/weather_station_phenological_observations.csv") 
## Data filtered by Lars


#These two parameters are fixed for all the models
Tc = 36
theta_star = 279

#load performance file for weightening of ensemble predictions
performance_df <- read.csv('data/performance_fitted_models.csv')

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
  almond_fit[[i]]  <- load_fitting_result(path = 'data/fitting/almond/repeated_fitting_santomera_v2_cleanly_saved/', prefix = paste0('repeat', i, '_'))
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



#-------------------------------#
# FUTURE WEATHER ####
#-------------------------------#



#maybe read at first all weather data
#future_weather_list <- chillR::load_temperature_scenarios('data/future_weather-v2/', prefix = '')



ensemble_prediction_with_failure <- function(par_list, confidence, temp, theta_star = 279, Tc = 36, return_se = TRUE, n_fail = 5){
  
  
  predicted <- purrr::map(par_list, function(x){
    par <- x$xbest
    par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
    
    return_predicted_days(convert_parameters(par), 
                          modelfn = custom_PhenoFlex_GDHwrapper, 
                          SeasonList =temp,
                          na_penalty = NA)
  }) %>% 
    stats::setNames(1:length(par_list)) %>% 
    dplyr::bind_cols() %>% 
    as.matrix()
  
  #if as many as n_fail model runs indicate failure, then mark as NA, otherwise discard them and calculate mean of the remaining ones
  pred_na <- apply(predicted, MARGIN = 1, FUN = function(x) sum(is.na(x)) >= n_fail) 
  
  
  #create a weight data.frame with the same dimensions as predicted
  weights <- apply(predicted, MARGIN = 1, function(x){
    #presence of NA
    if(sum(is.na(x)) == 0){
      return(confidence / sum(confidence))
    }
    #position of NA
    pos.na <- which(is.na(x))
    
    #create intermediate object which I can manipulate
    conf <- confidence
    
    #calculate replace the confidence at position of NA with zero
    conf[pos.na] <- 0
    
    return(conf / sum(conf))
  }) %>% 
    t()
  
  #calculate sd, excluding NA values
  sd_pred <- predicted %>% 
    t() %>% 
    as.data.frame() %>% 
    magrittr::set_colnames( 1:nrow(predicted)) %>% 
    reshape2::melt(id.vars = NULL) %>% 
    dplyr::group_by(.data$variable) %>% 
    dplyr::summarise(sd = stats::sd(.data$value, na.rm = TRUE)) %>% 
    dplyr::pull(.data$sd)
  
  #replace predicted NA with 0s
  predicted_tmp <- predicted %>% replace(is.na(.), 0)
  
  #calculate weighted individual pred, then get the sum
  weighted_pred <- predicted_tmp * weights 
  pred_out <- rowSums(weighted_pred)
  
  #in case too many models indicated NA, then the whole prediction becomes NA
  pred_out[pred_na] <- NA
  
  
  
  if(return_se){
    return(list(predicted = pred_out, sd = sd_pred,
                individual_pred = predicted))
  } else{
    return(weighted_pred)
  }
  
}


#maybe just use the parameters but nothing else, that will free the memory a bit
par_df <- purrr::map(input_list, function(x){
  
  df <- purrr::map(x$models, 'xbest') %>% 
    do.call(cbind, .) %>% 
    t() %>% 
    set_colnames(paste0('V', 1:10 )) %>% 
    as_tibble() %>% 
    mutate(repetition = 1:10)
  
  df$confidence <- x$confidence
  return(df)
           
           
}, .progress = TRUE) %>% 
  bind_rows(.id = 'spec_cult')
rm(par_list, performance_df, input_list, fit_list_combined)




return_predictions_together <- function(temp, par_df, theta_star = 279, Tc = 36, progress = FALSE){
  
  
  purrr::map(1:nrow(par_df), function(i){
    par <- par_df[i, paste0('V', 1:10)] %>%  unlist() %>%  unname()
    par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
    
    pred <- return_predicted_days(convert_parameters(par), 
                          modelfn = custom_PhenoFlex_GDHwrapper, 
                          SeasonList =temp,
                          na_penalty = NA)
    
    data.frame(pred = pred, 
               spec_cult = par_df$spec_cult[i], 
               repetition= par_df$repetition[i], 
               confidence = par_df$confidence[i])
  }, .progress = progress) %>% 
    do.call(rbind, .) 
}
 


rm(almond_fit, apple_fit, apricot_fit, cherry_fit, eplum_fit, jplum_fit, pear_fit, pistachio_fit)

f <- list.files('data/future_weather-v2/')
f_full <- list.files('data/future_weather-v2/', full.names = TRUE)


i_stop <-50
#
all_predictions <- purrr::map(1:i_stop, function(i){
  
  loc <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(1) %>% 
    unlist()
  
  ssp  <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(2) %>% 
    unlist()
  
  gcm <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(3) %>% 
    unlist()
  
  time <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(4) %>% 
    unlist()
  
  lat <- stations %>% 
    filter(station_name == loc) %>% 
    pull(latitude)
  
  out <-   read.csv(f_full[i]) %>% 
    chillR::stack_hourly_temps(latitude = lat) %>% 
    purrr::pluck('hourtemps') %>% 
    genSeasonList(years = 2001:2100) %>% 
    return_predictions_together(par_df) %>% 
    mutate(location = loc,
           gcm = gcm,
           ssp = ssp,
           scenario_year = time)

  
  return(out)
}, .progress = TRUE)


all_predictions_1 <- all_predictions %>% 
  bind_rows()

#bring it to form, so that the predictions of repeitions are in the columns

dir.create('data/predictions_future_v2')

all_predictions %>% 
  bind_rows() %>%  
  #add a year identifyier
  mutate(year_id = rep(1:100, nrow(.) / 100)) %>% 
  select(-confidence) %>% 
  pivot_wider(names_from = 'repetition', values_from = 'pred') %>% 
  write.csv('data/predictions_future_v2/predictions_batch_1.csv', row.names = FALSE)




#######
#second batch
######

i_start <- 51
i_stop <-100
#
all_predictions2 <- purrr::map(i_start:i_stop, function(i){
  
  loc <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(1) %>% 
    unlist()
  
  ssp  <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(2) %>% 
    unlist()
  
  gcm <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(3) %>% 
    unlist()
  
  time <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(4) %>% 
    unlist()
  
  lat <- stations %>% 
    filter(station_name == loc) %>% 
    pull(latitude)
  
  out <-   read.csv(f_full[i]) %>% 
    chillR::stack_hourly_temps(latitude = lat) %>% 
    purrr::pluck('hourtemps') %>% 
    genSeasonList(years = 2001:2100) %>% 
    return_predictions_together(par_df) %>% 
    mutate(location = loc,
           gcm = gcm,
           ssp = ssp,
           scenario_year = time)
  
  
  return(out)
}, .progress = TRUE)


all_predictions2 %>% 
  bind_rows() %>% 
  mutate(year_id = rep(1:100, nrow(.) / 100)) %>% 
  select(-confidence) %>% 
  pivot_wider(names_from = 'repetition', values_from = 'pred') %>% 
  write.csv('data/predictions_future_v2/predictions_batch_2.csv', row.names = FALSE)



#######
#third batch
######

i_start <- 101
i_stop <-150
#
all_predictions3 <- purrr::map(i_start:i_stop, function(i){
  
  loc <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(1) %>% 
    unlist()
  
  ssp  <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(2) %>% 
    unlist()
  
  gcm <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(3) %>% 
    unlist()
  
  time <- f[i] %>% 
    str_split('_') %>% 
    purrr::map(function(x) x[length(x)]) %>% 
    str_split('\\.') %>% 
    purrr::map(4) %>% 
    unlist()
  
  lat <- stations %>% 
    filter(station_name == loc) %>% 
    pull(latitude)
  
  out <-   read.csv(f_full[i]) %>% 
    chillR::stack_hourly_temps(latitude = lat) %>% 
    purrr::pluck('hourtemps') %>% 
    genSeasonList(years = 2001:2100) %>% 
    return_predictions_together(par_df) %>% 
    mutate(location = loc,
           gcm = gcm,
           ssp = ssp,
           scenario_year = time)
  
  
  return(out)
}, .progress = TRUE)


all_predictions3 %>% 
  bind_rows() %>% 
  mutate(year_id = rep(1:100, nrow(.) / 100)) %>% 
  select(-confidence) %>% 
  pivot_wider(names_from = 'repetition', values_from = 'pred') %>% 
  write.csv('data/predictions_future_v2/predictions_batch_3.csv', row.names = FALSE)





#######
#automate the rest of the batches
######

for(i in 4:16){
  print(i)
  
  
  i_start <- ((i-1)*50)+1
  i_stop <- i*50
  #
  if(i_stop > length(f)) i_stop <- length(f)
  
  all_predictions_rest <- purrr::map(i_start:i_stop, function(i){
    
    loc <- f[i] %>% 
      str_split('_') %>% 
      purrr::map(function(x) x[length(x)]) %>% 
      str_split('\\.') %>% 
      purrr::map(1) %>% 
      unlist()
    
    ssp  <- f[i] %>% 
      str_split('_') %>% 
      purrr::map(function(x) x[length(x)]) %>% 
      str_split('\\.') %>% 
      purrr::map(2) %>% 
      unlist()
    
    gcm <- f[i] %>% 
      str_split('_') %>% 
      purrr::map(function(x) x[length(x)]) %>% 
      str_split('\\.') %>% 
      purrr::map(3) %>% 
      unlist()
    
    time <- f[i] %>% 
      str_split('_') %>% 
      purrr::map(function(x) x[length(x)]) %>% 
      str_split('\\.') %>% 
      purrr::map(4) %>% 
      unlist()
    
    lat <- stations %>% 
      filter(station_name == loc) %>% 
      pull(latitude)
    
    out <-   read.csv(f_full[i]) %>% 
      chillR::stack_hourly_temps(latitude = lat) %>% 
      purrr::pluck('hourtemps') %>% 
      genSeasonList(years = 2001:2100) %>% 
      return_predictions_together(par_df) %>% 
      mutate(location = loc,
             gcm = gcm,
             ssp = ssp,
             scenario_year = time)
    
    
    return(out)
  }, .progress = TRUE)
  
  
  all_predictions_rest %>% 
    bind_rows() %>% 
    mutate(year_id = rep(1:100, nrow(.) / 100)) %>% 
    select(-confidence) %>% 
    pivot_wider(names_from = 'repetition', values_from = 'pred') %>% 
    write.csv(paste0('data/predictions_future_v2/predictions_batch_', i, '.csv'), row.names = FALSE)
  
  
}
  


#write.csv(all_predictions_df, 'data/projected_bloomdates_ensemble.csv', row.names = FALSE)

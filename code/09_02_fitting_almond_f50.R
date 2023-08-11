#yet another almond fitting
#I forgot santomera data

#setwd('../almond_pheno')

library(chillR)
library(tidyverse)
library(MEIGOR)
library(LarsChill)

sfax <- read.csv('data/weather_ready/sfax_1973-2021_fixed.csv')
meknes <- read.csv('data/weather_ready/meknes-bassatine-fixed_1973-2022.csv')
santomera <-read.csv('data/weather_ready/murcia_clean.csv')

stations <- read.csv('data/weather_ready/weather_station_phenological_observations.csv')


sfax_season <- sfax %>% 
  stack_hourly_temps(latitude = stations$latitude[stations$station_name == 'Sfax']) %>% 
  purrr::pluck('hourtemps') %>% 
  genSeasonList(years = (min(sfax$Year)+1):max(sfax$Year)) %>% 
  set_names((min(sfax$Year)+1):max(sfax$Year))

meknes_season <- meknes %>% 
  stack_hourly_temps(latitude = stations$latitude[stations$station_name == 'Meknes']) %>% 
  purrr::pluck('hourtemps') %>% 
  genSeasonList(years = (min(meknes$Year)+1):max(meknes$Year)) %>% 
  set_names((min(meknes$Year)+1):max(meknes$Year))

santomera_season <- santomera %>% 
  stack_hourly_temps(latitude = stations$latitude[stations$station_name == 'Santomera']) %>% 
  purrr::pluck('hourtemps') %>% 
  genSeasonList(years = (min(santomera$Year)+1):max(santomera$Year)) %>% 
  set_names((min(santomera$Year)+1):max(santomera$Year))


SeasonList <- list('Sfax' = sfax_season,
                   'Meknes' = meknes_season,
                   'Santomera' = santomera_season)

rm(santomera, sfax, meknes)


adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

almond_cult <- adamedor %>% 
  filter(is.na(flowering_f50) == FALSE,
         species == 'Almond') %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(),
            locations = length(unique(location)),
            countries = length(unique(country))) %>% 
  filter(n >= 20) %>% 
  dplyr::pull(cultivar)


#take all almond data

almond_sub <- adamedor %>% 
  filter(species == 'Almond',
         cultivar %in% almond_cult) %>% 
  drop_na(flowering_f50) %>% 
  mutate(flowering_f50 = lubridate::ymd(flowering_f50)) %>% 
  mutate(doy_begin = lubridate::yday(flowering_f50))

#only keep observations that have data for begin of flowering


cultivars <- unique(almond_sub$cultivar)
p <- 0.75
seed <- 1234567890



set.seed(123456789)
pheno_cal_list <- season_cal_list <- pheno_val_list <- season_val_list <- list()

for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()
  season_cal_list[[cult]] <-  season_val_list[[cult]] <- list()
  
  #check which and how many locations
  overview_df <- almond_sub %>% 
    dplyr::filter(cultivar == cult) %>% 
    group_by(location) %>% 
    summarise(n = n()) %>% 
    mutate(n_cal = floor(n * p),
           n_val = ceiling(n * (1 - p)))
  
  for(i in 1:10){
    pheno_cal_list[[cult]][[i]] <- data.frame()
    pheno_val_list[[cult]][[i]] <- data.frame()
    season_cal_list[[cult]][[i]] <-  list()
    season_val_list[[cult]][[i]] <- list()
    
    #for each location decide how much we take for training and calibration
    for(loc in overview_df$location){
      #extract years with observations
      pheno_years <- almond_sub %>% 
        dplyr::filter(cultivar == cult, location == loc) %>% 
        summarise(pheno_years = unique(year)) %>% 
        dplyr::pull(pheno_years)
      
      
      #decide which years belong to calibration and validation
      cal_years <- sort(sample(x = pheno_years, 
                               size = overview_df$n_cal[overview_df$location == loc], 
                               replace = FALSE))
      
      val_years <- pheno_years[!pheno_years %in% cal_years]
      
      
      #extract corresponding phenology data
      pheno_cal <- almond_sub %>% 
        dplyr::filter(location == loc, cultivar == cult, year %in% cal_years) %>% 
        dplyr::pull(doy_begin)
      
      pheno_val <- almond_sub %>% 
        dplyr::filter(location == loc, cultivar == cult, year %in% val_years) %>% 
        dplyr::pull(doy_begin)
      
      
      
      #add phenology information to list
      pheno_cal_list[[cult]][[i]] <- rbind(pheno_cal_list[[cult]][[i]],
                                           data.frame(location = loc,
                                                      year = cal_years,
                                                      pheno = pheno_cal))
      
      pheno_val_list[[cult]][[i]] <- rbind(pheno_val_list[[cult]][[i]],
                                           data.frame(location = loc,
                                                      year = val_years,
                                                      pheno = pheno_val))
      
      
      #add season data to season list
      season_cal_list[[cult]][[i]] <- c(season_cal_list[[cult]][[i]],
                                        SeasonList[[loc]][as.character(cal_years)]) 
      
      season_val_list[[cult]][[i]] <- c(season_val_list[[cult]][[i]],
                                        SeasonList[[loc]][as.character(val_years)]) 
      
      
    }
  }
  
  
}

#redo the fitting with lower bounds adjusted
#maybe use the parameters of the almond study as a starting point
#took sonora round 3 fitting from california as a starting point /rounded on 2 digits
#and maybe exlcude Tc and theta star´
#--> modify the fitting function for that
x_0 <- c(24.79,	337.04,	0.2529,	17.72,	285.54,	   45.67,	  29.49,	2.97,	1.87,	2.69)
#        yc      zc     s1      Tu      theta_c   tau      piec    Tf     Tb     slope
x_U <- c(40,    500,    1.0,    30,     287,       48,      50,    10,    10,     5.00)
x_L <- c(5,    100,    0.1,    15,     284,       16,      24,     2,     2,     1.2)

source('code/utilities/evaluation_function_meigo_nonlinear_fixed.R')

#limits for the inequality constraints
#         #gdh parameters   #q10 for E0 and E1
c_L <- c(  0,   0,   0,     1.5, 1.5)
c_U <- c(Inf, Inf, Inf,     3.5, 3.5)


problem<-list(f="custom_evaluation_function_meigo_nonlinear_fixed",
              x_0 = x_0,
              x_L = x_L,
              x_U = x_U,
              c_L = c_L, 
              c_U = c_U,
              vtr = 50)


#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 30, 
  maxeval = 30000,
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0,
  iterprint = 1,
  plot = 1)




set.seed(123456789)

res_list_refitted <- list()

pb = txtProgressBar(min = 0, max = 10*10, initial = 0, style = 3) 
stepi <- 0


#split fitting in several scripts

for(cult in almond_cult[1:10]){
  
  res_list_refitted[[cult]] <- list()
  
  for(i in 1:10){
    
    
    #x_0 <- almond_fit[[i]][[cult]]$xbest
    problem<-list(f="custom_evaluation_function_meigo_nonlinear_fixed",
                  x_0 = x_0,
                  x_L = x_L,
                  x_U = x_U,
                  c_L = c_L, 
                  c_U = c_U,
                  vtr = 50)
    
    res_list_refitted[[cult]][[i]] <- MEIGO(problem = problem,
                                            opts,
                                            algorithm="ESS", 
                                            modelfn = custom_PhenoFlex_GDHwrapper,
                                            bloomJDays = pheno_cal_list[[cult]][[i]]$pheno,
                                            SeasonList = season_cal_list[[cult]][[i]]) 
    
    stepi <- stepi + 1
    setTxtProgressBar(pb,stepi)
  }
  
}

res_list <- list()
for(i in 1:10){
  res_list[[i]] <- list()
  for(cult in almond_cult[1:10]){
    res_list[[i]][[cult]] <- res_list_refitted[[cult]][[i]]
  }
}

source('code/utilities/load_fitting_result.R')
source('code/utilities/save_fitting_list.R')
#save data
for(i in 1:10){
  save_fitting_list(fit_list = res_list[[i]], path = 'data/fitting/almond/repeated_fitting_with_santomera/', prefix = paste0('batch1_repeat', i, '_'))
}



for(cult in almond_cult[11:20]){
  
  res_list_refitted[[cult]] <- list()
  
  for(i in 1:10){
    
    
    #x_0 <- almond_fit[[i]][[cult]]$xbest
    problem<-list(f="custom_evaluation_function_meigo_nonlinear_fixed",
                  x_0 = x_0,
                  x_L = x_L,
                  x_U = x_U,
                  c_L = c_L, 
                  c_U = c_U,
                  vtr = 50)
    
    res_list_refitted[[cult]][[i]] <- MEIGO(problem = problem,
                                            opts,
                                            algorithm="ESS", 
                                            modelfn = custom_PhenoFlex_GDHwrapper,
                                            bloomJDays = pheno_cal_list[[cult]][[i]]$pheno,
                                            SeasonList = season_cal_list[[cult]][[i]]) 
    
    stepi <- stepi + 1
    setTxtProgressBar(pb,stepi)
  }
  
}

res_list <- list()
for(i in 1:10){
  res_list[[i]] <- list()
  for(cult in almond_cult[11:20]){
    res_list[[i]][[cult]] <- res_list_refitted[[cult]][[i]]
  }
}

source('code/utilities/load_fitting_result.R')
source('code/utilities/save_fitting_list.R')
#save data
for(i in 1:10){
  save_fitting_list(fit_list = res_list[[i]], path = 'data/fitting/almond/repeated_fitting_with_santomera/', prefix = paste0('batch2_repeat', i, '_'))
}



for(cult in almond_cult[21:30]){
  
  res_list_refitted[[cult]] <- list()
  
  for(i in 1:10){
    
    
    #x_0 <- almond_fit[[i]][[cult]]$xbest
    problem<-list(f="custom_evaluation_function_meigo_nonlinear_fixed",
                  x_0 = x_0,
                  x_L = x_L,
                  x_U = x_U,
                  c_L = c_L, 
                  c_U = c_U,
                  vtr = 50)
    
    res_list_refitted[[cult]][[i]] <- MEIGO(problem = problem,
                                            opts,
                                            algorithm="ESS", 
                                            modelfn = custom_PhenoFlex_GDHwrapper,
                                            bloomJDays = pheno_cal_list[[cult]][[i]]$pheno,
                                            SeasonList = season_cal_list[[cult]][[i]]) 
    
    stepi <- stepi + 1
    setTxtProgressBar(pb,stepi)
  }
  
}

res_list <- list()
for(i in 1:10){
  res_list[[i]] <- list()
  for(cult in almond_cult[21:30]){
    res_list[[i]][[cult]] <- res_list_refitted[[cult]][[i]]
  }
}

source('code/utilities/load_fitting_result.R')
source('code/utilities/save_fitting_list.R')
#save data
for(i in 1:10){
  save_fitting_list(fit_list = res_list[[i]], path = 'data/fitting/almond/repeated_fitting_with_santomera/', prefix = paste0('batch3_repeat', i, '_'))
}


for(cult in almond_cult[31:39]){
  
  res_list_refitted[[cult]] <- list()
  
  for(i in 1:10){
    
    
    #x_0 <- almond_fit[[i]][[cult]]$xbest
    problem<-list(f="custom_evaluation_function_meigo_nonlinear_fixed",
                  x_0 = x_0,
                  x_L = x_L,
                  x_U = x_U,
                  c_L = c_L, 
                  c_U = c_U,
                  vtr = 50)
    
    res_list_refitted[[cult]][[i]] <- MEIGO(problem = problem,
                                            opts,
                                            algorithm="ESS", 
                                            modelfn = custom_PhenoFlex_GDHwrapper,
                                            bloomJDays = pheno_cal_list[[cult]][[i]]$pheno,
                                            SeasonList = season_cal_list[[cult]][[i]]) 
    
    stepi <- stepi + 1
    setTxtProgressBar(pb,stepi)
  }
  
}

res_list <- list()
for(i in 1:10){
  res_list[[i]] <- list()
  for(cult in almond_cult[31:39]){
    res_list[[i]][[cult]] <- res_list_refitted[[cult]][[i]]
  }
}

source('code/utilities/load_fitting_result.R')
source('code/utilities/save_fitting_list.R')
#save data
for(i in 1:10){
  save_fitting_list(fit_list = res_list[[i]], path = 'data/fitting/almond/repeated_fitting_with_santomera/', prefix = paste0('batch4_repeat', i, '_'))
}




#read everything and make sure that they can be read properly

#load almond data
almond_fit1 <- almond_fit2 <- almond_fit3 <- almond_fit4 <- almond_fit <- list()


for(i in 1:10){
  
  almond_fit1[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting_with_santomera/',
                                          prefix = paste0('batch1_repeat', i, '_'))
  
  almond_fit2[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting_with_santomera/',
                                          prefix = paste0('batch2_repeat', i, '_'))
  
  
  almond_fit3[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting_with_santomera/',
                                          prefix = paste0('batch3_repeat', i, '_'))
  
  almond_fit4[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting_with_santomera/',
                                          prefix = paste0('batch4_repeat', i, '_'))
  
  almond_fit[[i]] <- c(almond_fit1[[i]], almond_fit2[[i]], almond_fit3[[i]], almond_fit4[[i]])
}


#save data
for(i in 1:10){
  save_fitting_list(almond_fit[[i]], path = 'data/fitting/almond/repeated_fitting_santomera_cleanly_saved/', prefix = paste0('repeat', i, '_'))
}

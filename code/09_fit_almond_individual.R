#fit cultivars individually, fix Tc and theta_star
setwd('../almond_pheno')

library(chillR)
library(tidyverse)
library(MEIGOR)
library(LarsChill)

sfax <- read.csv('data/weather_ready/sfax_1973-2021_fixed.csv')
meknes <- read.csv('data/weather_ready/meknes-bassatine-fixed_1973-2022.csv')

sfax %>% 
  mutate(Date = lubridate::ymd(paste0(Year, '-', Month, '-', Day))) %>% 
  ggplot(aes(x = Day, y = Tmin)) +
    geom_point() +
  facet_grid(~Month)

sfax %>% 
  mutate(Date = lubridate::ymd(paste0(Year, '-', Month, '-', Day))) %>% 
  ggplot(aes(x = Day, y = Tmax)) +
  geom_point() +
  facet_grid(~Month)

meknes %>% 
  mutate(Date = lubridate::ymd(paste0(Year, '-', Month, '-', Day))) %>% 
  ggplot(aes(x = Day, y = Tmin)) +
  geom_point() +
  facet_grid(~Month)

meknes %>% 
  mutate(Date = lubridate::ymd(paste0(Year, '-', Month, '-', Day))) %>% 
  ggplot(aes(x = Day, y = Tmax)) +
  geom_point() +
  facet_grid(~Month)


#make hourly
coord_meknes <- c(33.88, -5.54)
coord_sfax <-c(34.75, 10.75)

sfax_hourly <- stack_hourly_temps(sfax, latitude = coord_sfax[1])
meknes_hourly <- stack_hourly_temps(meknes, latitude = coord_meknes[1])

sfax_hourly <- sfax_hourly$hourtemps
meknes_hourly <- meknes_hourly$hourtemps

#make seasonal list
sfax_season <- genSeasonList(sfax_hourly, years = 1974:2021)
meknes_season <- genSeasonList(meknes_hourly, years = 1973:2021)

names(sfax_season) <- 1974:2021
names(meknes_season) <- 1973:2021

SeasonList <- list('Sfax' = sfax_season,
                   'Meknes' = meknes_season)


adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

adamedor_sum <- adamedor %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(),
            locations = length(unique(location)),
            countries = length(unique(country)))

almond_cult <- adamedor_sum %>% 
  filter(species == 'Almond', 
         n >= 20) %>% 
  dplyr::pull(cultivar)



#take all almond data

almond_sub <- adamedor %>% 
  filter(species == 'Almond',
         cultivar %in% almond_cult) %>% 
  drop_na(begin_flowering_f5) %>% 
  mutate(begin_flowering_f5 = lubridate::ymd(begin_flowering_f5)) %>% 
  mutate(doy_begin = lubridate::yday(begin_flowering_f5))

#only keep observations that have data for begin of flowering


cultivars <- unique(almond_sub$cultivar)
p <- 0.75
seed <- 1234567890


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
    pheno_cal_list[[cult]] <- rbind(pheno_cal_list[[cult]],
                                    data.frame(location = loc,
                                               year = cal_years,
                                               pheno = pheno_cal))
    
    pheno_val_list[[cult]] <- rbind(pheno_val_list[[cult]],
                                    data.frame(location = loc,
                                               year = val_years,
                                               pheno = pheno_val))
    
    
    #add season data to season list
    season_cal_list[[cult]] <- c(season_cal_list[[cult]],
                                 SeasonList[[loc]][as.character(cal_years)]) 
    
    season_val_list[[cult]] <- c(season_val_list[[cult]],
                                 SeasonList[[loc]][as.character(val_years)]) 
    
    
  }
  
}




ncult <- length(unique(almond_sub$cultivar))

#maybe use the parameters of the almond study as a starting point
#took sonora round 3 fitting from california as a starting point /rounded on 2 digits
#and maybe exlcude Tc and theta star´
#--> modify the fitting function for that
#        yc      zc     s1      Tu      theta_c   tau      piec    Tf     Tb     slope
x_0 <- c(24.79,	337.04,	0.2529,	17.72,	285.54,	   45.67,	  29.49,	2.97,	1.87,	2.69)
x_U <- c(80,    500,    1.0,    30,     287,       48,      50,    10,    10,     5.00)
x_L <- c(20,    100,    0.1,    15,     284,       16,      24,     2,     2,     1.2)

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
              c_U = c_U)


#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 10, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0,
  plot = 1)

set.seed(123456789)

res_list <- list()

for(cult in cultivars){
  
  
  
  res_list[[cult]] <- MEIGO(problem = problem,
                    opts,
                    algorithm="ESS", 
                    modelfn = custom_PhenoFlex_GDHwrapper,
                    bloomJDays = pheno_cal_list[[cult]]$pheno,
                    SeasonList = season_cal_list[[cult]])
}



source('code/utilities/load_save_fitting_results.R')

dir.create('data/fitting/almond')
save_fitting_list(res_list, path = 'data/fitting/almond/', prefix = 'r1_')

test <- load_fitting_result(path = 'data/fitting/almond/', prefix = 'r1_')


#retrieve model performance and model prediction
performance_df <- prediction_df <- data.frame()
pheno_cal_pred <- pheno_val_pred <- NULL
Tc = 36
theta_star = 279


for(i in 1:length(cultivars)){
  
  cult <- cultivars[i]
  
  #extract parameters
  par <- test[[cult]]$xbest 
  #add fixed parameters
  par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
  
  
  
  pheno_cal_pred <- return_predicted_days(convert_parameters(par), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_cal_list[[cult]])
  pheno_val_pred <- return_predicted_days(convert_parameters(par), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_val_list[[cult]])
  
  prediction_df <- rbind.data.frame(prediction_df,
                                    rbind.data.frame(cbind.data.frame(cultivar = cult,
                                                                      data = 'calibration',
                                                                      pheno_cal_list[[cult]],
                                                                      pheno_pred = pheno_cal_pred),
                                                     
                                                     cbind.data.frame(cultivar = cult,
                                                                      data = 'validation',
                                                                      pheno_val_list[[cult]],
                                                                      pheno_pred = pheno_val_pred)
                                    ))
  
  
}


######
#almond
######

prediction_df %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0) +
  facet_wrap(~cultivar)
  #coord_cartesian(ylim = c(60, 140))

performance <- prediction_df %>% 
  group_by(cultivar, data) %>% 
  summarize(RMSE = chillR::RMSEP(pheno, pheno_pred),
            RPIQ = chillR::RPIQ(pheno, pheno_pred),
            bias = mean(pheno_pred - pheno))

performance %>% 
  ggplot(aes(y = cultivar, fill = data, x = RMSE)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 5, linetype = 'dashed')+
  scale_y_discrete(limits=rev)


performance %>% 
  ggplot(aes(y = cultivar, fill = data, x = RPIQ)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed')+
  scale_y_discrete(limits=rev)

#maybe plot all parameters to see if there is a certain trend

purrr::map(res_list, 'xbest') %>% 
  bind_cols() %>% 
  mutate(par_name = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'piec', 'Tf', 'Tb', 'slope')) %>% 
  reshape2::melt(id.vars = 'par_name', variable.name = 'cultivar') %>%
  ggplot(aes(y = value, x = 1)) +
  #geom_point(aes(col = cultivar,)) +
  geom_violin()+
  facet_wrap(~par_name, scales = 'free_y')

#yc lower than 40 for sure
#pie_c in most cases smaller than 30, rather 25
#tau mostly between 35 and 40
#theta_c mostly arounf 286.5
#Tu is all over the place, but never smaller than 15 (my limit) but not higher than 25

#do second round of fitting, this time with fixed temperature data

#round two
#        yc      zc     s1      Tu      theta_c   tau      piec    Tf     Tb     slope
x_U <- c(40,    500,    1.0,    25,     287,       48,      40,    10,    10,     5.00)
x_L <- c(20,    100,    0.1,    15,     284,       20,      24,     2,     2,     1.2)


#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 10, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0,
  plot = 1)

pb = txtProgressBar(min = 0, max = length(cultivars), initial = 0, style = 3) 
stepi <- 0

set.seed(123456789)

res_list2 <- list()

for(cult in cultivars){
  setTxtProgressBar(pb,stepi)
  
  x_0 <- res_list[[cult]]$xbest
  
  problem<-list(f="custom_evaluation_function_meigo_nonlinear_fixed",
                x_0 = x_0,
                x_L = x_L,
                x_U = x_U,
                c_L = c_L, 
                c_U = c_U)
  
  
  res_list2[[cult]] <- MEIGO(problem = problem,
                            opts,
                            algorithm="ESS", 
                            modelfn = custom_PhenoFlex_GDHwrapper,
                            bloomJDays = pheno_cal_list[[cult]]$pheno,
                            SeasonList = season_cal_list[[cult]])
  
  stepi <- stepi + 1
}


save_fitting_list(res_list2, path = 'data/fitting/almond/', prefix = 'r2_')

prediction_df$round <- 1
round <- 2
for(cult in cultivars){
  
  #extract parameters
  par <- res_list2[[cult]]$xbest 
  #add fixed parameters
  par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
  
  
  
  pheno_cal_pred <- return_predicted_days(convert_parameters(par), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_cal_list[[cult]])
  pheno_val_pred <- return_predicted_days(convert_parameters(par), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_val_list[[cult]])
  
  prediction_df <- rbind.data.frame(prediction_df,
                                    rbind.data.frame(cbind.data.frame(cultivar = cult,
                                                                      data = 'calibration',
                                                                      pheno_cal_list[[cult]],
                                                                      pheno_pred = pheno_cal_pred,
                                                                      round = round),
                                                     
                                                     cbind.data.frame(cultivar = cult,
                                                                      data = 'validation',
                                                                      pheno_val_list[[cult]],
                                                                      pheno_pred = pheno_val_pred,
                                                                      round = round)
                                    ))
  
  
}

prediction_df %>% 
  filter(round == 2) %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0) +
  facet_wrap(~cultivar)
#coord_cartesian(ylim = c(60, 140))

prediction_df %>% 
  filter(round == 1) %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0) +
  facet_wrap(~cultivar)
#coord_cartesian(ylim = c(60, 140))

prediction_df %>% 
  filter(cultivar == 'Ferragnes') %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0) +
  facet_wrap(~round)

performance <- prediction_df %>% 
  group_by(cultivar, data, round) %>% 
  summarize(RMSE = chillR::RMSEP(pheno, pheno_pred),
            RPIQ = chillR::RPIQ(pheno, pheno_pred),
            bias = mean(pheno_pred - pheno))

performance %>% 
  filter(round == 2) %>% 
  ggplot(aes(y = cultivar, fill = data, x = RMSE)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 5, linetype = 'dashed')+
  scale_y_discrete(limits=rev)
#they look differently.....







######
#repeated fitting
#######




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



#maybe use the parameters of the almond study as a starting point
#took sonora round 3 fitting from california as a starting point /rounded on 2 digits
#and maybe exlcude Tc and theta star´
#--> modify the fitting function for that
#        yc      zc     s1      Tu      theta_c   tau      piec    Tf     Tb     slope
x_0 <- c(24.79,	337.04,	0.2529,	17.72,	285.54,	   45.67,	  29.49,	2.97,	1.87,	2.69)
x_U <- c(80,    500,    1.0,    30,     287,       48,      50,    10,    10,     5.00)
x_L <- c(20,    100,    0.1,    15,     284,       16,      24,     2,     2,     1.2)

source('code/utilities/evaluation_function_meigo_nonlinear_fixed.R')

#limits for the inequality constraints
#         #gdh parameters   #q10 for E0 and E1
c_L <- c(  0,   0,   0,     1.5, 1.5)
c_U <- c(Inf, Inf, Inf,     3.5, 3.5)



#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 30, 
  maxeval = 50000,
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0,
  iterprint = 1,
  plot = 1)

pb = txtProgressBar(min = 0, max = length(cultivars)*10, initial = 0, style = 3) 
stepi <- 0

set.seed(123456789)

res_list2 <- list()

for(cult in cultivars){
  
  res_list2[[cult]] <- list()
  
  for(i in 1:10){
    
    res_list2[[cult]][[i]] <- MEIGO(problem = problem,
                                    opts,
                                    algorithm="ESS", 
                                    modelfn = custom_PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_cal_list[[cult]][[i]]$pheno,
                                    SeasonList = season_cal_list[[cult]][[i]]) 
    
    stepi <- stepi + 1
    setTxtProgressBar(pb,stepi)
  }
  
}


#have to orginize data differently to save them easier
res_list3 <- list()
for(i in 1:10){
  res_list3[[i]] <- list()
  for(cult in cultivars){
    res_list3[[i]][[cult]] <- res_list2[[cult]][[i]]
  }
}

dir.create('data/fitting/almond/repeated_fitting')
source('code/utilities/load_save_fitting_results.R')

#save data
# for(i in 1:10){
#   save_fitting_list(res_list3[[i]], path = 'data/fitting/almond/repeated_fitting/', prefix = paste0('repeat', i, '_'))
# }









#redo fitting because the limits were not set properly

#redo the fitting with lower bounds adjusted
#maybe use the parameters of the almond study as a starting point
#took sonora round 3 fitting from california as a starting point /rounded on 2 digits
#and maybe exlcude Tc and theta star´
#--> modify the fitting function for that
x_0 <- c(24.79,	337.04,	0.2529,	17.72,	285.54,	   45.67,	  29.49,	2.97,	1.87,	2.69)
#        yc      zc     s1      Tu      theta_c   tau      piec    Tf     Tb     slope
x_U <- c(35,    500,    1.0,    30,     287,       48,      50,    10,    10,     5.00)
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


almond_fit <- list()
for(i in 1:10){
  almond_fit[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}

pb = txtProgressBar(min = 0, max = length(almond_fit[[1]])*10, initial = 0, style = 3) 
stepi <- 0

set.seed(123456789)

res_list_refitted <- list()


#split fitting in several scripts

for(cult in names(almond_fit[[1]])[14:26]){
  
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


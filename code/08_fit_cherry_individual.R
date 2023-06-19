#setwd('../almond_pheno')

#fit cultivars individually, fix Tc and theta_star
# devtools::install_github('larscaspersen/addition_chillR')
# 
# require(BiocManager)
# BiocManager::install('MEIGOR')
require(chillR)
library(tidyverse)
library(MEIGOR)
library(LarsChill)

#need zaragoza and klein altendorf
cka <- read.csv('data/weather_ready/temp_cka_1958-2022.csv')
zaragoza <- read.csv('data/weather_raw/temp_zgz_1973-2022.csv')

#make hourly
coord_zaragoza <- c(41.65, -0.88)
coord_cka <- c(50.61, 6.99)

cka_hourly <- stack_hourly_temps(cka, latitude = coord_cka[1])
zaragoza_hourly <- stack_hourly_temps(zaragoza, latitude = coord_zaragoza[1])

cka_hourly <- cka_hourly$hourtemps
zaragoza_hourly <- zaragoza_hourly$hourtemps

#make seasonal list
cka_season <- genSeasonList(cka_hourly, years = min(cka$Year):max(cka$Year))
zaragoza_season <- genSeasonList(zaragoza_hourly, years = min(zaragoza$Year):max(zaragoza$Year))

names(cka_season) <- min(cka$Year):max(cka$Year)
names(zaragoza_season) <- min(zaragoza$Year):max(zaragoza$Year)

SeasonList <- list('Zaragoza' = zaragoza_season, 
                    'Klein-Altendorf' = cka_season)


adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

adamedor_sum <- adamedor %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(),
            locations = length(unique(location)),
            countries = length(unique(country)))

species_cult <- adamedor_sum %>% 
  filter(species == 'Sweet Cherry', 
         n >= 20) %>% 
  dplyr::pull(cultivar)



#take all almond data


species_sub <- adamedor %>% 
  filter(species == 'Sweet Cherry',
         cultivar %in% species_cult) %>% 
  drop_na(flowering_f50) %>% 
  mutate(begin_flowering_f50 = lubridate::ymd(flowering_f50)) %>% 
  mutate(doy_begin = lubridate::yday(flowering_f50))

#only keep observations that have data for begin of flowering


cultivars <- unique(species_sub$cultivar)
p <- 0.75
seed <- 1234567890


pheno_cal_list <- season_cal_list <- pheno_val_list <- season_val_list <- list()

for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()
  season_cal_list[[cult]] <-  season_val_list[[cult]] <- list()
  
  #check which and how many locations
  overview_df <- species_sub %>% 
    dplyr::filter(cultivar == cult) %>% 
    group_by(location) %>% 
    summarise(n = n()) %>% 
    mutate(n_cal = floor(n * p),
           n_val = ceiling(n * (1 - p)))
  
  #for each location decide how much we take for training and calibration
  for(loc in overview_df$location){
    #extract years with observations
    pheno_years <- species_sub %>% 
      dplyr::filter(cultivar == cult, location == loc) %>% 
      summarise(pheno_years = unique(year)) %>% 
      dplyr::pull(pheno_years)
    
    
    #decide which years belong to calibration and validation
    cal_years <- sort(sample(x = pheno_years, 
                             size = overview_df$n_cal[overview_df$location == loc], 
                             replace = FALSE))
    
    val_years <- pheno_years[!pheno_years %in% cal_years]
    
    
    #extract corresponding phenology data
    pheno_cal <- species_sub %>% 
      dplyr::filter(location == loc, cultivar == cult, year %in% cal_years) %>% 
      dplyr::pull(doy_begin)
    
    pheno_val <- species_sub %>% 
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




ncult <- length(unique(species_sub$cultivar))

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

#save fitting results
dir.create('data/fitting')
dir.create('data/fitting/sweet_cherry')

source('code/utilities/load_save_fitting_results.R')

save_fitting_list(fit_list = res_list, path = 'data/fitting/sweet_cherry/', prefix = 'r1_')

test <- load_fitting_result(path = 'data/fitting/sweet_cherry/', prefix = 'r1_')


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

#############
#sweet cherry
#############

prediction_df %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0) +
  facet_wrap(~cultivar) +
  coord_cartesian(ylim = c(60, 140))

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

purrr::map(res_list, 'xbest') %>% 
  bind_cols() %>% 
  mutate(par_name = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'piec', 'Tf', 'Tb', 'slope')) %>% 
  reshape2::melt(id.vars = 'par_name', variable.name = 'cultivar') %>%
  ggplot(aes(y = value, x = 1)) +
  #geom_point(aes(col = cultivar,)) +
  geom_violin()+
  facet_wrap(~par_name, scales = 'free_y')

#cultivars with poor performance are
#Blanca Provinza
#Burlat
#Compact Stella
#Lambert
#Napoleon
#Newstar
#Regina
#Sam
#Star
#Van_Spur

#source('code/utilities/combined_temp_response.R')

par_round1 <- purrr::map(res_list, 'xbest') %>% 
  bind_cols() %>% 
  mutate(par_name = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope')) %>% 
  reshape2::melt(id.vars = 'par_name', variable.name = 'cultivar') %>%
  reshape2::dcast(cultivar ~ par_name, value.name = 'value') %>% 
  mutate(Tc = Tc, 
         theta_star = theta_star)

LarsChill::gen_combined_temp_response_plot(par_round1, weather_list = list(cka_hourly, zaragoza_hourly), )


cult_r2 <- performance %>% 
  filter(RMSE > 5) %>% 
  summarise(cultivar = unique(cultivar))

cult_r2 <- cult_r2$cultivar




#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 10, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0,
  plot = 1)

pb = txtProgressBar(min = 0, max = length(cult_r2), initial = 0, style = 3) 
stepi <- 0

set.seed(123456789)

res_list2 <- list()

for(cult in cult_r2){
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

save_fitting_list(res_list2, path = 'data/fitting/sweet_cherry/', prefix = 'r2_')

prediction_df$round <- 1

for(cult in cult_r2){
  
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
                                                                      round = 2),
                                                     
                                                     cbind.data.frame(cultivar = cult,
                                                                      data = 'validation',
                                                                      pheno_val_list[[cult]],
                                                                      pheno_pred = pheno_val_pred,
                                                                      round = 2)
                                    ))
  
  
}

prediction_df %>% 
  filter(round == 2) %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0) +
  facet_wrap(~cultivar) +
  coord_cartesian(ylim = c(60, 140))

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


#try different split (because calibration is really well but validation is not) in:
#blanca de provenza
#lapins
#napoleon
#newstar
#sam
#star
#van-spur


#simply more fitting for
#sandom rose
#burlat


#regina predicts no flowering in 2020 and can't get it right...
#same for sylvia, maybe there is something in the weather data








set.seed(123456789)
pheno_cal_list <- season_cal_list <- pheno_val_list <- season_val_list <- list()

for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()
  season_cal_list[[cult]] <-  season_val_list[[cult]] <- list()
  
  #check which and how many locations
  overview_df <- species_sub %>% 
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
      pheno_years <- species_sub %>% 
        dplyr::filter(cultivar == cult, location == loc) %>% 
        summarise(pheno_years = unique(year)) %>% 
        dplyr::pull(pheno_years)
      
      
      #decide which years belong to calibration and validation
      cal_years <- sort(sample(x = pheno_years, 
                               size = overview_df$n_cal[overview_df$location == loc], 
                               replace = FALSE))
      
      val_years <- pheno_years[!pheno_years %in% cal_years]
      
      
      #extract corresponding phenology data
      pheno_cal <- species_sub %>% 
        dplyr::filter(location == loc, cultivar == cult, year %in% cal_years) %>% 
        dplyr::pull(doy_begin)
      
      pheno_val <- species_sub %>% 
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


source('code/utilities/load_save_fitting_results.R')
dir.create('data/fitting/sweet_cherry/repeated_fitting')

#have to orginize data differently to save them easier
res_list3 <- list()
for(i in 1:10){
  res_list3[[i]] <- list()
  for(cult in cultivars){
    res_list3[[i]][[cult]] <- res_list2[[cult]][[i]]
  }
}

#save data
for(i in 1:10){
  save_fitting_list(res_list3[[i]], path = 'data/fitting/sweet_cherry/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}

#make predictions based on cultivar parameters
#retrieve model performance and model prediction
performance_df <- prediction_df <- data.frame()
pheno_cal_pred <- pheno_val_pred <- NULL
Tc = 36
theta_star = 279

for(i in 1:10){
  for(cult in cultivars){
    
    #extract parameters
    par <- res_list3[[i]][[cult]]$xbest 
    #add fixed parameters
    par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
    
    
    
    pheno_cal_pred <- return_predicted_days(convert_parameters(par), 
                                            modelfn = custom_PhenoFlex_GDHwrapper, 
                                            SeasonList = season_cal_list[[cult]][[i]])
    pheno_val_pred <- return_predicted_days(convert_parameters(par), 
                                            modelfn = custom_PhenoFlex_GDHwrapper, 
                                            SeasonList = season_val_list[[cult]][[i]])
    
    prediction_df <- rbind.data.frame(prediction_df,
                                      rbind.data.frame(cbind.data.frame(repetition = i,
                                                                        cultivar = cult,
                                                                        data = 'calibration',
                                                                        pheno_cal_list[[cult]][[i]],
                                                                        pheno_pred = pheno_cal_pred),
                                                       
                                                       cbind.data.frame(repetition = i,
                                                                        cultivar = cult,
                                                                        data = 'validation',
                                                                        pheno_val_list[[cult]][[i]],
                                                                        pheno_pred = pheno_val_pred)
                                      ))
    
    
  }
}


prediction_df %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_abline(slope = 1, linetype = 'dashed') + 
  geom_point() +
  facet_grid(repetition ~ cultivar)


prediction_df %>% 
  filter(repetition %in% c(1, 10), cultivar == 'Star') %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_abline(slope = 1, linetype = 'dashed') + 
  geom_point() +
  facet_grid(repetition ~ cultivar)


iqr_df <- prediction_df %>% 
  group_by(repetition, cultivar) %>% 
  summarize(iqr = IQR(pheno))



performance <- prediction_df %>% 
  merge.data.frame(iqr_df, by = c('repetition', 'cultivar')) %>% 
  group_by(cultivar, data, repetition, iqr) %>% 
  summarize(RMSE = chillR::RMSEP(pheno, pheno_pred),
            bias = mean(pheno_pred - pheno)) %>% 
  mutate(RPIQ = iqr / RMSE)

performance %>% 
  ggplot(aes(y = cultivar, fill = data, x = RMSE)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 5, linetype = 'dashed')+
  scale_y_discrete(limits=rev)


performance %>% 
  ggplot(aes(y = cultivar, fill = data, x = RPIQ)) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  geom_boxplot() +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed')+
  scale_y_discrete(limits=rev)

par_df <- data.frame()
for(i in 1:10){
  for(cult in cultivars){
    
    par <- res_list3[[i]][[cult]]$xbest
    par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
    
    par_df <- rbind.data.frame(par_df,
                               data.frame(t(par), 'cultivar' = i, 'cult' = cult, 'repitition' = i))    
    
  }
}
colnames(par_df)[1:12] <- c('yc', 'zc', 's1', 'Tu', 'theta_star', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tc', 'Tb', 'slope')
par_df$cultivar <- as.factor(par_df$cultivar)

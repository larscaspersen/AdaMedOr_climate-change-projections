#fit cultivars individually, fix Tc and theta_star
setwd('../almond_pheno')

library(chillR)
library(tidyverse)
library(MEIGOR)
library(LarsChill)


zaragoza <- read.csv('data/weather_raw/temp_zgz_1973-2022.csv')
cieza <- readxl::read_xlsx('data/weather_raw/Cieza(95_22)Tmax&Tmin.xlsx')



cieza <- cieza %>% 
  dplyr::select(-Hours) %>% 
  mutate(Month = lubridate::month(Date),
         Day = lubridate::day(Date))

zaragoza %>% 
  mutate(Date = lubridate::ymd(paste0(Year, '-', Month, '-', Day))) %>% 
  ggplot(aes(x = Day, y = Tmin)) +
  geom_point() +
  facet_grid(~Month)

zaragoza %>% 
  mutate(Date = lubridate::ymd(paste0(Year, '-', Month, '-', Day))) %>% 
  ggplot(aes(x = Day, y = Tmax)) +
  geom_point() +
  facet_grid(~Month)

cieza %>% 
  mutate(Month = lubridate::month(Date),
         Day = lubridate::day(Date)) %>% 
  ggplot(aes(x = Day, y = Tmin)) +
  geom_point() +
  facet_grid(~Month)

cieza %>% 
  mutate(Month = lubridate::month(Date),
         Day = lubridate::day(Date)) %>% 
  ggplot(aes(x = Day, y = Tmax)) +
  geom_point() +
  facet_grid(~Month)

#need co

#make hourly
coord_zaragoza <- c(41.65, -0.88)
coord_cieza <-c(38.24, -1.41)

zaragoza_hourly <- stack_hourly_temps(zaragoza, latitude = coord_zaragoza[1])
cieza_hourly <- stack_hourly_temps(cieza, latitude = coord_cieza[1])

zaragoza_hourly <- zaragoza_hourly$hourtemps
cieza_hourly <- cieza_hourly$hourtemps

#make seasonal list
zaragoza_season <- genSeasonList(zaragoza_hourly, years = min(zaragoza$Year):max(zaragoza$Year))
cieza_season <- genSeasonList(cieza_hourly, years = min(cieza$Year):max(cieza$Year))

names(zaragoza_season) <- min(zaragoza$Year):max(zaragoza$Year)
names(cieza_season) <-  min(cieza$Year):max(cieza$Year)

SeasonList <- list('Zaragoza' = zaragoza_season, 
                   'Cieza' = cieza_season)


adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

adamedor_sum <- adamedor %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(),
            locations = length(unique(location)),
            countries = length(unique(country)))

almond_cult <- adamedor_sum %>% 
  filter(species == 'Apricot', 
         n >= 20) %>% 
  dplyr::pull(cultivar)



#take all almond data

almond_sub <- adamedor %>% 
  filter(species == 'Apricot',
         cultivar %in% almond_cult) %>% 
  drop_na(flowering_f50) %>% 
  mutate(flowering_f50 = lubridate::ymd(flowering_f50)) %>% 
  mutate(doy_begin = lubridate::yday(flowering_f50))

#only keep observations that have data for begin of flowering


cultivars <- unique(almond_sub$cultivar)
p <- 0.75
seed <- 1234567890


#there are two observations in 2007 for Apricot Goldrich in Cieza (2007-03-11 and 2007-03-12)
almond_sub <- almond_sub[!(almond_sub$location == 'Cieza' & 
             almond_sub$cultivar == 'Goldrich' &
             almond_sub$year == 2007 &
             almond_sub$flowering_f50 == "2007-03-11"), ]

#duplicated
almond_sub <- almond_sub[-(which(almond_sub$location == 'Cieza' & 
               almond_sub$cultivar == 'Canino' &
               almond_sub$year == 2009)[1]), ]

#duplicated
almond_sub <- almond_sub[-(which(almond_sub$location == 'Cieza' & 
                                   almond_sub$cultivar == 'Canino' &
                                   almond_sub$year == 2006)[1]), ]

#difference of 4 days, took the first one
almond_sub <- almond_sub[-(which(almond_sub$location == 'Cieza' & 
                                   almond_sub$cultivar == 'Búlida' &
                                   almond_sub$year == 2015)[1]), ]

#difference of three weeks, excluded both observations
almond_sub <- almond_sub[-(which(almond_sub$location == 'Cieza' & 
                                   almond_sub$cultivar == 'Búlida' &
                                   almond_sub$year == 2021)), ]

#difference of 12 days, excluded both observations
almond_sub <- almond_sub[-(which(almond_sub$location == 'Cieza' & 
                                   almond_sub$cultivar == 'Dorada' &
                                   almond_sub$year == 2021)), ]

#difference of four days


# almond_sub <- almond_sub %>% 
#   filter(location != 'Cieza',
#          cultivar != 'Goldrich',
#          year != 2007,
#          flowering_f50 != '2007-03-11')


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
  
  for(step in 1:10){
    
  }
  
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

# res_list <- list()
# 
# for(cult in cultivars){
#   
#   
#   
#   res_list[[cult]] <- MEIGO(problem = problem,
#                             opts,
#                             algorithm="ESS", 
#                             modelfn = custom_PhenoFlex_GDHwrapper,
#                             bloomJDays = pheno_cal_list[[cult]]$pheno,
#                             SeasonList = season_cal_list[[cult]])
# }


# source('code/utilities/load_save_fitting_results.R')
# dir.create('data/fitting/apricot')
# save_fitting_list(res_list, path = 'data/fitting/apricot/', prefix = 'r1_')

res_list <- load_fitting_result('data/fitting/apricot/', prefix = 'r1_')


#retrieve model performance and model prediction
performance_df <- prediction_df <- data.frame()
pheno_cal_pred <- pheno_val_pred <- NULL
Tc = 36
theta_star = 279

cultivars <- names(res_list)

for(cult in cultivars){

  #extract parameters
  par <- res_list[[cult]]$xbest 
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
#apricot
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

#stark and pepepito need more fitting
#henderson and sunglo maybe overfit?


performance %>% 
  ggplot(aes(y = cultivar, fill = data, x = RPIQ)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
 # coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = 1, linetype = 'dashed')+
  scale_y_discrete(limits=rev)

#maybe redo the fitting
purrr::map(res_list, 'numeval')
#something between 33,000 and 58,000 evaluations
f_list <- purrr::map(res_list, 'f')



#get rmse for a matrix of parameters
matrix_to_predicted <- function(matrix, 
                           SeasonList,
                           observed_pheno,
                           Tc = 36,
                           theta_star = 279){
  
  pred_df <- data.frame(NULL)
  
  for(i in 1:nrow(matrix)){
    
    par <- matrix[i,]
    par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
    
    pheno_cal_pred <- return_predicted_days(convert_parameters(par), 
                                            modelfn = custom_PhenoFlex_GDHwrapper, 
                                            SeasonList = SeasonList)
    
    pred_df <- rbind.data.frame(pred_df,
                     data.frame(step = i,
                                predicted = pheno_cal_pred,
                                observed = observed_pheno$pheno,
                                year = observed_pheno$year,
                                location = observed_pheno$location))
  }
  
  return(pred_df)
}

purrr::map2(res_list, names(res_list), function(fitted, cultivar){
  matrix_to_predicted(matrix = fitted$x, SeasonList = season_cal_list[[cultivar]], observed_pheno = pheno_cal_list[[cultivar]])
}) %>% 
  bind_rows(.id = 'cultivar') %>% 
  group_by(cultivar, step) %>% 
  summarize(RMSE = chillR::RMSEP(observed, predicted),
            RPIQ = chillR::RPIQ(observed, predicted),
            bias = mean(predicted - observed),
            n = n()) %>% 
  ungroup() %>% 
  mutate(f = unname(unlist(purrr::map(res_list, function(x) x$f)))) %>% 
  ggplot(aes(x = f / n, y = RMSE, col = cultivar)) +
  geom_point()

#stop if f is smaller than 100










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
      
      
      if(overview_df$n_cal[overview_df$location == loc] == 0){
        
        cal_years <- NULL
        val_years <- pheno_years
        
      } else{
        #decide which years belong to calibration and validation
        cal_years <- sort(sample(x = pheno_years, 
                                 size = overview_df$n_cal[overview_df$location == loc], 
                                 replace = FALSE)) 
        
        val_years <- pheno_years[!pheno_years %in% cal_years]
      }
      

      
      
      #extract corresponding phenology data
      pheno_cal <- almond_sub %>% 
        dplyr::filter(location == loc, cultivar == cult, year %in% cal_years) %>% 
        dplyr::pull(doy_begin)
      
      pheno_val <- almond_sub %>% 
        dplyr::filter(location == loc, cultivar == cult, year %in% val_years) %>% 
        dplyr::pull(doy_begin)
      
      
      
      if(length(pheno_cal) != 0){
        #add phenology information to list
        pheno_cal_list[[cult]][[i]] <- rbind(pheno_cal_list[[cult]][[i]],
                                             data.frame(location = loc,
                                                        year = cal_years,
                                                        pheno = pheno_cal))
      }

      
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

#redo the fitting only for cultivars which have cieza

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

# res_list2 <- list()
# 
# for(cult in cultivars){
#   
#   res_list2[[cult]] <- list()
#   
#   for(i in 1:10){
#    
#     res_list2[[cult]][[i]] <- MEIGO(problem = problem,
#                                     opts,
#                                     algorithm="ESS", 
#                                     modelfn = custom_PhenoFlex_GDHwrapper,
#                                     bloomJDays = pheno_cal_list[[cult]][[i]]$pheno,
#                                     SeasonList = season_cal_list[[cult]][[i]]) 
#     
#     stepi <- stepi + 1
#     setTxtProgressBar(pb,stepi)
#   }
#   
# }


#dir.create('data/fitting/apricot/repeated_fitting')

#have to orginize data differently to save them easier
res_list3 <- list()
for(i in 1:10){
  res_list3[[i]] <- list()
  for(cult in cultivars){
    res_list3[[i]][[cult]] <- res_list2[[cult]][[i]]
  }
}

#save data
# for(i in 1:10){
#   save_fitting_list(res_list3[[i]], path = 'data/fitting/apricot/repeated_fitting/', prefix = paste0('repeat', i, '_'))
# }



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

performance <- prediction_df %>% 
  group_by(cultivar, data, repetition) %>% 
  summarize(RMSE = chillR::RMSEP(pheno, pheno_pred),
            RPIQ = chillR::RPIQ(pheno, pheno_pred),
            bias = mean(pheno_pred - pheno))

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

LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == 'Mitger', ], weather_list = zaragoza_hourly)  
LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == 'Henderson', ], weather_list = zaragoza_hourly) 
LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == 'Pandora', ], weather_list = zaragoza_hourly) 
LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == 'Stark E. Orange', ], weather_list = zaragoza_hourly) 
LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == 'Veecot', ], weather_list = zaragoza_hourly)
LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == 'Tadeo', ], weather_list = zaragoza_hourly)
LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == 'Harcot', ], weather_list = zaragoza_hourly)
LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == 'Canino', ], weather_list = zaragoza_hourly)
LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == 'Sunglo', ], weather_list = zaragoza_hourly)
LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == 'Pepito del Rubio', ], weather_list = zaragoza_hourly)


#for some cultivars it seems that the heat submodel yields very similar parameters, but for chill quite a lot of differences

test_res <- list()
for(i in 1:10){
  test_res[[i]] <- load_fitting_result(path = 'data/fitting/apricot/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}
#seems like fitting and saving works well now





#redo fitting for cieza cultivars, because I accidentally filtered them out
cult_redo <- almond_sub %>% 
  filter(location == 'Cieza') %>% 
  group_by(cultivar) %>% 
  summarize(cultivar = unique(cultivar)) %>% 
  pull(cultivar)


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

#redo the fitting only for cultivars which have cieza

#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 30, 
  maxeval = 30000,
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0,
  iterprint = 1,
  plot = 1)

pb = txtProgressBar(min = 0, max = length(cult_redo)*10, initial = 0, style = 3) 
stepi <- 0

set.seed(123456789)

res_list_redo <- list()

for(cult in cult_redo){

  res_list_redo[[cult]] <- list()

  for(i in 1:10){

    res_list_redo[[cult]][[i]] <- MEIGO(problem = problem,
                                    opts,
                                    algorithm="ESS",
                                    modelfn = custom_PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_cal_list[[cult]][[i]]$pheno,
                                    SeasonList = season_cal_list[[cult]][[i]])

    stepi <- stepi + 1
    setTxtProgressBar(pb,stepi)
  }

}


res_list4 <- list()
for(i in 1:10){
  res_list4[[i]] <- list()
  for(cult in cult_redo){
    res_list4[[i]][[cult]] <- res_list_redo[[cult]][[i]]
  }
}


source('code/utilities/load_save_fitting_results.R')
for(i in 1:10){
  save_fitting_list(res_list4[[i]], path = 'data/fitting/apricot/repeated_fitting_with_cieza/', prefix = paste0('repeat', i, '_'))
}






#save apricot cleanly

library(LarsChill)

source('code/utilities/load_save_fitting_results.R')

#load fitting results

#number of repititions
r <- 10

apricot_fit <- apricot_fit2 <- list()

for(i in 1:r){
  apricot_fit[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  apricot_fit2[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting_with_cieza/', prefix = paste0('repeat', i, '_'))
}


names(apricot_fit2[[1]])
#add the cultivars which are not in apricot fit2 from apricot fit
apricot_zaragoza_only  <- names(apricot_fit[[1]])[!names(apricot_fit[[1]]) %in% names(apricot_fit2[[1]])]

#add cultivars which I had not to refit
for(i in 1:r){
  for(cult in apricot_zaragoza_only){
    apricot_fit2[[i]][[cult]] <- apricot_fit[[i]][[cult]]
  }
  
}

dir.create('data/fitting/apricot/repeated_fitting_clean/')

for(i in 1:10){
  save_fitting_list(apricot_fit2[[i]], path = 'data/fitting/apricot/repeated_fitting_clean/', prefix = paste0('repeat', i, '_'))
}
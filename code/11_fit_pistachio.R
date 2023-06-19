library(chillR)
library(tidyverse)
library(LarsChill)
library(MEIGOR)

setwd('../../../../../Desktop/almond_pheno/')

#fit other cultivars / species


adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

adamedor_sum <- adamedor %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(),
            locations = length(unique(location)),
            countries = length(unique(country)))

adamedor_sum <- adamedor_sum %>% 
  filter(!(species %in% c('Almond', 'Sweet Cherry', 'Apricot'))) %>% 
  filter(n >= 20)


#pistacio

species_cult <- adamedor_sum %>% 
  filter(species == 'Pistachio', 
         n >= 20) %>% 
  dplyr::pull(cultivar)

#in case of pistachio the data was not formatted right
adamedor <- read.csv('data/combined_phenological_data_adamedor.csv', 
                     colClasses="character",na.strings="?")

adamedor_transformed <- adamedor %>% 
  filter(species == 'Pistachio',
         cultivar %in% species_cult) %>% 
  mutate(begin_flowering_f5 = lubridate::dmy(begin_flowering_f5),
         flowering_f50 = lubridate::dmy(flowering_f50),
         flowering_f90 = lubridate::dmy(flowering_f90))

#I have f5 and f50 and f90



species_sub <- adamedor_transformed %>% 
  filter(species == 'Pistachio',
         cultivar %in% species_cult) %>% 
  drop_na(flowering_f50) %>% 
  mutate(begin_flowering_f50 = lubridate::ymd(flowering_f50)) %>% 
  mutate(doy_begin = lubridate::yday(flowering_f50),
         location = 'Sfax')



#need data from sfax
sfax <- read.csv('data/weather_ready/sfax_1973-2021_fixed.csv')

#make hourly
coord_sfax <-c(34.75, 10.75)

sfax_hourly <- stack_hourly_temps(sfax, latitude = coord_sfax[1])

sfax_hourly <- sfax_hourly$hourtemps

#make seasonal list
sfax_season <- genSeasonList(sfax_hourly, years = 1974:2021)

names(sfax_season) <- 1974:2021

SeasonList <- list('Sfax' = sfax_season)


cultivars <- unique(species_sub$cultivar)

p <- 0.75
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

dir.create('data/fitting/pistachio')
source('code/utilities/load_save_fitting_results.R')


res_list3 <- list()
for(i in 1:10){
  res_list3[[i]] <- list()
  for(cult in cultivars){
    res_list3[[i]][[cult]] <- res_list2[[cult]][[i]]
  }
}

#save data
for(i in 1:10){
  save_fitting_list(res_list3[[i]], path = 'data/fitting/pistachio/', prefix = paste0('repeat', i, '_'))
}

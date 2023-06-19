#this script collects the different training / validation splits for the cultivars
#it is not well written, because it is copy pasted from the individual scripts for the species
#result is a csv file with the different splits, so that I do not need to do the split each time I run a different script


library(chillR)
library(tidyverse)
library(LarsChill)

#--------------#
# pistachio ####
#--------------#


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

species_sub <- adamedor_transformed %>% 
  filter(species == 'Pistachio',
         cultivar %in% species_cult) %>% 
  drop_na(flowering_f50) %>% 
  mutate(begin_flowering_f50 = lubridate::ymd(flowering_f50)) %>% 
  mutate(doy_begin = lubridate::yday(flowering_f50),
         location = 'Sfax')

cultivars <- unique(species_sub$cultivar)
p <- 0.75
set.seed(123456789)
pheno_cal_list <- pheno_val_list <- list()

for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()

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
      
    }
  }
  
  
}
#need to save a document saying which year from which location and species belongs to training and validation
master_pheno <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(master_pheno) <- c('species', 'cultivar', 'location', 'repetition', 'split','year',  'pheno', 'measurement_type')

for(cult in cultivars){
  for(i in 1:r){
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Pistachio', 
                                                      cultivar = cult, 
                                                      location = pheno_cal_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Calibration',
                                                      year = pheno_cal_list[[cult]][[i]]$year,
                                                      pheno = pheno_cal_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'flowering_f50')
    )
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Pistachio', 
                                                      cultivar = cult, 
                                                      location = pheno_val_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Validation',
                                                      year = pheno_val_list[[cult]][[i]]$year,
                                                      pheno = pheno_val_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'flowering_f50')
    )
    
  }
}




#------------------------#
#sweet cherry ####
#------------------------#


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
set.seed(123456789)
pheno_cal_list <- pheno_val_list <- list()

for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()

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
      
      
    }
  }
  
  
}



for(cult in cultivars){
  for(i in 1:r){
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Sweet Cherry', 
                                                      cultivar = cult, 
                                                      location = pheno_cal_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Calibration',
                                                      year = pheno_cal_list[[cult]][[i]]$year,
                                                      pheno = pheno_cal_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'flowering_f50')
    )
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Sweet Cherry', 
                                                      cultivar = cult, 
                                                      location = pheno_val_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Validation',
                                                      year = pheno_val_list[[cult]][[i]]$year,
                                                      pheno = pheno_val_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'flowering_f50')
    )
    
  }
}



#-------------------------#
#apricot ####
#-------------------------#


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

#there is a duplicated observation in apricot, remove it
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

#only keep observations that have data for begin of flowering


cultivars <- unique(almond_sub$cultivar)


set.seed(123456789)
pheno_cal_list <- pheno_val_list <- list()

for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()

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
      
      
    }
  }
  
  
}


for(cult in cultivars){
  for(i in 1:r){
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Apricot', 
                                                      cultivar = cult, 
                                                      location = pheno_cal_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Calibration',
                                                      year = pheno_cal_list[[cult]][[i]]$year,
                                                      pheno = pheno_cal_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'flowering_f50')
    )
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Apricot', 
                                                      cultivar = cult, 
                                                      location = pheno_val_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Validation',
                                                      year = pheno_val_list[[cult]][[i]]$year,
                                                      pheno = pheno_val_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'flowering_f50')
    )
    
  }
}




#-------------------#
#almond ####
#-------------------#


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


pheno_cal_list <- pheno_val_list <- list()

for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()
  
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
      
      
    }
  }
  
  
}


for(cult in cultivars){
  for(i in 1:r){
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Almond', 
                                                      cultivar = cult, 
                                                      location = pheno_cal_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Calibration',
                                                      year = pheno_cal_list[[cult]][[i]]$year,
                                                      pheno = pheno_cal_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'begin_flowering_f5')
    )
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Almond', 
                                                      cultivar = cult, 
                                                      location = pheno_val_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Validation',
                                                      year = pheno_val_list[[cult]][[i]]$year,
                                                      pheno = pheno_val_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'begin_flowering_f5')
    )
    
  }
}


#---------------#
#european plum ####
#---------------#

adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

adamedor_sum <- adamedor %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(),
            locations = length(unique(location)),
            countries = length(unique(country)))

species_cult <- adamedor_sum %>% 
  filter(species == 'European plum', 
         n >= 20) %>% 
  dplyr::pull(cultivar)



#take all almond data


species_sub <- adamedor %>% 
  filter(species == 'European plum',
         cultivar %in% species_cult) %>% 
  drop_na(begin_flowering_f5) %>% 
  mutate(begin_flowering_f5 = lubridate::ymd(begin_flowering_f5)) %>% 
  mutate(doy_begin = lubridate::yday(begin_flowering_f5))

#only keep observations that have data for begin of flowering


cultivars <- unique(species_sub$cultivar)

p <- 0.75
seed <- 1234567890

set.seed(123456789)
pheno_cal_list <- pheno_val_list <- list()

for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()
  
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
      
      
    }
  }
  
  
}


for(cult in cultivars){
  for(i in 1:r){
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'European Plum', 
                                                      cultivar = cult, 
                                                      location = pheno_cal_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Calibration',
                                                      year = pheno_cal_list[[cult]][[i]]$year,
                                                      pheno = pheno_cal_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'begin_flowering_f5')
    )
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'European Plum', 
                                                      cultivar = cult, 
                                                      location = pheno_val_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Validation',
                                                      year = pheno_val_list[[cult]][[i]]$year,
                                                      pheno = pheno_val_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'begin_flowering_f5')
    )
    
  }
}



#------------------#
#japanese plum####
#------------------#

adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

adamedor_sum <- adamedor %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(),
            locations = length(unique(location)),
            countries = length(unique(country)))

species_cult <- adamedor_sum %>% 
  filter(species == 'Japanese plum', 
         n >= 20) %>% 
  dplyr::pull(cultivar)



#take all almond data


species_sub <- adamedor %>% 
  filter(species == 'Japanese plum',
         cultivar %in% species_cult) %>% 
  drop_na(begin_flowering_f5) %>% 
  mutate(begin_flowering_f5 = lubridate::ymd(begin_flowering_f5)) %>% 
  mutate(doy_begin = lubridate::yday(begin_flowering_f5))

#only keep observations that have data for begin of flowering


cultivars <- unique(species_sub$cultivar)

p <- 0.75
seed <- 1234567890

set.seed(123456789)
pheno_cal_list <- pheno_val_list <- list()

for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()
  
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
      

    }
  }
  
  
}


for(cult in cultivars){
  for(i in 1:r){
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Japanese Plum', 
                                                      cultivar = cult, 
                                                      location = pheno_cal_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Calibration',
                                                      year = pheno_cal_list[[cult]][[i]]$year,
                                                      pheno = pheno_cal_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'begin_flowering_f5')
    )
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Japanese Plum', 
                                                      cultivar = cult, 
                                                      location = pheno_val_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Validation',
                                                      year = pheno_val_list[[cult]][[i]]$year,
                                                      pheno = pheno_val_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'begin_flowering_f5')
    )
    
  }
}


#----------------------------#
#pear####
#----------------------------#

adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

adamedor_sum <- adamedor %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(),
            locations = length(unique(location)),
            countries = length(unique(country)))

species_cult <- adamedor_sum %>% 
  filter(species == 'Pear', 
         n >= 20) %>% 
  dplyr::pull(cultivar)



#take all almond data

species_sub <- adamedor %>% 
  filter(species == 'Pear',
         cultivar %in% species_cult) %>% 
  drop_na(flowering_f50) %>% 
  mutate(flowering_f50 = lubridate::ymd(flowering_f50)) %>% 
  mutate(doy_begin = lubridate::yday(flowering_f50))


cultivars <- unique(species_sub$cultivar)
p <- 0.75
seed <- 1234567890


pheno_cal_list <- pheno_val_list <-  list()

for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()
  
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

      
    }
  }
  
  
}


for(cult in cultivars){
  for(i in 1:r){
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Pear', 
                                                      cultivar = cult, 
                                                      location = pheno_cal_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Calibration',
                                                      year = pheno_cal_list[[cult]][[i]]$year,
                                                      pheno = pheno_cal_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'flowering_f50')
    )
    
    master_pheno <- rbind.data.frame(master_pheno,
                                     cbind.data.frame(species = 'Pear', 
                                                      cultivar = cult, 
                                                      location = pheno_val_list[[cult]][[i]]$location,
                                                      repetition = i, 
                                                      split = 'Validation',
                                                      year = pheno_val_list[[cult]][[i]]$year,
                                                      pheno = pheno_val_list[[cult]][[i]]$pheno,
                                                      measurement_type = 'flowering_f50')
    )
    
  }
}


write.csv(master_pheno, 'data/master_phenology_repeated_splits.csv', row.names = FALSE)

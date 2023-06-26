#download cmip6 and extract data

library(LarsChill)
library(tidyverse)

source('code/utilities/format_cmip6.R')

stations <- read.csv('data/weather_ready/weather_station_phenological_observations.csv')
stations <- stations %>% 
  mutate(id = station_name,
         Latitude = latitude,
         Longitude = longitude)

source('code/utilities/extract_cmip6_no_internet.R')

cmip6_downloaded <- extract_cmip6_no_internet(coordinates = stations)

formatted_cmip6 <- format_downloaded_ssp(cmip6_downloaded)

#remove the empty historic scenarios
remove_empy_entries <- function(cmip6_one_station){
  purrr::map(cmip6_one_station, function(x){
  if(nrow(x) == 0){
    return(NULL)
  } else {
    return(x)
  }
} )
}


#also remove historical scenarios
remove_empy_entries_wrapper <- function(cmip6_list){
  
  int <- purrr::map(cmip6_list, function(x){
    test <- remove_empy_entries(formatted_cmip6$Zaragoza)
    test <- test[lapply(test,length)>0]
    
    return(test)
  })
  
  purrr::map(int, function(x){
    drop <- grep(x = names(x), pattern = 'historical')
    
    return(x[-drop])
  })
  

}
  
formatted_cmip6_clean <- remove_empy_entries_wrapper(cmip6_list = formatted_cmip6)



#read weather observations
cka <- read.csv('data/weather_ready/cka_clean.csv')
cieza <- read.csv('data/weather_ready/cieza_clean.csv')
sfax <- read.csv('data/weather_ready/sfax_clean.csv')
meknes <- read.csv('data/weather_ready/meknes_clean.csv')
zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv') %>% 
  filter(Year < 2022)

names(formatted_cmip6)
weather_list <- list('Zaragoza' = zaragoza, 
     'Klein-Altendorf' = cka, 
     'Sfax' = sfax,
     'Cieza' = cieza,
     'Meknes' = meknes)


scenarios_rel_change <- gen_rel_change_scenario(downloaded_list = formatted_cmip6_clean, 
                        weather_list = weather_list)

#weather generator 

sum(is.na(weather_list$Zaragoza$Tmax))

#the weather generator only works when lubirdate works when this package loaded
library(lubridate)


names(weather_list)
i <- 1
dir.create('data/future_weather')

for(loc in c("Zaragoza", "Sfax", "Cieza", "Meknes")){
  
  for(i in 1:length(scenarios_rel_change[[loc]])){
    
    print(paste0('Generating ', loc, ', scenario number: ', i, ' of ', length(scenarios_rel_change[[loc]])))
    scen <- names(scenarios_rel_change[[loc]])[i]
    
    temps <- temperature_generation(weather = weather_list[[loc]],
                                    #years = c(min(weather_list$Zaragoza$Year),max(weather_list$Zaragoza$Year)),
                                    years = c(min(weather_list[[loc]]$Year), max(weather_list[[loc]]$Year)), 
                                    sim_years = c(2000, 2100),
                                    temperature_scenario = list(scenarios_rel_change[[loc]][[i]]),
                                    temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
    
    fname <- paste0('data/future_weather/', i, '_', loc, '_', scen, '.csv')
    write.csv(x = temps, file = fname, row.names = FALSE)
    
  }

}

start_time <- Sys.time()
temps <- temperature_generation(weather = weather_list$Zaragoza,
                                #years = c(min(weather_list$Zaragoza$Year),max(weather_list$Zaragoza$Year)),
                                years = c(min(weather_list$Zaragoza$Year),2021), 
                                sim_years = c(2000, 2100),
                                temperature_scenario = scenarios_rel_change$Zaragoza,
                                temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
end_time <- Sys.time()
end_time - start_time
#2min per scenario










master_pheno <- read.csv('data/master_phenology_repeated_splits.csv')

master_reduced <- master_pheno %>% 
  filter(repetition == 1) %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(),
            location = paste0(unique(location), collapse = ", "))

write.csv(master_reduced, 'data/overiew_pheno.csv', row.names = FALSE)

master_pheno %>% 
  filter(repetition == 1, 
         cultivar == 'Canino') %>% 
  group_by(species, cultivar) %>% 
  summarise(location = paste(unique(location)),
            n = n())
  

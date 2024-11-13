#download cmip6 and extract data

library(LarsChill)
library(tidyverse)


stations <- read.csv('data/weather_ready/weather_station_phenological_observations.csv')
stations <- stations %>% 
  mutate(id = station_name,
         Latitude = latitude,
         Longitude = longitude)


area <- c(max(stations$latitude), min(stations$longitude), min(stations$latitude), max(stations$longitude))
area <- area + c(5, -5, -5, 5)
area <- purrr::map_dbl(area, floor)

chillR::download_cmip6_ecmwfr(scenarios = c('ssp126', 'ssp245', 'ssp370', 'ssp585'), 
                              area = area,
                              user = '243306', 
                              key = 'cf909b0a-39cd-417f-89fa-198963d45ef7', 
                              model = 'default')


chillR::download_baseline_cmip6_ecmwfr(area = area,
                                       user = '243306', 
                                       key = 'cf909b0a-39cd-417f-89fa-198963d45ef7')

scen_list <- chillR::extract_cmip6_data(stations) %>% 
  chillR::gen_rel_change_scenario()

write.csv(scen_list, file = 'data/temp_scenarios_future.csv', row.names = FALSE)

scen_list <- chillR::convert_scen_information(scen_list)



#for each weather station get the median year, then make the baseline adjustment
cka <- read.csv('data/weather_ready/cka_clean.csv') %>% 
  filter(Year < 2022)
cieza <- read.csv('data/weather_ready/cieza_clean_patched.csv')
sfax <- read.csv('data/weather_ready/sfax_clean.csv')
meknes <- read.csv('data/weather_ready/meknes_clean.csv')
zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv') %>% 
  filter(Year < 2022)
santomera <- read.csv('data/weather_ready/murcia_clean.csv')

weather_obs_list <- list('Klein-Altendorf' = cka,
                         'Zaragoza' = zaragoza,
                         'Cieza' = cieza,
                         'Santomera' = santomera,
                         'Meknes' = meknes,
                         'Sfax' = sfax)
rm(zaragoza, cieza, cka, santomera, sfax, meknes)



med_obs <- purrr::map(weather_obs_list, function(x) median(min(x$Year):max(x$Year)))

#all the scenarios are calculated relative to baseline year 2000, so we need to do some adjustment

#calculate scenario for observed weather in 2000 and for 
library(lubridate)

source('code/utilities/purrr_temperature_generation.R')

for(stat in names(weather_obs_list)){
  
  #stat <- 'Klein-Altendorf'
  if(stat %in% c('Klein-Altendorf', 'Zaragoza', 'Cieza')){
    cat(paste0(stat, ' skipped\n'))
    next()
  } else {
    cat(paste0('Generating weather for: ', stat, '\n'))
  }
  
  #get temperature scenario for actual median of observed temperatures
  med_scenario_obs <- chillR::temperature_scenario_from_records(weather_obs_list[[stat]],
                                                                med_obs[[stat]])
  
  #get temperature scenario for needed time point
  scenario_2000 <- chillR::temperature_scenario_from_records(weather_obs_list[[stat]],
                                                             2000)
  
  #calculate the baseline adjustment needed for the observed weather 
  base <- chillR::temperature_scenario_baseline_adjustment(med_scenario_obs,
                                                   scenario_2000)
  
  scen_list_sub <- scen_list %>% 
    chillR::convert_scen_information() %>% 
    filter(location == stat) %>% 
    chillR::convert_scen_information(give_structure = FALSE)
  
  #adjust the calculated temperature scenarios for the future weather
  adjusted_list <- chillR::temperature_scenario_baseline_adjustment(base,
                                                                    scen_list_sub,
                                                            temperature_check_args=
                                                              list( scenario_check_thresholds = c(-5, 15)))
  
  
  
  #start <- Sys.time()
  temps <- temperature_generation_modified(weather_obs_list[[stat]], 
                                  years = c(min(weather_obs_list[[stat]]$Year), max(weather_obs_list[[stat]]$Year)), 
                                  sim_years = c(2001, 2100), 
                                  adjusted_list,    
                                  temperature_check_args=
                                    list( scenario_check_thresholds = c(-5, 15)))
  #end <- Sys.time()
  
  
  
  save_temperature_scenarios(temps,
                             "data/future_weather-v2/",
                             paste0("future_weather_", stat))
  
  
  
}








weather_obs <- read.csv('data/weather_ready/')




#source('code/utilities/extract_cmip6_no_internet.R')

#cmip6_downloaded <- extract_cmip6_no_internet(coordinates = stations)

cmip6_downloaded <- LarsChill::get_scenarioMIP_data(coordinates = stations, 
                                                    start_year = 2015, 
                                                    end_year = 2100,
                                                    metric = c('tasmin', 'tasmax'),
                                                    experiment = c('ssp126', 'ssp245', 'ssp370', 'ssp585'))

formatted_cmip6 <- format_downloaded_ssp(cmip6_downloaded)
#I have only 5 gcms, that is a bit poor


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
cka <- read.csv('data/weather_ready/cka_clean.csv') %>% 
  filter(Year < 2022)
cieza <- read.csv('data/weather_ready/cieza_clean_patched.csv')
sfax <- read.csv('data/weather_ready/sfax_clean.csv')
meknes <- read.csv('data/weather_ready/meknes_clean.csv')
zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv') %>% 
  filter(Year < 2022)
santomera <- read.csv('data/weather_ready/murcia_clean.csv')

names(formatted_cmip6)
weather_list <- list('Zaragoza' = zaragoza, 
     'Klein-Altendorf' = cka, 
     'Sfax' = sfax,
     'Cieza' = cieza,
     'Meknes' = meknes,
     'Santomera' = santomera)


scenarios_rel_change <- gen_rel_change_scenario(downloaded_list = formatted_cmip6_clean, 
                        weather_list = weather_list)

#weather generator 

sum(is.na(weather_list$Zaragoza$Tmax))

#the weather generator only works when lubirdate works when this package loaded
library(lubridate)

names(weather_list)
i <- 1
dir.create('data/future_weather')

for(loc in c("Zaragoza", "Sfax", "Cieza", "Meknes", "Klein-Altendorf", 'Santomera')){
  
  for(i in 1:length(scenarios_rel_change[[loc]])){
    
    #get scenario file
    scen <- names(scenarios_rel_change[[loc]])[i]
    
    #get file name
    fname <- paste0('data/future_weather/', i, '_', loc, '_', scen, '.csv')
    
    #check if the file already exists
    if(file.exists(fname)){
      next()
    }
    
    
    print(paste0('Generating ', loc, ', scenario number: ', i, ' of ', length(scenarios_rel_change[[loc]])))

    
    temps <- temperature_generation(weather = weather_list[[loc]],
                                    #years = c(min(weather_list$Zaragoza$Year),max(weather_list$Zaragoza$Year)),
                                    years = c(min(weather_list[[loc]]$Year), max(weather_list[[loc]]$Year)), 
                                    sim_years = c(2000, 2100),
                                    temperature_scenario = list(scenarios_rel_change[[loc]][[i]]),
                                    temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
    

    write.csv(x = temps, file = fname, row.names = FALSE)
    
  }

}



library(lubridate)

#historic simulations of 2015 conditions

hist_gen_weather <- list()

for(i in 1:length(weather_list)){
  
  #get scenario
  scen <- chillR::temperature_scenario_from_records(weather_list[[i]], year = 2015)
  
   loc <- names(weather_list)[i]
  
  #generate weather
  hist_gen_weather[[loc]] <- chillR::temperature_generation(weather_list[[i]], 
                                 years = c(min(weather_list[[i]]$Year), max(weather_list[[i]]$Year)), 
                                 sim_years = c(2000, 2100), 
                                 temperature_scenario = scen)
  
}

dir.create('data/hist-sim-weather')

chillR::save_temperature_scenarios(hist_gen_weather, path = 'data/hist-sim-weather/', prefix = 'hist_gen_2015')



str(hist_gen_weather)







# start_time <- Sys.time()
# temps <- temperature_generation(weather = weather_list$Zaragoza,
#                                 #years = c(min(weather_list$Zaragoza$Year),max(weather_list$Zaragoza$Year)),
#                                 years = c(min(weather_list$Zaragoza$Year),2021), 
#                                 sim_years = c(2000, 2100),
#                                 temperature_scenario = scenarios_rel_change$Zaragoza,
#                                 temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
# end_time <- Sys.time()
# end_time - start_time
#2min per scenario



















# sum(is.na(weather_list$`Klein-Altendorf`$Tmax))
# 
# 
# names(scenarios_rel_change)
# #do the same for klein-altendorf
# names(weather_list)
# i <- 1
# #dir.create('data/future_weather')
# 
# for(loc in c("Cieza")){
#   
#   for(i in 1:length(scenarios_rel_change[[loc]])){
#     
#     print(paste0('Generating ', loc, ', scenario number: ', i, ' of ', length(scenarios_rel_change[[loc]])))
#     scen <- names(scenarios_rel_change[[loc]])[i]
#     
#     temps <- temperature_generation(weather = weather_list[[loc]],
#                                     #years = c(min(weather_list$Zaragoza$Year),max(weather_list$Zaragoza$Year)),
#                                     years = c(min(weather_list[[loc]]$Year), max(weather_list[[loc]]$Year)), 
#                                     sim_years = c(2000, 2100),
#                                     temperature_scenario = list(scenarios_rel_change[[loc]][[i]]),
#                                     temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
#     
#     fname <- paste0('data/future_weather/', i, '_', loc, '_', scen, '.csv')
#     write.csv(x = temps, file = fname, row.names = FALSE)
#     
#   }
#   
# }



# tail(weather_list$Cieza)
# weather <- weather_list$Cieza
# years <- c(min(weather_list[['Cieza']]$Year), max(weather_list[['Cieza']]$Year))
# 
# 
# TEMP_MAX <- weather[, c("Month", "Day", "Year", "Tmax")]
# colnames(TEMP_MAX) <- c("month", "day", "year", "station")
# TEMP_MIN <- weather[, c("Month", "Day", "Year", "Tmin")]
# colnames(TEMP_MIN) <- c("month", "day", "year", "station")
# 
# year_min <- years[1]
# year_max <- years[2]
# 
# alldays <- make_all_day_table(tab = data.frame(Year = c(year_min, 
#                                                         year_max), Month = c(1, 12), Day = c(1, 31)), no_variable_check = TRUE)
# alldays[, "YEARMODA"] <- alldays$Year * 10000 + alldays$Month * 
#   100 + alldays$Day
# Tn <- TEMP_MIN
# Tn[, "YEARMODA"] <- Tn$year * 10000 + Tn$month * 100 + Tn$day
# allTmins <- merge(alldays, Tn, by = "YEARMODA", all.x = TRUE)
# Tx <- TEMP_MAX
# Tx[, "YEARMODA"] <- Tx$year * 10000 + Tx$month * 100 + Tx$day
# allTmaxs <- merge(alldays, Tx, by = "YEARMODA", all.x = TRUE)
# miss <- length(which(is.na(allTmins$station))) + length(which(is.na(allTmaxs$station)))
# 
# #find a weather station close by and add information+
# 
# loc <- stations %>% 
#   filter(station_name == 'Cieza') %>% 
#   dplyr::select(Latitude, Longitude)
# 
# possible_stations <- handle_gsod(action = 'list_stations', location = loc)
# 
# #download murcia station
# murcia <- handle_gsod(action = 'download_weather', 
#                       location =  possible_stations$chillR_code[1], 
#                       time_interval = c(1973, 2022),
#                       station_list = possible_stations)
# 
# murcia <- handle_gsod(murcia)
# 
# cieza_patched <- patch_daily_temperatures(cieza,murcia$MURCIA$weather)

# gw <- handle_gsod(action = "download_weather",
#                   location = "724828_93241",
#                   time_interval = c(2010, 2012),
#                   station_list = stat_list,
#                   quiet = TRUE)


#make historic scenarios for or 2021

# hist_scen <- chillR::temperature_scenario_from_records(weather_list, year = 2021)
# locations <- c("Zaragoza", "Sfax", "Cieza", "Meknes", "Klein-Altendorf")
# 
# dir.create('data/hist_sim_weather/')
# 
# for(loc in locations){
#   
#   print(paste0('Generating historic scenario for ', loc))
# 
#   temps <- temperature_generation(weather = weather_list[[loc]],
#                                   #years = c(min(weather_list$Zaragoza$Year),max(weather_list$Zaragoza$Year)),
#                                   years = c(min(weather_list[[loc]]$Year), max(weather_list[[loc]]$Year)), 
#                                   sim_years = c(2000, 2100),
#                                   temperature_scenario = list(hist_scen[[loc]]),
#                                   temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
#   
#   fname <- paste0('data/future_weather/', i, '_', loc, '_', scen, '.csv')
#   write.csv(x = temps, file = fname, row.names = FALSE)
#   
#   
# }










# master_pheno <- read.csv('data/master_phenology_repeated_splits.csv')
# 
# master_reduced <- master_pheno %>% 
#   filter(repetition == 1) %>% 
#   group_by(species, cultivar) %>% 
#   summarise(n = n(),
#             location = paste0(unique(location), collapse = ", "))
# 
# write.csv(master_reduced, 'data/overiew_pheno.csv', row.names = FALSE)
# 
# master_pheno %>% 
#   filter(repetition == 1, 
#          cultivar == 'Canino') %>% 
#   group_by(species, cultivar) %>% 
#   summarise(location = paste(unique(location)),
#             n = n())
  

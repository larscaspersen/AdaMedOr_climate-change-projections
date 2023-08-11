library(chillR)
library(tidyverse)

source('code/utilities/handle_gsod_new.R')

adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv') %>% 
  filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),
         !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014))

unique(adamedor$location)

coord_santomera<- c(-1.05, 38.06)

stations <- handle_gsod_new(action = 'list_stations', time_interval = c(1990, 2020), 
                            ocation = coord_santomera)


# stat_spatial <- SpatialPointsDataFrame(data.frame(stations$Long, stations$Lat), stations, 
#                                        proj4string = CRS("+init=epsg:4326"))
# 
# library(mapview)
# 
# mapview(stat_spatial)

adamedor %>% 
  filter(location == 'Santomera') %>% 
  summarise(min(year), max(year))

#download data from 1990 to 2022

murcia <- handle_gsod_new(action = 'download_weather', 
                          location = stations$chillR_code[1], 
                          time_interval = c(1990, 2022))

murcia <- handle_gsod_new(murcia)


sum(is.na(murcia$MURCIA$Tmin))  / nrow(murcia$MURCIA)
sum(is.na(murcia$MURCIA$Tmax))  / nrow(murcia$MURCIA)

#0.4% of data missing

#fill gaps with auxiliary weather stations

aux1 <- handle_gsod_new(action = 'download_weather', 
                        location = stations$chillR_code[2:4], 
                        time_interval = c(1990, 2022)) %>% 
  handle_gsod_new()


patched <- chillR::patch_daily_temps(weather = murcia$MURCIA, 
                                 patch_weather = aux1, 
                                 vars = c('Tmin', 'Tmax'), 
                                 max_mean_bias = 2, 
                                 max_stdev_bias = 2,
                                 time_interval = '2 weeks')

patched$weather %>% 
  summarise(miss_Tmin = sum(is.na(Tmin)),
            miss_Tmax = sum(is.na(Tmax)))

#5 missing in Tmin
#18 missing in Tmax
#after patching

interpolated <- chillR::fix_weather(patched$weather)

write.csv(interpolated$weather, file = 'data/weather_ready/murcia_clean.csv', 
          row.names = FALSE)




santo_data <- adamedor %>% 
  filter(location == 'Santomera') %>% 
  filter(is.na(flowering_f50) == FALSE) %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n())

santo_data <- adamedor %>% 
  filter(location == 'Santomera') %>% 
  filter(is.na(begin_flowering_f5) == FALSE) %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n())

al_out <- adamedor %>% 
  filter(species == 'Almond') %>% 
  filter(is.na(flowering_f50) == FALSE) %>% 
  group_by(cultivar) %>% 
  summarise(n = n())

#need to redo the fitting of almond, because I have actually spanish data 


ada_f50 <- adamedor %>% 
  filter(is.na(flowering_f50) == FALSE) %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n()) %>% 
  filter(n >= 20)

ada_f5 <- adamedor %>% 
  filter(is.na(begin_flowering_f5) == FALSE) %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n()) %>% 
  filter(n >= 20)
  
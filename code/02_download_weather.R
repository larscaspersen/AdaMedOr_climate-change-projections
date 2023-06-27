library(chillR)
library(tidyverse)

#read weather data of Meknes and Sfax
sfax <- readxl::read_excel('data/weather_raw/Sfax_1973-2021.xlsx')

sfax <- sfax %>% 
  mutate(Tmin = as.numeric(Tmin),
         Tmax = as.numeric(Tmax),
         Tmean = as.numeric(Tmean))

sum(is.na(sfax$Tmean))
sum(is.na(sfax$Tmax))
sum(is.na(sfax$Tmin))

#for now just fill the gaps by linear interpolation
sfax_fixed <- fix_weather(sfax)

sfax_hourly <- chillR::stack_hourly_temps(sfax_fixed$weather, lat = 34.746)

SeasonList_sfax <-  genSeasonList(sfax_hourly$hourtemps, years = 1974:2021)
names(SeasonList_sfax) <- 1974:2021


meknes <- readxl::read_excel('data/weather_raw/Temp Meknes1972-2014.xlsx')
meknes %>%
  reshape2::melt()

#download the weather station bassatine
bassatine <- chillR::handle_gsod(action = 'list_stations',
                                 location = c(-5.55502, 33.88264))

stations <- chillR::handle_gsod(action = 'download_weather',
                                location = bassatine$chillR_code[1],
                                time_interval = c(1970, 2022))

stations <- chillR::handle_gsod(stations)

bassatine <- stations$BASSATINE$weather %>% 
  dplyr::filter(Year >= 1973)
#--> muss adrians script benutzen!
#meknes data is a mess right now

bassatine %>% 
  summarise(complete_tmin = sum(is.na(Tmin)) / length(Tmin),
            complete_tmax = sum(is.na(Tmax)) / length(Tmin),
            complete_tmean = sum(is.na(Tmean)) / length(Tmin))
#7% of the data is missing

stations <- chillR::handle_gsod(action = 'list_stations',
                                 location = c(-5.55502, 33.88264))


#calculate distance 
sp::spDistsN1(as.matrix(data.frame(stations$Long, stations$Lat)), 
              c(stations$Long[1], stations$Lat[1]), longlat = TRUE)

start_year <- 1973
end_year <- 2022
min_overlap <- 5
max_bias <- 3
max_sd <- 3
source('code/utilities/daily_bias.R')

bassatine$source_Tmax <- bassatine$source_Tmin <- NA

for(i in 2:lenth(stations)){
  
  #select a suitable start year
  if(as.numeric(substr(stations$BEGIN[i], start = 1,stop = 4)) < start_year){
    download_start <- start_year
  } else {
    download_start <- as.numeric(substr(stations$BEGIN[i], start = 1,stop = 4))
  }
  
  #select a suitable end year
  if(as.numeric(substr(stations$END[i], start = 1,stop = 4)) > end_year){
    download_end <- end_year
  } else {
    download_end <- as.numeric(substr(stations$END[i], start = 1,stop = 4))
  }
  
  
  aux <- handle_gsod(action = 'download_weather', 
                     location = stations$chillR_code[i], 
                     time_interval = c(download_start, download_end))
  
  if(is.na(aux[[1]]$weather) == TRUE | is.character(aux[[1]]$weather) == TRUE){
    next()
  }
  
  aux <- handle_gsod(aux)
  aux <- aux[[1]]$weather
  
  #merge with bassatine
  
  weather_merged <- merge.data.frame(bassatine, 
                                     aux, 
                                     by = c('DATE', 'Year', 'Month', 'Day'), 
                                     all.x = TRUE, suffixes = c('', '.aux'))
  
  overlap <- weather_merged %>% 
    summarise(overlap_tmin = sum(is.na(Tmin) == FALSE & is.na(Tmin.aux) == FALSE) / 365,
              overlap_tmax = sum(is.na(Tmax) == FALSE & is.na(Tmax.aux) == FALSE) / 365)
  
  if(any(overlap < min_overlap)){
    next()
  }
  
  weather_merged$JDay <- lubridate::yday(weather_merged$DATE)
  
  bias_df <- get_daily_bias_df(weather_merged, window_width = 15)
  
  weather_merged <- merge.data.frame(weather_merged, bias_df, by = 'JDay', all.x = TRUE) %>% 
    arrange(DATE)
    
  #plot(weather_merged$Tmin - weather_merged$Tmin.aux)
  
  
  
  #remove aux observations which violate the conditions of mean and sd bias
  #if condition is fulfilled, correct for bias
  weather_merged$Tmin.aux_corrected <- ifelse(abs(weather_merged$Tmin_mean_bias) > max_bias | weather_merged$Tmin_sd_bias > max_sd, 
                                              yes = NA, 
                                              no = weather_merged$Tmin.aux + weather_merged$Tmin_mean_bias)
  
  weather_merged$Tmax.aux_corrected <- ifelse(abs(weather_merged$Tmax_mean_bias) > max_bias | weather_merged$Tmax_sd_bias > max_sd, 
                                              yes = NA, 
                                              no = weather_merged$Tmax.aux + weather_merged$Tmax_mean_bias)
  
  
  #fill gap in original variable
  Tmin_filled  <- ifelse(is.na(weather_merged$Tmin),
                         yes = weather_merged$Tmin.aux_corrected,
                         no = weather_merged$Tmin)
  
  Tmax_filled  <- ifelse(is.na(weather_merged$Tmax),
                         yes = weather_merged$Tmax.aux_corrected,
                         no = weather_merged$Tmax)
  
  #add information which station lead to the gap filling
  weather_merged$source_Tmin <-  ifelse(is.na(Tmin_filled) == FALSE & is.na(weather_merged$Tmin) == TRUE,
                                        yes = aux_station$chillR_code[j],
                                        no = weather_merged$source_Tmin)
  
  weather_merged$source_Tmax <-  ifelse(is.na(Tmax_filled) == FALSE & is.na(weather_merged$Tmax) == TRUE,
                                        yes = aux_station$chillR_code[j],
                                        no = weather_merged$source_Tmax)
  
  weather_merged$Tmin <- Tmin_filled
  weather_merged$Tmax <- Tmax_filled
  
  #save the gap filled weather station
  bassatine <- weather_merged[,c('DATE', 'Year', 'Month', 'Day', 'Tmin', 'Tmax', 'Tmean', 'Prec', 'source_Tmin', 'source_Tmax')]
  
  if(sum(is.na(bassatine$Tmin)) == 0 & sum(is.na(bassatine$Tmin)) == 0){
    break()
  }
}


bassatine_fixed <- fix_weather(bassatine)

bassatine_fixed_df <- bassatine_fixed$weather

bassatine_fixed_df$source_Tmin <- ifelse(bassatine_fixed_df$no_Tmin == TRUE, yes = 'Linear_Interpolation', no = bassatine_fixed_df$source_Tmin)
bassatine_fixed_df$source_Tmax <- ifelse(bassatine_fixed_df$no_Tmax == TRUE, yes = 'Linear_Interpolation', no = bassatine_fixed_df$source_Tmax)

write.csv(bassatine_fixed_df, file = 'data/meknes-bassatine-fixed_1973-2022.csv', row.names = FALSE)


#bias correct bassatine data with real meknes data
meknes_tmax <- readxl::read_excel('data/weather_raw/Temp Meknes1972-2014.xlsx', 
                             sheet = 'Tmax')

meknes_tmin <- readxl::read_excel('data/weather_raw/Temp Meknes1972-2014.xlsx', 
                                  sheet = 'Tmin')
meknes_tmean <- readxl::read_excel('data/weather_raw/Temp Meknes1972-2014.xlsx', 
                                               sheet = 'Tmoy')

meknes_tmax_long <- reshape2::melt(meknes_tmax, 
                                   id.vars = c('Mois', 'Jour'), 
                                   variable.name = 'Year',
                                   value.name = 'Tmax')

meknes_tmin_long <- reshape2::melt(meknes_tmin, 
                                   id.vars = c('Mois', 'Jour'), 
                                   variable.name = 'Year',
                                   value.name = 'Tmin')

meknes_tmean_long <- reshape2::melt(meknes_tmean, 
                                   id.vars = c('Mois', 'Jour'), 
                                   variable.name = 'Year',
                                   value.name = 'Tmean')



meknes <- meknes_tmin_long %>% 
  merge.data.frame(meknes_tmean_long, by = c('Mois', 'Jour', 'Year')) %>% 
  merge.data.frame(meknes_tmax_long, by = c('Mois', 'Jour', 'Year')) %>% 
  rename(Month = Mois,
         Day = Jour) %>%
  filter(Month %in% 1:12) %>% 
  mutate(Month = as.numeric(Month),
         Year = as.numeric(levels(Year))[Year]) %>% 
  arrange(Year, Month, Day) %>% 
  relocate(Day, Month)

sum(is.na(meknes$Tmin))
#there are some false leap years in the dataset with obviously missing data

meknes <- meknes %>% 
  dplyr::filter(!(Year %% 4 != 0 & Month == 2 & Day == 29))

sum(is.na(meknes$Tmin))
sum(is.na(meknes$Tmax))

write.csv(meknes, 'data/meknes-org_1972-2014.csv', row.names = FALSE)
write.csv(sfax, 'data/sfax-org_1973-2021.csv', row.names = FALSE)


#--> bias correct the period 2015 - 2022 from the other meknes station to have an even longer period
















cieza <- read.csv('data/weather_ready/cieza_clean.csv')

stations <- read.csv('data/weather_ready/weather_station_phenological_observations.csv')
stations <- stations %>% 
  mutate(id = station_name,
         Latitude = latitude,
         Longitude = longitude)


loc <- stations %>% 
  filter(station_name == 'Cieza') %>% 
  dplyr::select(Latitude, Longitude)

possible_stations <- handle_gsod(action = 'list_stations', location = loc)

#download murcia station
murcia <- handle_gsod(action = 'download_weather', 
                      location =  possible_stations$chillR_code[1], 
                      time_interval = c(1973, 2022),
                      station_list = possible_stations)

murcia <- handle_gsod(murcia)

cieza_patched <- patch_daily_temperatures(cieza,murcia$MURCIA$weather)


write.csv(cieza_patched$weather, file = 'data/weather_ready/cieza_clean_patched.csv', row.names = FALSE)






#need to load and recreate all the splits I did in the other scripts, because I forgot to save it


# #need zaragoza and klein altendorf
# cka <- read.csv('data/weather_ready/temp_cka_1958-2022.csv')
# zaragoza <- read.csv('data/weather_raw/temp_zgz_1973-2022.csv')
# sfax <- read.csv('data/weather_ready/sfax_1973-2021_fixed.csv')
# cieza <- readxl::read_xlsx('data/weather_raw/Cieza(95_22)Tmax&Tmin.xlsx')
# meknes <- read.csv('data/weather_ready/meknes-bassatine-fixed_1973-2022.csv')
# 
# #cieza needs to be modified a bit
# cieza <- cieza %>%
#   dplyr::select(-Hours) %>%
#   mutate(Month = lubridate::month(Date),
#          Day = lubridate::day(Date))
# 
# 
# str(cka)
# str(zaragoza)
# str(sfax)
# str(cieza)
# str(meknes)
# 
# #bring it to the same format
# 
# cka_clean <- cka %>%
#   mutate(Date = lubridate::ymd(DATE)) %>%
#   dplyr::select(Date, Year, Month, Day, Tmin, Tmax)
# 
# zaragoza_clean <- zaragoza %>%
#   mutate(Date = lubridate::ymd(paste0(Year, '-', Month, '-', Day))) %>%
#   dplyr::select(Date, Year, Month, Day, Tmin, Tmax)
# 
# sfax_clean <- sfax %>%
#   mutate(Date = lubridate::ymd_hms(DATE)) %>%
#   mutate(Date = as.Date.character(Date)) %>%
#   dplyr::select(Date, Year, Month, Day, Tmin, Tmax)
# 
# cieza_clean <- cieza %>%
#   mutate(Date = as.Date(Date) ) %>%
#   dplyr::select(Date, Year, Month, Day, Tmin, Tmax)
# 
# meknes_clean <- meknes %>%
#   mutate(Date = as.Date(DATE)) %>%
#   dplyr::select(Date, Year, Month, Day, Tmin, Tmax)
# 
# 
# write.csv(sfax_clean, 'data/weather_ready/sfax_clean.csv', row.names = FALSE)
# write.csv(zaragoza_clean, 'data/weather_ready/zaragoza_clean.csv', row.names = FALSE)
# write.csv(cka_clean, 'data/weather_ready/cka_clean.csv', row.names = FALSE)
# write.csv(cieza_clean, 'data/weather_ready/cieza_clean.csv', row.names = FALSE)
# write.csv(meknes_clean, 'data/weather_ready/meknes_clean.csv', row.names = FALSE)
# 
# #make hourly
# coord_zaragoza <- c(41.65, -0.88)
# coord_cka <- c(50.61, 6.99)
# coord_sfax <-c(34.75, 10.75)
# coord_cieza <-c(38.24, -1.41)
# coord_meknes <- c(33.88, -5.54)
# 
# 
# stations_pheno_observations <- data.frame(station_name = c('Zaragoza', 'Klein-Altendorf', 'Sfax', 'Cieza', 'Meknes'),
#            country = c('Spain', 'Germany', 'Tunisia', 'Spain', 'Morocco'),
#            latitude = c(coord_zaragoza[1], coord_cka[1], coord_sfax[1], coord_cieza[1], coord_meknes[1]),
#            longitude = c(coord_zaragoza[2], coord_cka[2], coord_sfax[2], coord_cieza[2], coord_meknes[2]))
# 
# write.csv(stations_pheno_observations, 'data/weather_ready/weather_station_phenological_observations.csv', row.names = FALSE)





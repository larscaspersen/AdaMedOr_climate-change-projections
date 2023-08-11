#look into the pep27 data

pep_stations <-  read.csv('data/PEP725/PEP725_ES_stations.csv', sep = ';')

pep_stations_spatial <- SpatialPointsDataFrame(data.frame(pep_stations$LON, pep_stations$LAT), pep_stations,
                                       proj4string = CRS("+init=epsg:4326"))

library(mapview)

mapview(pep_stations_spatial,
        zcol = c('NAME'))

pep_data <- read.csv('data/PEP725/PEP725_ES_Prunus_amygdalis.csv', sep = ';')

pep_data %>% 
  filter(PEP_ID == '20651', BBCH == 61)
#two observations, 73 and 61 for 10% flowering 


pep_data %>% 
  filter(PEP_ID == '20651', BBCH == 65)
#82, 78, 64 for full bloom

source('code/utilities/handle_gsod_new.R')

w_stations <- handle_gsod_new('list_stations', location = c(pep_stations$LON[66], pep_stations$LAT[66]), time_interval = c(1990, 2021))

daroca_weather <- handle_gsod_new('download_weather', time_interval = c(1990, 2021), location = w_stations$chillR_code[1:2])
#-->  quite empty :(

daroca_weather <- daroca_weather %>% 
  handle_gsod_new(daroca_weather)


calamocha_risk <- daroca_weather$CALAMOCHA %>% 
  na.omit() %>% 
  mutate(doy = lubridate::yday(Date)) %>% 
  group_by(doy) %>% 
  mutate(frost_risk = sum(Tmin <= 0) / n(),
         heat_risk = sum(Tmax >= 31) / n()) %>% 
  ungroup() %>% 
  arrange(doy) %>% 
  mutate(run_mean_frost_risk = runn_mean(frost_risk, runn_mean = 20),
         run_mean_heat_risk = runn_mean(heat_risk, runn_mean = 20))


ggplot(calamocha_risk, aes(x = doy, y = run_mean_frost_risk)) +
  geom_line()

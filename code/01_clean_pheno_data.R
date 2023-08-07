library(tidyverse)
adamedor <- read.csv('data/combined_phenological_data_adamedor.csv', 
                     colClasses="character",na.strings="?")

adamedor$begin_flowering_f5 <- as.Date(paste0(adamedor$begin_flowering_f5,'-', adamedor$year), format = '%d-%b-%Y')
adamedor$flowering_f50 <- as.Date(paste0(adamedor$flowering_f50, '-', adamedor$year), format = '%d-%b-%Y')
adamedor$flowering_f90 <- as.Date(paste0(adamedor$flowering_f90, '-', adamedor$year), format = '%d-%b-%Y')
adamedor$end_flowering <- as.Date(paste0(adamedor$end_flowering, '-', adamedor$year), format = '%d-%b-%Y')
adamedor$ripening <- as.Date(paste0(adamedor$ripening, format = '-', adamedor$year), format = '%d-%b-%Y')

unique_cult <- adamedor %>% 
  group_by(species) %>% 
  summarise(cultivar = unique(cultivar))

#achak and achaak --> Achaak
#angeleno and Angeleno and ANGELENO --> Angeleno
#black amber and BLACK AMBER --> Black Amber
#black diamond and Black Diamond --> Black Diamond
#Black splendor and black splendor, BLACK SPENDOR --> Black Spendor
#desmayo and desmayo l
#EARLIQUEEN and early queen --> Earlyqueen
#Ferragnes and 	Ferragnès --> Ferragnes
#4 types of fortune --> Fortune
#3 types of m golden japan --> Golden Japan
#Larry Ann and Larryanne --> Larry Ann
#Malagueña and Malaguena --> Malaguena
#several types of pioneer --> Pioneer
#	Planeta F. and 	Planeta R.
#	red beauty several types of --> Red Beauty
#Royal Garned and Royal Garnet --> Royal Garnet
#several types of santa rosa --> Santa Rosa

adamedor <- adamedor %>% 
  mutate(cultivar = replace(cultivar, cultivar == 'Achak', 'Achaak')) %>% 
  mutate(cultivar = replace(cultivar, cultivar %in% c('ANGELENO', 'angeleno'), 'Angeleno')) %>% 
  mutate(cultivar = replace(cultivar, cultivar == 'BLACK AMBER', 'Black Amber')) %>% 
  mutate(cultivar = replace(cultivar, cultivar == 'black diamond', 'Black Diamond')) %>% 
  mutate(cultivar = replace(cultivar, cultivar %in% c('black splendor', 'BLACK SPLENDOR', 'Black splendor'), 'Black Splendor')) %>% 
  mutate(cultivar = replace(cultivar, cultivar %in% c('EARLIQUEEN'), 'Early Queen')) %>% 
  mutate(cultivar = replace(cultivar, cultivar == 'Ferragnès', 'Ferragnes')) %>% 
  mutate(cultivar = replace(cultivar, cultivar %in% c('fortune', 'FORTUNE', 'Fortune '), 'Fortune')) %>% 
  mutate(cultivar = replace(cultivar, cultivar %in% c('golden japan', 'GOLDEN JAPAN'), 'Golden Japan')) %>% 
  mutate(cultivar = replace(cultivar, cultivar == 'Larryanne', 'Larry Ann')) %>% 
  mutate(cultivar = replace(cultivar, cultivar == 'Malagueña', 'Malaguena')) %>% 
  mutate(cultivar = replace(cultivar, cultivar %in% c('golden japan', 'GOLDEN JAPAN'), 'Golden Japan')) %>%
  mutate(cultivar = replace(cultivar, cultivar %in% c('pioneer', 'PIONEER'), 'Pioneer')) %>%
  mutate(cultivar = replace(cultivar, cultivar %in% c('red beauty', 'RED BEAUTY'), 'Red Beauty')) %>%  
  mutate(cultivar = replace(cultivar, cultivar == 'Royal Garned', 'Royal Garnet')) %>% 
  mutate(cultivar = replace(cultivar, cultivar %in% c('santa rosa', 'SANTA ROSA', 'Santa Rosa '), 'Santa Rosa'))   
    
write.csv(adamedor, 'data/combined_phenological_data_adamedor_clean.csv', row.names = FALSE)

adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')
unique(adamedor$location)

adamedor <- adamedor %>% 
  mutate(location = replace(location, location == 'Zaragoza/Spain', 'Zaragoza')) %>% 
  mutate(location = replace(location, location %in% c('Cieza (Murcia)', 'Cieza/Spain'), 'Cieza')) %>% 
  mutate(location = replace(location, location %in% c('Mornag/Tunisia'), 'Mornag')) %>% 
  mutate(location = replace(location, location %in% c('Archena (Murcia)'), 'Archena')) %>% 
  mutate(location = replace(location, location %in% c('Klein-Altendorf/Germany'), 'Klein-Altendorf')) %>% 
  mutate(location = replace(location, location %in% c("Santomera, Murcia"), "Santomera")) %>% 
  mutate(location = replace(location, location %in% c("Meknes, Morocco"), "Meknes")) %>% 
  mutate(location = replace(location, location %in% c("Sfax/Tunisia"), "Sfax")) %>% 
  mutate(location = replace(location, location %in% c("Tassaout, Morocco"), "Tassaout")) %>% 
  mutate(location = replace(location, location %in% c("Khenifra, Morocco"), "Khenifra"))

adamedor$country <- ifelse(adamedor$location %in% c('Zaragoza', 'Cieza', 'Archena', 'Santomera'), yes = 'Spain', 
                           no = NA)
adamedor$country <- ifelse(adamedor$location %in% c('Mornag', 'Sfax'), yes = 'Tunisia', 
                           no = adamedor$country)
adamedor$country <- ifelse(adamedor$location %in% c('Meknes', 'Tassaout', 'Khenifra'), yes = 'Morocco', 
                           no = adamedor$country)
adamedor$country <- ifelse(adamedor$location %in% c('Klein-Altendorf'), yes = 'Germany', 
                           no = adamedor$country)




adamedor_old <- read.csv('data/combined_phenological_data_adamedor.csv', 
                         colClasses="character",na.strings="?")

adamedor_transformed <- adamedor_old %>% 
  filter(species == 'Pistachio') %>% 
  mutate(begin_flowering_f5 = lubridate::dmy(begin_flowering_f5),
         flowering_f50 = lubridate::dmy(flowering_f50),
         flowering_f90 = lubridate::dmy(flowering_f90))

#only contains data from sfax
adamedor_transformed <- adamedor_transformed %>% 
  mutate(location = 'Sfax',
         country = 'Tunisia',
         begin_flowering_f5 = as.Date(begin_flowering_f5, format = '%Y-%m-%d'),
         flowering_f50 = as.Date(flowering_f50, format = '%Y-%m-%d'),
         flowering_f90 = as.Date(flowering_f90, format = '%Y-%m-%d'))


#kick out pistachio data from adamedor and replace with adamedor transformed
adamedor_test <- adamedor %>% 
  filter(species != 'Pistachio') %>% 
  mutate(begin_flowering_f5 = lubridate::ymd(begin_flowering_f5),
         flowering_f50 = lubridate::ymd(flowering_f50),
         flowering_f90 = lubridate::ymd(flowering_f90)) %>% 
  rbind.data.frame(adamedor_transformed)


#there are redundancies in the almond data of santomera
adamedor_old <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

#only contains almond data

santomera <- adamedor_old %>% 
  filter(location == 'Santomera') %>% 
  split(f = ~ species  + cultivar) %>% 
  purrr::map(function(x){ #remove duplicated years, always take the first reading
    x[duplicated(x$year) == FALSE,]
  }) %>% 
  bind_rows()


adamedor <- adamedor_old %>% 
  filter(location != 'Santomera') %>% 
  rbind(santomera)


write.csv(adamedor, 'data/combined_phenological_data_adamedor_clean.csv', row.names = FALSE)

library(tidyverse)
library(ggplot2)
library(readr)
library(chillR)
library(LarsChill)

### Load current data

stations <- read.csv("data/combined_phenological_data_adamedor_clean.csv", fileEncoding="ISO-8859-1" ) %>% 
  as.data.frame()

Flowering <- plyr::ddply(filter(stations, location %in% c("Meknes","Sfax","Zaragoza")), 
                         ~species+cultivar+location, 
                         summarise, 
                         Mean=mean(yday(flowering_f50), na.rm=T),
                         SD=sd(yday(flowering_f50), na.rm=T),
                         Max_range= (yday(flowering_f50)-mean(yday(flowering_f50), na.rm=T)) %>% 
                           abs() %>% 
                           max()*1.5
                           )

### Load future estimations

load("data/Results_almond_future.RData")
Almond <- Results_almond %>% dplyr::mutate(., Species = "Almond", .before="cultivar")
rm(Results_almond)

load("data/Results_apricot_future.RData")
Apricot <- Results_apricot %>% dplyr::mutate(., Species = "Apricot", .before="cultivar")
rm(Results_apricot)

#### To be continued...

Full_fut <- rbind.data.frame(Almond, Apricot)

# Future <- Full_fut %>%
#   plyr::ddply(., ~Species+cultivar+Location+Scenario+Year,
#         summarise,
#         Mean_F=mean(predicted, na.rm=TRUE),
#         SD_F=sd(predicted, na.rm=TRUE))

Future <- Full_fut %>%
  group_by(Species, cultivar, Location, Scenario, Year) %>%
  summarise(Mean_F=mean(predicted, na.rm=TRUE),
         SD_F=sd(predicted, na.rm=TRUE),
         Numb=n())


colnames(Flowering)[1:3] <- colnames(Future)[1:3]


Changes <- merge(Full_fut, Flowering, by= c("Species", "cultivar", "Location"), all.x=TRUE) %>%
  mutate(., Dif=abs(predicted-Mean)) %>% 
  filter(., Dif>Max_range) %>%
  group_by(Species, cultivar, Location, Scenario, Year) %>%
  count()

Risk <- merge(Future, Changes, by=c("Species", "cultivar", "Location", "Year", "Scenario"), all.x=TRUE)

## Replace the NAs by 0 to avoid problems
Risk[which(is.na(Risk$n)),"n"]<-0

Risk <- Risk %>%
  mutate(N_misw_FW=(n/Numb)*100, .before="Numb") %>%
  .[,c(1:8)]





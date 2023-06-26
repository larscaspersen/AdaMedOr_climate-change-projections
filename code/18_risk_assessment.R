library(tidyverse)
library(ggplot2)
library(readr)
library(chillR)
library(LarsChill)

### Load observed data

stations <- read.csv("data/combined_phenological_data_adamedor_clean.csv", fileEncoding="ISO-8859-1" ) %>% 
  as.data.frame()
## Pistachio did not have data for Sfax in the "clean" file. I suspect it is because it has a different date format 
stations_pistachio <- read.csv("data/combined_phenological_data_adamedor.csv", fileEncoding="ISO-8859-1" ) %>% 
  as.data.frame() %>% filter(species=="Pistachio")
flowering_pistachio <- stations_pistachio$flowering_f50 %>% as.Date(format = "%d/%m/%y") %>% as.character()
stations[which(stations$species=="Pistachio"), "flowering_f50"]<-flowering_pistachio

stations$flowering_f50 <- yday(as.Date(stations$flowering_f50))


Flowering <- stations %>%
  filter(location %in% c("Meknes","Sfax","Zaragoza")) %>%
  group_by(species, cultivar, location) %>%
  summarise(Mean=mean(flowering_f50, na.rm=T),
            SD=sd(flowering_f50, na.rm=T),
            Max_range= max(abs(flowering_f50-mean(flowering_f50, na.rm=T)), na.rm=TRUE)*1.5
            )

### Load future estimations

load("data/Results_almond_future.RData")
Almond <- Results_almond %>% dplyr::mutate(., Species = "Almond", .before="cultivar") %>% unique()
rm(Results_almond)

load("data/Results_apricot_future.RData")
Apricot <- Results_apricot %>% dplyr::mutate(., Species = "Apricot", .before="cultivar") %>% unique()
rm(Results_apricot)

load("data/Results_sweet_cherry_future.RData")
Sweet_cherry <- Results_sweet_cherry %>% dplyr::mutate(., Species = "Sweet Cherry", .before="cultivar") %>% unique()
rm(Results_sweet_cherry)


load("data/Results_pear_future.RData")
Pear <- Results_pear %>% dplyr::mutate(., Species = "Pear", .before="cultivar") %>% unique()
rm(Results_pear)

load("data/Results_pistachio_future.RData")
Pistachio <- Results_pistachio %>% dplyr::mutate(., Species = "Pistachio", .before="cultivar") %>% unique()
rm(Results_pistachio)

Full_fut <- rbind.data.frame(Almond, Apricot, Sweet_cherry, Pear, Pistachio)
#################################

## Future estimations of flowering dates

Future <- Full_fut %>%
  group_by(Species, cultivar, Location, Scenario, Year, Model) %>%
  summarise(Mean_F=mean(predicted, na.rm=TRUE),
         SD_F=sd(predicted, na.rm=TRUE),
         Numb=n())


colnames(Flowering)[1:3] <- colnames(Future)[1:3]


Changes <- merge(Full_fut, Flowering, by= c("Species", "cultivar", "Location"), all.x=TRUE) %>%
  mutate(., Dif=abs(predicted-Mean)) %>% 
  filter(., Dif>Max_range) %>%
  group_by(Species, cultivar, Location, Scenario, Year, Model) %>%
  count()

# Clean_data <- merge(Full_fut, Flowering, by= c("Species", "cultivar", "Location"), all.x=TRUE) %>%
#   mutate(., Dif=abs(predicted-Mean)) %>%
#   filter(., Dif<=Max_range)
# 
# Clean_future <- Clean_data %>%
#   group_by(Species, cultivar, Location, Scenario, Year, Model) %>%
#   summarise(Mean_F=mean(predicted, na.rm=TRUE),
#             SD_F=sd(predicted, na.rm=TRUE),
#             Numb=n())

Risk <- merge(Future, Changes, by=c("Species", "cultivar", "Location", "Year", "Scenario", "Model"), all.x=TRUE)

Risk[is.na(Risk$n), "n"]<-0

Risk <- Risk %>%
  mutate(N_miss_FW=101-Numb+n, .before="Numb")


Risk <- merge(Risk, Flowering, by=c("Species", "cultivar", "Location"), all.x=TRUE) %>%
  mutate(Fw_change=abs(Mean_F - Mean),
         SD_change=abs(SD_F - SD))


### Debug code, not important
# Sp="Almond"
# Cul="Avola"
# Loc="Sfax"
# Yr=2050
# Scen="ssp585"

# Risk$Signif.change <- apply(Risk, 1, function(x){
#   Sp=x[1]
#   Cul=x[2]
#   Loc=x[3]
#   Yr=x[4]
#   Scen=x[5]
#   Past_pop <- filter(stations, species==Sp & cultivar==Cul & location==Loc) %>%
#     dplyr::select(species, cultivar, location, flowering_f50) %>%
#     na.omit() %>%
#     mutate(Fw_past=yday(flowering_f50)) %>% 
#     .[,"Fw_past"]
#   if(length(Past_pop)>0){
#   Future_pop <- filter(Full_fut, Species == Sp & cultivar == Cul & Location == Loc & Year == Yr & Scenario == Scen) %>%
#     group_by(Model) %>%
#     summarise(Wilcox = wilcox.test(predicted, y = unlist(Past_pop))$p.value) %>%
#     pull(Wilcox)
#   Prop_sig <- length(which(Future_pop<=0.05)) / length(Future_pop)}else{Prop_sig <- NA}
# 
#   return(Prop_sig)
# })

Risk$Signif.change <- apply(Risk, 1, function(x){
  Sp=x[1]
  Cul=x[2]
  Loc=x[3]
  Yr=x[4]
  Scen=x[5]
  Mod=x[6]
  Past_pop <- filter(stations, species==Sp & cultivar==Cul & location==Loc) %>%
    dplyr::select(species, cultivar, location, flowering_f50) %>%
    na.omit() %>%
    mutate(Fw_past=flowering_f50) %>% 
    .[,"Fw_past"]
  if(length(Past_pop)>0){
    Future_pop <- filter(Full_fut, Species == Sp & cultivar == Cul & Location == Loc & Year == Yr & Scenario == Scen & Model == Mod) %>%
      pull("predicted") %>%
      wilcox.test(., y = unlist(Past_pop)) %>% .$p.value
    Prop_sig <- Future_pop}else{Prop_sig <- NA}
  
  return(Prop_sig)
})

# save(Risk, file="data/preliminary_risk.RData")

load("data/preliminary_risk.RData")

Risk$Pond_FW_change <- Risk$Fw_change * (Risk$N_miss_FW/101)

Final_risk <- data.frame(
  Risk[,1:6],
  FW_change = scales::rescale(Risk$Pond_FW_change),
  FW_variab = scales::rescale(Risk$SD_change),
  Sig = ifelse(Risk$Signif.change<=0.05, 1, 0)
)

Final_risk <- Final_risk %>%
  mutate(TOTAL=FW_change+ FW_variab)


ggplot(filter(Final_risk, Year==2050), aes(y=reorder(cultivar, TOTAL), x=TOTAL, fill=Scenario))+
  geom_boxplot()+
  facet_grid(rows=vars(Species), cols = vars(Location), scales = "free")
  # scale_x_continuous(limits = c(0,3))

jpeg("data/risk_plot_v1_2050.jpeg", width = 10, height = 20, units = "in", res = 300)
ggplot(filter(Final_risk, Year==2050), aes(y=reorder(cultivar, TOTAL), x=TOTAL, fill=cultivar))+
  geom_boxplot()+
  labs(title="2050")+
  facet_grid(rows=vars(Species), cols = vars(Location), scales = "free")+
  theme_bw()+
  theme(legend.position = "none")
dev.off()


jpeg("data/risk_plot_v1_2085.jpeg", width = 10, height = 20, units = "in", res = 300)
ggplot(filter(Final_risk, Year==2085), aes(y=reorder(cultivar, TOTAL), x=TOTAL, fill=cultivar))+
  geom_boxplot()+
  labs(title="2085")+
  facet_grid(rows=vars(Species), cols = vars(Location), scales = "free")+
  theme_bw()+
  theme(legend.position = "none")
dev.off()


### Debug code, not relevant
# Prop_sig <- c()
# for(a in 1:nrow(Risk)){
#   x <- Risk[a,]
#   
#   Sp=x[1,1]
#   Cul=x[1,2]
#   Loc=x[1,3]
#   Yr=x[1,4]
#   Scen=x[1,5]
#   Past_pop <- filter(stations, species==Sp & cultivar==Cul & location==Loc) %>%
#     dplyr::select(species, cultivar, location, flowering_f50) %>%
#     na.omit() %>%
#     mutate(Fw_past=yday(flowering_f50)) %>% 
#     .[,"Fw_past"]
#   if(length(Past_pop)>0){
#   Future_pop <- filter(Full_fut, Species == Sp & cultivar == Cul & Location == Loc & Year == Yr & Scenario == Scen) %>%
#     group_by(Model) %>%
#     summarise(Wilcox = wilcox.test(predicted, y = unlist(Past_pop))$p.value) %>%
#     pull(Wilcox)
#   Newrow <- length(which(Future_pop<=0.05)) / length(Future_pop)}else{Newrow<-NA}
# 
#   Prop_sig <- c(Prop_sig, Newrow)
# }

Risk_frame <- Risk %>%
  mutate(Norm_FW_dif=Fw_change/max(Risk$Fw_change))

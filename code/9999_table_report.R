library(tidyverse)

adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv') %>% 
  select(species, cultivar, location, year, begin_flowering_f5, flowering_f50, 
         end_flowering, ripening, rootstock, 
         plantation_framework, training_system, irrigation_system) %>% 
  pivot_longer(cols = c('begin_flowering_f5', 'flowering_f50', 
                        'end_flowering', 'ripening'), names_to = 'pheno_stage') %>% 
  drop_na(value)

adamedor_summarized <- adamedor %>% 
  group_by(species, cultivar, location, pheno_stage) %>% 
  summarise(n = n(),
            min_year = min(year),
            max_year = max(year)) %>% 
  mutate(pheno_stage = factor(pheno_stage, 
                              levels = c('begin_flowering_f5', 'flowering_f50', 
                                                      'end_flowering', 'ripening'),
                              labels = c('Begin Flowering', 'Flowering', 'End Flowering', 'Ripening')),
         source = recode(location, 
                         `Klein-Altendorf` = 'Uni Bonn',
                         Zaragoza = 'CITA',
                         Cieza = 'CEBAS-CSIC',
                         Mornag = 'INAT',
                         Archena = 'CEBAS-CSIC',
                         Meknes = 'USMS',
                         Sfax = 'INAT',
                         Tassaout = 'ENA',
                         Khenifra = 'ENA',
                         Santomera = 'CEBAS-CSIC'),
         period = paste0(min_year, ' - ', max_year)) %>% 
  relocate(species, cultivar, location, source, pheno_stage, period, n)

adamedor_summarized_v2 <- adamedor_summarized %>% 
  group_by(species, source, pheno_stage) %>% 
  summarise(med_length = median(n),
            n = sum(n),
            n_cult = length(unique(cultivar)),
            min_year = min(min_year),
            max_year = max(max_year)) %>% 
  mutate(period = paste0(min_year, ' - ', max_year)) %>% 
  select(-max_year, -min_year) %>% 
  relocate(species, source, pheno_stage, n, n_cult, med_length, period)
  
colnames(adamedor_summarized) <- c('Species', 'Cultivar', 'Location', 'Source', 'Phenological Stage', 'Period', 'n Observations')
colnames(adamedor_summarized_v2) <- c('Species', 'Source', 'Phenological Stage', 'Total Observations', 'Number Cultivars', 'Median Number Seasons per Cultivar', 'Period')


write.csv(adamedor_summarized, file = 'data/long_term_phenology_summarized.csv', row.names = FALSE)
write.csv(adamedor_summarized_v2, file = 'data/long_term_phenology_summarized_v2.csv', row.names = FALSE)


#who is the dataprovider / source depending on the location?
#CKA --> Bonn
#Zaragoza --> CITA
#Santomera, Cieza, Archena --> CEBAS-CSIC
#Meknes --> USMS
#Khenifra --> ENA
#Sfax --> INAT
#Mornag --> INAT?
#Tassaout --> ENA?


adamedor$location %>%  unique()

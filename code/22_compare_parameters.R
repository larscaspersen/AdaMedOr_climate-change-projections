#compare parameters of cultivars fitted by different locations
library(tidyverse)
library(ggplot2)
#library(readr)
library(chillR)
#library(devtools)
# install_github("larscaspersen/addition_chillR")
library(LarsChill)

### Read all the cultivars located in each single location

#These two parameters are fixed for all the models
Tc = 36
theta_star = 279

#load performance file for weightening of ensemble predictions
performance_df <- read.csv('data/performance_fitted_models.csv')

## Load all the fitting results from the 10 repetitions
apricot_fit <- apple_fit <- eplum_fit <- jplum_fit <- pistachio_fit <- cherry_fit <- almond_fit <- pear_fit <-  list()

for(i in 1:10){
  apricot_fit[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting_clean/', prefix = paste0('repeat', i, '_'))
  apple_fit[[i]]  <- load_fitting_result('data/fitting/apple/', prefix = paste0('repeat', i, '_'))
  eplum_fit[[i]] <- load_fitting_result('data/fitting/european_plum/', prefix = paste0('repeat', i, '_'))
  jplum_fit[[i]] <- load_fitting_result('data/fitting/japanese_plum/', prefix = paste0('repeat', i, '_'))
  pistachio_fit[[i]] <- load_fitting_result('data/fitting/pistachio/', prefix = paste0('repeat', i, '_'))
  cherry_fit[[i]] <- load_fitting_result('data/fitting/sweet_cherry/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  #almond_fit[[i]]  <- load_fitting_result(path = 'data/fitting/almond/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  #almond_fit[[i]] <- load_fitting_result(path = 'data/fitting/almond/repeated_fitting_new_bounds', prefix = paste0('repeat', i, '_'))
  almond_fit[[i]]  <- load_fitting_result(path = 'data/fitting/almond/repeated_fitting_santomera_v2_cleanly_saved/', prefix = paste0('repeat', i, '_'))
  pear_fit[[i]] <- load_fitting_result('data/fitting/pear/', prefix = paste0('repeat', i, '_'))
}



fit_list_combined <- list('Apricot' = apricot_fit,
                          'Apple' = apple_fit,
                          'European Plum' = eplum_fit,
                          'Japanese Plum' = jplum_fit,
                          'Pistachio' = pistachio_fit,
                          'Sweet Cherry' = cherry_fit,
                          'Almond' = almond_fit,
                          'Pear' = pear_fit)


#master pheno
master_fitting_data <- read.csv('data/master_phenology_repeated_splits.csv') %>% 
  filter(repetition == 1) %>% 
  mutate(species = gsub('Japanese Plum', 'European Plum', species)) %>% 
  group_by(species, cultivar) %>% 
  summarize(loc = unique(location)) %>% 
  mutate(spec_cult_loc = paste(species, cultivar, loc, sep = '_'),
         species_location = paste(species, loc, sep = '_'))


#extract the parameters and save them to a data.frame

adamedor <-read.csv('data/combined_phenological_data_adamedor_clean.csv') 

f50_cult <- adamedor %>% 
  dplyr::filter(species %in% c(c('Sweet Cherry', 'Pistachio', 'Pear', 'Apricot', 'Almond'))) %>% 
  dplyr::select(species, cultivar, location, year, flowering_f50) %>% 
  stats::na.omit() %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n()) %>% 
  dplyr::filter(n >= 20) %>% 
  dplyr::pull(cultivar)


f5_cult <- adamedor %>% 
  dplyr::filter(species %in% c(c('Apple', 'European plum', 'Japanese plum'))) %>% 
  dplyr::select(species, cultivar, location, year, begin_flowering_f5) %>% 
  stats::na.omit() %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n()) %>% 
  dplyr::filter(n >= 20 )%>% 
  dplyr::pull(cultivar)

cultivar_n <- c(f50_cult, f5_cult)


par_df <- purrr::map(fit_list_combined, function(species){
  
  purrr::map(species, function(repetition){
    
    purrr::map(repetition, 'xbest') %>% 
    bind_rows() %>% 
      mutate(parameter = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope')) %>% 
      pivot_longer(cols = -'parameter', names_to = 'cultivar')
    
  }) %>% 
    bind_rows(.id = 'repetition')
  
}) %>% 
  bind_rows(.id = 'species')%>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum')) %>% 
  filter(cultivar %in% cultivar_n)

rm(almond_fit, apricot_fit, apple_fit, pear_fit, cherry_fit, pistachio_fit, eplum_fit, fit_list_combined, jplum_fit)

#add infomation from which location the cultivar has data


par_df$species_cultivar <- paste(par_df$species, par_df$cultivar, sep ='_')

master_fitting_data2 <- master_fitting_data %>% 
  group_by(species, cultivar) %>% 
  summarise(loc_all = paste(unique(loc), collapse = '_')) %>% 
  filter(cultivar %in% cultivar_n)

library(ggpubr)
library(rstatix)


#----------------------------#
#boxplot with statistical test of differences in parameters by species
#-----------------------------#

#apple
y <- 1:100
yc <- 80
s1 <- 0.9

sy = exp(s1*yc*((y-yc)/y))
py = sy / (sy+1)
plot(py)


par_prep <- par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  mutate(loc_all = recode(loc_all, `Klein-Altendorf` = 'CKA', `Klein-Altendorf_Meknes` = 'CKA - Meknes'),
         loc_all = factor(loc_all)) %>% 
  filter(species == 'Apple')

par_prep %>%
  mutate(values_adj = ifelse(value == max(value), value + 20, value), .by = parameter,
         parameter = factor(parameter, levels = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope'))) %>% 
  ggplot(aes(x = loc_all, y = values_adj)) +
  geom_boxplot() +
  stat_compare_means(method = "wilcox.test", aes(label = ..p.signif.., y = value), size = 5, 
                     comparisons = list(c("CKA" , "CKA - Meknes"))) +
  facet_wrap( ~ parameter, scales = "free_y")+
  scale_y_continuous(expand = c(.1, .1))

#-----#

#pear

par_prep <- par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species == 'Pear') %>% 
  mutate(loc_all = recode(loc_all, `Klein-Altendorf` = 'CKA'),
         loc_all = factor(loc_all)) 


par_prep %>%
  mutate(values_adj = ifelse(value == max(value), value + 20, value), .by = parameter,
         parameter = factor(parameter, levels = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope'))) %>% 
  ggplot(aes(x = loc_all, y = values_adj)) +
  geom_boxplot() +
  stat_compare_means(method = "wilcox.test", aes(label = ..p.signif.., y = value), size = 5, 
                     comparisons = list(c("CKA" , "Zaragoza"))) +
  facet_wrap( ~ parameter, scales = "free_y")+
  scale_y_continuous(expand = c(.1, .1))


#-----#

#sweet cherry

par_prep <- par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species == 'Sweet Cherry') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = factor(loc_all)) 



par_prep %>%
  mutate(values_adj = ifelse(value == max(value), value + 20, value), .by = parameter,
         parameter = factor(parameter, levels = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope'))) %>% 
  ggplot(aes(x = loc_all, y = values_adj)) +
  geom_boxplot() +
  stat_compare_means(method = "wilcox.test", aes(label = ..p.signif.., y = value), size = 5, 
                     comparisons = list(c("CKA" , "CKA_Zaragoza"),c("CKA_Zaragoza", 'Zaragoza'),  c("CKA" , "Zaragoza"))) +
  facet_wrap( ~ parameter, scales = "free_y")+
  scale_y_continuous(expand = c(.1, .1))



#-----#
#apricot

par_prep <- par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species == 'Apricot') %>% 
  mutate(loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         loc_all = factor(loc_all, levels = c('Zaragoza', 'Zaragoza_Cieza', 'Cieza'))) 

par_prep %>%
  mutate(values_adj = ifelse(value == max(value), value + 20, value), .by = parameter,
         parameter = factor(parameter, levels = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope'))) %>% 
  ggplot(aes(x = loc_all, y = values_adj)) +
  geom_boxplot() +
  stat_compare_means(method = "wilcox.test", aes(label = ..p.signif.., y = value), size = 5, 
                     comparisons = list(c("Zaragoza" , "Zaragoza_Cieza"),c("Zaragoza_Cieza", 'Cieza'),  c("Zaragoza", 'Cieza'))) +
  facet_wrap( ~ parameter, scales = "free_y")+
  scale_y_continuous(expand = c(.1, .1))



#-----#
#almond

par_prep <- par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species == 'Almond') %>% 
  mutate(loc_all = factor(loc_all, levels = c('Meknes_Santomera', 'Santomera', 'Meknes_Santomera_Sfax', 'Santomera_Sfax', 'Sfax')))

comb_df <- expand.grid(c('Meknes_Santomera', 'Santomera', 'Meknes_Santomera_Sfax', 'Santomera_Sfax', 'Sfax'), 
            c('Meknes_Santomera', 'Santomera', 'Meknes_Santomera_Sfax', 'Santomera_Sfax', 'Sfax')) %>% 
  filter(Var1 != Var2) %>% 
  mutate(comb = apply(., MARGIN = 1, function(i){
    unlist(i) %>% 
      sort() %>% 
      paste(collapse = '-')
  }),
  un = duplicated(comb)) %>% 
  filter(un == FALSE)

combinations <- purrr::map(1:nrow(comb_df), function(i) as.character(c(comb_df$Var1[i], comb_df$Var2[i])))


par_prep %>%
  mutate(values_adj = ifelse(value == max(value), value + 20, value), .by = parameter,
         parameter = factor(parameter, levels = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope'))) %>% 
  ggplot(aes(x = loc_all, y = values_adj)) +
  geom_boxplot() +
  stat_compare_means(method = "wilcox.test", aes(label = ..p.signif.., y = value), size = 5, 
                     comparisons = combinations) +
  facet_wrap( ~ parameter, scales = "free_y")+
  scale_y_continuous(expand = c(.1, .1))






#calculate the average temperature for these location in winter, assume equal contribution in mixtures
temp_list <- list('Zaragoza' = zaragoza <- read.csv('data/weather_raw/temp_zgz_1973-2022.csv'),
                  'CKA' = cka <- read.csv('data/weather_ready/temp_cka_1958-2022.csv'),
                  'Cieza' = cieza <- readxl::read_xlsx('data/weather_raw/Cieza(95_22)Tmax&Tmin.xlsx'),
                  'Sfax' = sfax <- read.csv('data/weather_ready/sfax_1973-2021_fixed.csv'),
                  'Meknes' = meknes <- read.csv('data/weather_ready/meknes-bassatine-fixed_1973-2022.csv'),
                  'Santomera' =  santomera <-read.csv('data/weather_ready/murcia_clean.csv'))

tmeans <- purrr::map(temp_list, function(temp){
   if('Month' %in% colnames(temp) == FALSE){
     temp$Month <- lubridate::month(temp$Date)
   }
  
  temp %>% 
    filter(Month %in% c(11, 12, 1, 2, 3, 4),
           Year > 1990 & Year <= 2020) %>%
    mutate(Tmean = (Tmin + Tmax) / 2) %>% 
    summarise(Tmean = mean(Tmean)) %>% 
    pull(Tmean)
}) %>% 
  bind_rows(.id = 'Location') %>% 
  mutate(`CKA_Meknes` = (CKA + Meknes) / 2,
         `Zaragoza_Cieza` = (Zaragoza + Cieza) / 2,
         `CKA_Zaragoza` = (CKA + Zaragoza) / 2,
         `Meknes_Santomera` = (Meknes + Santomera) / 2,
         `Meknes_Santomera_Sfax` = (Meknes + Santomera + Sfax) / 3,
         `Santomera_Sfax` = (Santomera + Sfax) / 2) %>% 
  pivot_longer(cols = everything(), names_to = 'location') %>% 
  arrange(value)


#plot the parameters together spitted by species
par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species %in% c('Pistachio', 'European Plum') == FALSE,
         parameter == 'yc') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'Almond')),
         loc_all = factor(loc_all, levels = tmeans$location)) %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  ggtitle('yc') +
  facet_wrap(~species, scales = 'free_x')

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species %in% c('Pistachio', 'European Plum') == FALSE,
         parameter == 'zc') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'Almond')),
         loc_all = factor(loc_all, levels = tmeans$location)) %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  ggtitle('zc') +
  facet_wrap(~species, scales = 'free_x')

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species %in% c('Pistachio', 'European Plum') == FALSE,
         parameter == 'pie_c') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'Almond')),
         loc_all = factor(loc_all, levels = tmeans$location)) %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  ggtitle('pie_c') +
  facet_wrap(~species, scales = 'free_x')
#higher pie_c means that it takes longer to negate labile accumulated chill

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species %in% c('Pistachio', 'European Plum') == FALSE,
         parameter == 'Tf') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'Almond')),
         loc_all = factor(loc_all, levels = tmeans$location)) %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  ggtitle('Tf') +
  facet_wrap(~species, scales = 'free_x')
#Tf affects the conversion of labile to stabile chill
#higher Tf means that the infliction point of the conversion is at higher temperatures



par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species %in% c('Pistachio', 'European Plum') == FALSE,
         parameter == 's1') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'Almond')),
         loc_all = factor(loc_all, levels = tmeans$location)) %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  ggtitle('s1') +
  facet_wrap(~species, scales = 'free_x')


par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species %in% c('Pistachio', 'European Plum') == FALSE,
         parameter == 'tau') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'Almond')),
         loc_all = factor(loc_all, levels = tmeans$location)) %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  ggtitle('tau') +
  facet_wrap(~species, scales = 'free_x')

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species %in% c('Pistachio', 'European Plum') == FALSE,
         parameter == 'theta_c') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'Almond')),
         loc_all = factor(loc_all, levels = tmeans$location)) %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  ggtitle('theta_c') +
  facet_wrap(~species, scales = 'free_x')
#more species specific than location specific

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species %in% c('Pistachio', 'European Plum') == FALSE,
         parameter == 'Tb') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'Almond')),
         loc_all = factor(loc_all, levels = tmeans$location)) %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  ggtitle('Tb') +
  facet_wrap(~species, scales = 'free_x')

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species %in% c('Pistachio', 'European Plum') == FALSE,
         parameter == 'Tu') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'Almond')),
         loc_all = factor(loc_all, levels = tmeans$location)) %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  ggtitle('Tu') +
  facet_wrap(~species, scales = 'free_x')

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species %in% c('Pistachio', 'European Plum') == FALSE,
         parameter == 'slope') %>% 
  mutate(loc_all = gsub('Klein-Altendorf', 'CKA', loc_all),
         loc_all = recode(loc_all, `Cieza_Zaragoza` = 'Zaragoza_Cieza'),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'Almond')),
         loc_all = factor(loc_all, levels = tmeans$location)) %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  ggtitle('slope') +
  facet_wrap(~species, scales = 'free_x')







sort(c('B', 'A'))



stat.test <- par_prep %>%
  group_by(parameter) %>%
  t_test(value ~ loc_all, ref.group = 'CKA') %>% 
  adjust_pvalue() %>% 
  add_xy_position(x = "loc_all", group = 'parameter') %>% 
  mutate(xmin = 1,
         xmax = 2)
  
ggboxplot(par_prep, x = "loc_all", y = "value", facet.by = "parameter", scales = 'free_y') +
  stat_pvalue_manual(stat.test, label = 'p.adj', )


my_comparisons <- list( c("CKA", "CKA - Meknes"))
par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  mutate(loc_all = recode(loc_all, `Klein-Altendorf` = 'CKA', `Klein-Altendorf_Meknes` = 'CKA - Meknes')) %>% 
  filter(species == 'Apple') %>% 
  ggboxplot(x = 'loc_all', y = 'value', facet.by = 'parameter', scales = 'free_y') +
  # stat_compare_means(label = "p.format")
  stat_compare_means(comparisons = my_comparisons, label = 'p.signif')

facet(p, facet.by = 'parameter', scales = 'free_y')
  geom_boxplot() +
  facet_wrap(~parameter, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species == 'Apple') %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  facet_wrap(~parameter, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#higher yc, higher piec, higher s1, higher tb in CKA comapred to shared fitting

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species == 'Almond') %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  facet_wrap(~parameter, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species == 'Sweet Cherry') %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  facet_wrap(~parameter, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  filter(species == 'Pear') %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  facet_wrap(~parameter, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

par_df %>% 
  merge(master_fitting_data2, by = c('species', 'cultivar')) %>% 
  mutate(loc_all = recode(loc_all, Cieza_Zaragoza = 'Zaragoza_Cieza'),
         loc_all = factor(loc_all, levels = c('Zaragoza', 'Zaragoza_Cieza', 'Cieza'))) %>% 
  filter(species == 'Apricot') %>% 
  ggplot(aes(x = loc_all, y = value)) +
  geom_boxplot() +
  facet_wrap(~parameter, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






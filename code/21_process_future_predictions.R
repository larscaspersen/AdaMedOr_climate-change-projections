#----------------------------------#
#change bloom date
#----------------------------------#

library(chillR)
library(LarsChill)
library(tidyverse)


#get the name of the cultivars considered in this study

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



#read the predictions for 2015
pheno_2015 <- read.csv('data/projected_bloomdates_ensemble_historic_scenarios.csv') %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum')) %>% 
  filter(cultivar %in% cultivar_n)

#read the validation performance
performance <- read.csv('data/performance_fitted_models.csv') %>% 
  filter(cultivar %in% cultivar_n, 
         split == 'Validation')





# #read the different batches of the future bloom dates
# f <- list.files('data/predictions_future_v2/', full.names = TRUE) %>% 
#   purrr::map(read.csv, .progress = TRUE) %>% 
#   do.call(rbind, .) 
# 
# 
# 
# prepare_conf <- function(m1, conf,  NA_threshold = 5){
#   apply(m1, MARGIN = 1, FUN = function(x){
#     if(sum(is.na(x)) >= NA_threshold){
#       return(rep(NA, length(x)))
#     } 
#     
#     if(any(is.na(x))){
#       conf_adjust <- conf
#       conf_adjust[is.na(x)] <- 0
#       
#       #now give the rescaled values of confidence between 0 and 1
#       conf_adjust <- conf_adjust / sum(conf_adjust)
#       
#       return(conf_adjust)
#       
#     }
#     
#     return(conf / sum(conf))
#   })
# }
# prepare_m1 <- function(m1){
#   apply(m1, MARGIN = 1, FUN = function(x){
#     x[is.na(x)] <- 0
#     return(x)
#   }) %>% 
#     t() %>% 
#     return()
# }
# 
# 
# 
# 
# #split species_cultivar column
# f$cultivar <- str_split(f$spec_cult, '_') %>% 
#   purrr::map_chr(2)
# 
# #filter for cultivars which were included in the study
# f <- f %>% 
#   filter(cultivar %in% cultivar_n)
# 
# 
# #get the cultivars to iterate over
# cases <- unique(f$spec_cult) 
# f <- data.table::data.table(f)
# 
# #calculate the weighted prediction
# weighted_pred <- purrr::map(cases, function(case){
#   
#   #extract the weights
#   conf <- performance %>% 
#     mutate(spec_cul = paste0(species,'_', cultivar)) %>% 
#     filter(spec_cul == case) %>% 
#     arrange(repetition) %>% 
#       pull(rpiq_adj)
#   
#   #arrange as a matrix, adjust the weights so that they add up to 1
#   conf_matrix <- f %>% 
#       filter(spec_cult == case) %>% 
#       select(X1:X10) %>% 
#       as.matrix() %>% 
#       prepare_conf(conf = conf)
#   
#   #arrange the predictions as a matrix, remove all predictions if more than 5 NAs
#   m1 <-f %>% 
#       filter(spec_cult == case) %>% 
#       select(X1:X10) %>% 
#       as.matrix() %>% 
#       prepare_m1()
# 
#   #calculate the weighted sum
#   test <- purrr::map_dbl(1:ncol(conf_matrix), function(i) m1[i,] %*% conf_matrix[,i])
#     
#     return(test)
#   }, .progress = TRUE) %>% 
#   do.call(c,.) 
# 
# 
# pred_info <- purrr::map(cases, function(case){
#   
#   f %>% 
#     filter(spec_cult == case) %>% 
#     select(spec_cult, location, gcm, ssp, scenario_year, year_id) %>% 
#     return()
# }, .progress = TRUE) %>% 
#   do.call(rbind,.) 
# 
#   #calculate the sd of the prediction
# f_test <- f %>% 
#   filter(spec_cult %in% cases) %>% 
#   select(X1:X10) %>% 
#   apply(MARGIN = 1, function(r) sd(r, na.rm = TRUE)) %>% 
#   unlist() %>% 
#   cbind(f)
# 
# 
# #add the weighted prediction to the pred_info and then merge it with the f_test object to have everything together
# f_out <- pred_info %>% 
#   mutate(weighted_pred = weighted_pred) %>% 
#   merge(f_test, by = c('spec_cult', 'location', 'gcm', 'ssp', 'scenario_year', 'year_id'))
# 
# write.csv(f_out, file = 'data/projected_bloomdates_future.csv', row.names = FALSE)

f_out <- read.csv('data/projected_bloomdates_future.csv')

#rm(f, f_test, sd, weighted_pred)

#memory.limit(size = 10000)
f_out <- data.table::as.data.table(f_out)
gc()



#split the column into species and cultivar
f_out$species <- sapply(strsplit(as.character(f_out$spec_cult), "_"), function(x) x[1])
f_out$cultivar <- sapply(strsplit(as.character(f_out$spec_cult), "_"), function(x) x[2])

#subset so that we only have cultivars we included in the study
f_out <- f_out[cultivar %in% cultivar_n]

#make sure Japanese Plum is labelled as European plum
f_out$species <- gsub("Japanese Plum", 'European Plum', f_out$species)


names(f_out)[names(f_out) == "."] <- "sd"
names(f_out)[names(f_out) == "weighted_pred"] <- "pheno_predicted"


#make plot to compare projections of local fitted models and "foreign" models
master_fitting_data <- read.csv('data/master_phenology_repeated_splits.csv') %>% 
  filter(repetition == 1) %>% 
  mutate(species = gsub('Japanese Plum', 'European Plum', species)) %>% 
  group_by(species, cultivar) %>% 
  summarize(loc = unique(location)) %>% 
  mutate(spec_cult_loc = paste(species, cultivar, loc, sep = '_'),
         species_location = paste(species, loc, sep = '_'))


f_out$spec_cult_loc <- paste(f_out$species, f_out$cultivar, f_out$location, sep = '_')
f_out$local_cultivar <- ifelse(f_out$spec_cult_loc %in% master_fitting_data$spec_cult_loc, yes = TRUE, no = FALSE)

f_out$fail <- is.na(f_out$pheno_predicted)
fail_intermediate <- f_out[, .(fail_rate = round((sum(fail) / length(fail))*100, digits = 2)),
                           by = .(species, location, ssp, gcm, scenario_year, local_cultivar)]
fail_summarized <- fail_intermediate[,.(median_fail = median(fail_rate)),
                                     by = .(species, location, ssp, scenario_year, local_cultivar)]



median_future <- f_out[, .(med_future = mean(pheno_predicted, na.rm = TRUE), 
                 sd_future = sd(pheno_predicted, na.rm = TRUE)),
               by = .(species, location, ssp, gcm, scenario_year)]


median_future_local_vs_foreign <- f_out[, .(med_future = mean(pheno_predicted, na.rm = TRUE), 
                           sd_future = sd(pheno_predicted, na.rm = TRUE)),
                       by = .(species, location, ssp, gcm, scenario_year, local_cultivar)]

rm(f_out)
gc()





#calculate median for both
median_2015 <- pheno_2015 %>% 
  group_by(species, location) %>% 
  summarise(med_current = median(pheno_predicted, na.rm = TRUE),
            sd_current = sd(pheno_predicted, na.rm = TRUE))

median_2015_locfor <- pheno_2015 %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_'),
         local_cultivar = ifelse(spec_cult_loc %in% master_fitting_data$spec_cult_loc, yes = TRUE, no = FALSE)) %>% 
  group_by(species, location, local_cultivar) %>% 
  summarise(med_current = median(pheno_predicted, na.rm = TRUE),
            sd_current = sd(pheno_predicted, na.rm = TRUE))


shift_df <- merge(median_2015, median_future, by = c('species','location')) %>% 
  mutate(shift_bloom = round(med_future - med_current, digits = 2))

shift_table <- shift_df %>% 
  group_by(species, location, scenario_year, ssp) %>% 
  summarise(min_shift = min(shift_bloom, na.rm = TRUE),
            max_shift = max(shift_bloom, na.rm = TRUE))

write.csv(shift_table, 'data/summary_shift_bloom_date_text_v2.csv', row.names = FALSE)


#merge the two
shift_df %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  filter(scenario_year == '2050') %>% 
  ggplot(ggplot2::aes(y = loc)) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = loc + dodge_low, ymax = loc + dodge_up, fill = ssp)) +
  geom_point(aes(x = med_future, y = loc + ((dodge_low + dodge_up)/2)),
             col = 'black',
             show.legend = FALSE, shape = 21) + 
  geom_rect(aes(xmin = med_current - 2.5, xmax = med_current + 2.5, ymax = loc - 0.4, ymin = loc + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(~species, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(1,  60, 121, 182), 
                     labels = c('Jan', 'Mar', 'May', 'Jul'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152, 182)) +
  scale_y_reverse(breaks = 1:6, labels =c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')) +
  coord_cartesian(xlim = c(1, 182)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_med_bloom_2050_v1_updated_gcms.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')


shift_df %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  filter(scenario_year == '2085') %>% 
  ggplot(ggplot2::aes(y = loc)) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = loc + dodge_low, ymax = loc + dodge_up, fill = ssp)) +
  geom_point(aes(x = med_future, y = loc + ((dodge_low + dodge_up)/2)),
             col = 'black',
             show.legend = FALSE, shape = 21) + 
  geom_rect(aes(xmin = med_current - 2.5, xmax = med_current + 2.5, ymax = loc - 0.4, ymin = loc + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(~species, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(1,  60, 121, 182), 
                     labels = c('Jan', 'Mar', 'May', 'Jul'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152, 182)) +
  scale_y_reverse(breaks = 1:6, labels =c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')) +
  coord_cartesian(xlim = c(1, 182)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_med_bloom_2085_v1_updated_gcms.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')

shift_df %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  filter(scenario_year == '2050') %>% 
  ggplot(ggplot2::aes(y = spec)) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = spec + dodge_low, ymax = spec + dodge_up, fill = ssp)) +
  geom_point(aes(x = med_future, y = spec + ((dodge_low + dodge_up)/2)),
             col = 'black',
             show.legend = FALSE, shape = 4, size = 1) + 
  geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = spec - 0.4, ymin = spec + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(location~., scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  scale_y_reverse(breaks = rep(1:7,6), labels = rep(c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'), 6)) +
  coord_cartesian(xlim = c(32, 155)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_med_bloom_2050_v2_updated_gcms.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')


shift_df %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  filter(scenario_year == '2085') %>% 
  ggplot(ggplot2::aes(y = spec)) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = spec + dodge_low, ymax = spec + dodge_up, fill = ssp)) +
  geom_point(aes(x = med_future, y = spec + ((dodge_low + dodge_up)/2)),
             col = 'black',
             show.legend = FALSE, shape = 4, size = 1) + 
  geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = spec - 0.4, ymin = spec + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(location~., scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  scale_y_reverse(breaks = rep(1:7,6), labels = rep(c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'), 6)) +
  coord_cartesian(xlim = c(32, 155)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_med_bloom_2085_v2_updated_gcms.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')




#similar plot but now comparing local fitted vs foreign projections of species


shift_df_locfor <- merge(median_2015_locfor, median_future_local_vs_foreign, by = c('species','location', 'local_cultivar')) %>% 
  mutate(shift_bloom = round(med_future - med_current, digits = 2))

shift_table <- shift_df_locfor %>% 
  group_by(species, location, scenario_year, ssp, local_cultivar) %>% 
  summarise(min_shift = min(shift_bloom, na.rm = TRUE),
            max_shift = max(shift_bloom, na.rm = TRUE))

shift_df_locfor %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         species_location = factor(paste(species, location, sep = '_'), levels = c("Apple_Klein-Altendorf",
                                                                                   "Apple_Meknes",
                                                                                   "Pear_Klein-Altendorf",
                                                                                   "Pear_Zaragoza",
                                                                                   "Apricot_Zaragoza",
                                                                                   "Apricot_Cieza",
                                                                                   "European Plum_Klein-Altendorf",
                                                                                   "Sweet Cherry_Klein-Altendorf",
                                                                                   "Sweet Cherry_Zaragoza",
                                                                                   "Almond_Santomera",
                                                                                   "Almond_Meknes",
                                                                                   "Almond_Sfax",
                                                                                   "Pistachio_Sfax")),
         spec_loc = as.numeric(species_location),
         species = factor(species, levels =  c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
         local_cultivar = factor(local_cultivar, levels = c(TRUE, FALSE), labels = c('local calibration', 'projected')),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  drop_na() %>% 
  filter(scenario_year == '2050') %>% 
  ggplot(ggplot2::aes(y = spec_loc)) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = spec_loc + dodge_low, ymax = spec_loc + dodge_up, fill = ssp), alpha = 0.2) +
  geom_point(aes(x = med_future, y = spec_loc + ((dodge_low + dodge_up)/2)),
             col = 'black',
             show.legend = FALSE, shape = 4, size = 1) + 
  geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = spec_loc - 0.4, ymin = spec_loc + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species~local_cultivar, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  scale_y_reverse(breaks = 1:13, labels = c('Klein-Altendorf', 'Meknes', 'Klein-Altendorf', 'Zaragoza', 'Zaragoza', 'Cieza', 'Klein-Altendorf', 'Klein-Altendorf', 'Zaragoza', 'Santomera', 'Meknes', 'Sfax', 'Sfax')) +
  coord_cartesian(xlim = c(32, 155)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_med_bloom_2050_loc-vs-for_v3_updated_gcms.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')


#data.frame for heat map of median unfulfilled thermal requirements
fail_plot <- fail_summarized %>% 
  mutate(species_location = factor(paste(species, location, sep = '_'), levels = c("Apple_Klein-Altendorf",
                                                                                   "Apple_Meknes",
                                                                                   "Pear_Klein-Altendorf",
                                                                                   "Pear_Zaragoza",
                                                                                   "Apricot_Zaragoza",
                                                                                   "Apricot_Cieza",
                                                                                   "European Plum_Klein-Altendorf",
                                                                                   "Sweet Cherry_Klein-Altendorf",
                                                                                   "Sweet Cherry_Zaragoza",
                                                                                   "Almond_Santomera",
                                                                                   "Almond_Meknes",
                                                                                   "Almond_Sfax",
                                                                                   "Pistachio_Sfax")),
         spec_loc = as.numeric(species_location),
         species = factor(species, levels =  c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'),
                          labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
         local_cultivar = factor(local_cultivar, levels = c(TRUE, FALSE), labels = c('Local calibration', 'Projected')),
         dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         median_fail_text = round(median_fail, digits = 0),
         #median_fail_text = ifelse(median_fail < 10, yes = paste0(' ', median_fail_text), no = median_fail_text),
         median_fail_text = ifelse(median_fail < 1 & median_fail > 0, yes = '<1', no = median_fail_text),
         median_fail_text = ifelse(median_fail < 100 & median_fail > 99, yes = '>99', no = median_fail_text),
         median_fail_text = paste0(median_fail_text, ' %')) %>% 
  drop_na() 

#decides where to start the heatmap
x_cutoff <- 157
heat_map_width <- 11.6


#data.frame controlling the positioning of the "No Pred." label in the plot
df_text_no_pred <- data.frame(y =  rep(c(0.5,2.5, 4.5, 6.5, 7.5, 9.45, 12.5), 2),
                              x = x_cutoff + 0.5,
                              species = factor(rep(c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio'), 2),
                                               levels =  c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
                              local_cultivar = factor(rep(c('Local calibration', 'Projected'), each = 7), levels = c('Local calibration', 'Projected')))

df_padding_box <- data.frame(y = rep(c(6.45, 12.45), 2),
                             x = x_cutoff +1,
                             species = factor(rep(c('Europ. Plum', 'Pistachio'), 2),
                                              levels =  c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
                             local_cultivar = factor(rep(c('Local calibration', 'Projected'), each = 2), levels = c('Local calibration', 'Projected')))


#data.frame to control the grey box background of the heatmap
grey_box_df <- data.frame(spec_loc = NA,
           start = 152,
           end = Inf,
           sepecies = factor(rep(c('Apple', 'Apple',  'Pear',  'Pear', 'Apricot', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Sweet Cherry', rep('Almond', 3), 'Pistachio'), 2),
                             levels =  c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
           local_cultivar = factor(rep(c('Local calibration', 'Projected'), each = 13), levels = c('Local calibration', 'Projected')))


#needed for subsetting the data.frame
year_used <- 2050

shift_df_locfor %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         species_location = factor(paste(species, location, sep = '_'), levels = c("Apple_Klein-Altendorf",
                                                                                   "Apple_Meknes",
                                                                                   "Pear_Klein-Altendorf",
                                                                                   "Pear_Zaragoza",
                                                                                   "Apricot_Zaragoza",
                                                                                   "Apricot_Cieza",
                                                                                   "European Plum_Klein-Altendorf",
                                                                                   "Sweet Cherry_Klein-Altendorf",
                                                                                   "Sweet Cherry_Zaragoza",
                                                                                   "Almond_Santomera",
                                                                                   "Almond_Meknes",
                                                                                   "Almond_Sfax",
                                                                                   "Pistachio_Sfax")),
         ssp_label = factor(ssp, levels = c('ssp126', 'ssp245', 'ssp370', 'ssp585'), labels = c('SSP1', 'SSP2', 'SSP3', 'SSP5')),
         spec_loc = as.numeric(species_location),
         species = factor(species, levels =  c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'),
                          labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
         local_cultivar = factor(local_cultivar, levels = c(TRUE, FALSE), labels = c('Local calibration', 'Projected')),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  drop_na() %>% 
  filter(scenario_year == as.character(year_used)) %>% 
  ggplot(ggplot2::aes(y = spec_loc)) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = spec_loc + dodge_low, ymax = spec_loc + dodge_up, fill = ssp_label), alpha = 0.2) +
  geom_point(aes(x = med_future, y = spec_loc + ((dodge_low + dodge_up)/2), 
                 shape = 'Median prediction\nof individual GCMs'),
             col = 'black',
             show.legend = TRUE, size = 1) + 
  geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = spec_loc - 0.4, ymin = spec_loc + 0.4, fill = 'Simulation 2015'),  size = 2) +
  geom_rect(data = grey_box_df, xmin = x_cutoff, xmax = Inf,ymax = Inf, ymin = -Inf, fill = 'grey85',  size = 2) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+3, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+5, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+7, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+9, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+11, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+13, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+15, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  scale_colour_gradient(low = "white", high = "firebrick2", limits = c(0, 100),name = 'Median Failure Rate') +
  geom_vline(xintercept = x_cutoff) +
  geom_point(data = df_padding_box, aes(y = y, x = x), col = 'grey85', size = 0.1) +
  geom_text(data = df_text_no_pred, aes(y = y, x = x,
                                        label = 'No Pred.'),hjust = 0) +
  # geom_text(data = fail_plot[fail_plot$scenario_year == year_used],aes(x = 155 + 2, y = spec_loc  + ((dodge_low + dodge_up)/2), 
  #               label = paste(format(round(median_fail, digits = 0), nsmall = 0), '%')), size = 3) +
  geom_text(data = fail_plot[fail_plot$scenario_year == year_used],aes(x = x_cutoff + heat_map_width * 1.2, y = spec_loc  + ((dodge_low + dodge_up)/2),
                label = median_fail_text), size = 3, hjust = 1) +
  facet_grid(species~local_cultivar, scales = 'free_y', space = 'free_y') +
  #scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(breaks = c('SSP1', 'SSP2',  'Simulation 2015', 'SSP3', 'SSP5'),  
                    values = c("#56B4E9", "#009E73",'black',"#F0E442",  "#E69F00", 'grey70'), name = "Weather Scenario")+
  scale_shape_manual(values = 4, name = NULL) + 
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  scale_y_reverse(breaks = 1:13, labels = c('Klein-Altendorf', 'Meknes', 'Klein-Altendorf', 'Zaragoza', 'Zaragoza', 'Cieza', 'Klein-Altendorf', 'Klein-Altendorf', 'Zaragoza', 'Santomera', 'Meknes', 'Sfax', 'Sfax'),
                  minor_breaks = NULL) +
  coord_cartesian(xlim = c(32, x_cutoff + heat_map_width)) +
  guides( fill=guide_legend(nrow=2,byrow=TRUE, order = 1)) +
  theme(legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date')
ggplot2::ggsave('figures/paper/change_med_bloom_2050_loc-vs-for_v4_updated_gcms.jpeg', device = 'jpeg',
                height = 42, width = 31, units = 'cm')




year_used <- 2085
shift_df_locfor %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         species_location = factor(paste(species, location, sep = '_'), levels = c("Apple_Klein-Altendorf",
                                                                                   "Apple_Meknes",
                                                                                   "Pear_Klein-Altendorf",
                                                                                   "Pear_Zaragoza",
                                                                                   "Apricot_Zaragoza",
                                                                                   "Apricot_Cieza",
                                                                                   "European Plum_Klein-Altendorf",
                                                                                   "Sweet Cherry_Klein-Altendorf",
                                                                                   "Sweet Cherry_Zaragoza",
                                                                                   "Almond_Santomera",
                                                                                   "Almond_Meknes",
                                                                                   "Almond_Sfax",
                                                                                   "Pistachio_Sfax")),
         ssp_label = factor(ssp, levels = c('ssp126', 'ssp245', 'ssp370', 'ssp585'), labels = c('SSP1', 'SSP2', 'SSP3', 'SSP5')),
         spec_loc = as.numeric(species_location),
         species = factor(species, levels =  c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'),
                          labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
         local_cultivar = factor(local_cultivar, levels = c(TRUE, FALSE), labels = c('Local calibration', 'Projected')),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  drop_na() %>% 
  filter(scenario_year == as.character(year_used)) %>% 
  ggplot(ggplot2::aes(y = spec_loc)) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = spec_loc + dodge_low, ymax = spec_loc + dodge_up, fill = ssp_label), alpha = 0.2) +
  geom_point(aes(x = med_future, y = spec_loc + ((dodge_low + dodge_up)/2), 
                 shape = 'Median prediction\nof individual GCMs'),
             col = 'black',
             show.legend = TRUE, size = 1) + 
  geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = spec_loc - 0.4, ymin = spec_loc + 0.4, fill = 'Simulation 2015'),  size = 2) +
  geom_rect(data = grey_box_df, xmin = x_cutoff, xmax = Inf,ymax = Inf, ymin = -Inf, fill = 'grey85',  size = 2) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+3, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+5, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+7, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+9, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+11, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+13, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = fail_plot[fail_plot$scenario_year == year_used], aes(x=x_cutoff+15, y= spec_loc + ((dodge_low + dodge_up)/2), color=median_fail), shape=15, size=7.5) +
  geom_point(data = df_padding_box, aes(y = y, x = x), col = 'grey85', size = 0.1) +
  scale_colour_gradient(low = "white", high = "firebrick2", limits = c(0, 100),name = 'Median Failure Rate') +
  geom_vline(xintercept = x_cutoff) +
  geom_text(data = df_text_no_pred, aes(y = y, x = x,
                                        label = 'No Pred.'),hjust = 0) +
  # geom_text(data = fail_plot[fail_plot$scenario_year == year_used],aes(x = 155 + 2, y = spec_loc  + ((dodge_low + dodge_up)/2), 
  #               label = paste(format(round(median_fail, digits = 0), nsmall = 0), '%')), size = 3) +
  geom_text(data = fail_plot[fail_plot$scenario_year == year_used],aes(x = x_cutoff + heat_map_width * 1.2, y = spec_loc  + ((dodge_low + dodge_up)/2),
                                                                       label = median_fail_text), size = 3, hjust = 1) +
  facet_grid(species~local_cultivar, scales = 'free_y', space = 'free_y') +
  #scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(breaks = c('SSP1', 'SSP2',  'Simulation 2015', 'SSP3', 'SSP5'),  
                    values = c("#56B4E9", "#009E73",'black',"#F0E442",  "#E69F00", 'grey70'), name = "Weather Scenario")+
  scale_shape_manual(values = 4, name = NULL) + 
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  scale_y_reverse(breaks = 1:13, 
                  labels = c('Klein-Altendorf', 'Meknes', 'Klein-Altendorf', 'Zaragoza', 'Zaragoza', 'Cieza', 'Klein-Altendorf', 'Klein-Altendorf', 'Zaragoza', 'Santomera', 'Meknes', 'Sfax', 'Sfax'), 
                  minor_breaks = NULL) +
  guides( fill=guide_legend(nrow=2,byrow=TRUE, order = 1)) +
  coord_cartesian(xlim = c(32, x_cutoff + heat_map_width)) +
  theme(legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date')
ggplot2::ggsave('figures/paper/change_med_bloom_2085_loc-vs-for_v4_updated_gcms.jpeg', device = 'jpeg',
                height = 42, width = 31, units = 'cm')





#makes similar plot for 2015 (but maybe without the flowering time window)

fail_2015 <- pheno_2015 %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = "_"),
         local_cultivar = ifelse(spec_cult_loc %in% master_fitting_data$spec_cult_loc, yes = 'Local calibration', no = 'Projected'),
         species_location = factor(paste(species, location, sep = '_'), levels = c("Apple_Klein-Altendorf",
                                                                           "Apple_Meknes",
                                                                           "Pear_Klein-Altendorf",
                                                                           "Pear_Zaragoza",
                                                                           "Apricot_Zaragoza",
                                                                           "Apricot_Cieza",
                                                                           "European Plum_Klein-Altendorf",
                                                                           "Sweet Cherry_Klein-Altendorf",
                                                                           "Sweet Cherry_Zaragoza",
                                                                           "Almond_Santomera",
                                                                           "Almond_Meknes",
                                                                           "Almond_Sfax",
                                                                           "Pistachio_Sfax")),
         spec_loc = as.numeric(species_location),
         species = factor(species, levels =  c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'),
                          labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
         fail = is.na(pheno_predicted)) %>%
  select(species, location, local_cultivar, fail, spec_loc) %>% 
  drop_na() %>% 
  group_by(species, location, local_cultivar, spec_loc) %>% 
  summarize(fail_rate = (sum(fail) / n()) * 100) %>% 
  mutate(fail_text = round(fail_rate, digits = 0),
         fail_text = ifelse(fail_rate < 1 & fail_rate > 0, yes = '<1', no = as.character(fail_text)),
         fail_text = ifelse(fail_rate > 99 & fail_rate < 100, yes = '>99', no = fail_text),
         fail_text = paste0(fail_text, ' %'))

x_cutoff <- 162
point_size <- 15
heat_map_width <- 11.6

df_text_no_pred <- data.frame(y =  rep(c(0.5,2.5, 4.5, 6.5, 7.5, 9.45, 12.5), 2),
                              x = x_cutoff + 0.5,
                              species = factor(rep(c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio'), 2),
                                               levels =  c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
                              local_cultivar = factor(rep(c('Local calibration', 'Projected'), each = 7), levels = c('Local calibration', 'Projected')))


pheno_2015 %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = "_"),
         local_cultivar = ifelse(spec_cult_loc %in% master_fitting_data$spec_cult_loc, yes = 'Local calibration', no = 'Projected'),
         species_location = factor(paste(species, location, sep = '_'), levels = c("Apple_Klein-Altendorf",
                                                                                   "Apple_Meknes",
                                                                                   "Pear_Klein-Altendorf",
                                                                                   "Pear_Zaragoza",
                                                                                   "Apricot_Zaragoza",
                                                                                   "Apricot_Cieza",
                                                                                   "European Plum_Klein-Altendorf",
                                                                                   "Sweet Cherry_Klein-Altendorf",
                                                                                   "Sweet Cherry_Zaragoza",
                                                                                   "Almond_Santomera",
                                                                                   "Almond_Meknes",
                                                                                   "Almond_Sfax",
                                                                                   "Pistachio_Sfax")),
         spec_loc = as.numeric(species_location),
         species = factor(species, levels =  c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'),
                          labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))) %>% 
  drop_na() %>% 
  ggplot(ggplot2::aes(y = spec_loc)) +
  geom_boxplot(aes(y = spec_loc, x = pheno_predicted, group = spec_loc, fill = species), width = 0.6, show.legend = FALSE) +
  geom_rect(data = grey_box_df, xmin = x_cutoff, xmax = Inf,ymax = Inf, ymin = -Inf, fill = 'grey85',  size = 2) +
  #geom_point(data = fail_2015, aes(x=x_cutoff+3, y= spec_loc, color=fail_rate), shape=15, size=point_size) +
  geom_point(data = fail_2015, aes(x=x_cutoff+6+1, y= spec_loc, color=fail_rate), shape=15, size=point_size) +
  geom_point(data = fail_2015, aes(x=x_cutoff+8, y= spec_loc, color=fail_rate), shape=15, size=point_size) +
  geom_point(data = fail_2015, aes(x=x_cutoff+10, y= spec_loc, color=fail_rate), shape=15, size=point_size) +
  geom_point(data = fail_2015, aes(x=x_cutoff+12, y= spec_loc, color=fail_rate), shape=15, size=point_size) +
  geom_point(data = fail_2015, aes(x=x_cutoff+14, y= spec_loc, color=fail_rate), shape=15, size=point_size) +
  geom_point(data = fail_2015, aes(x=x_cutoff+16, y= spec_loc, color=fail_rate), shape=15, size=point_size) +
  geom_point(data = df_padding_box, aes(y = y, x = x), col = 'grey85', size = 0.1) +
  scale_colour_gradient(low = "white", high = "firebrick2", limits = c(0, 100),name = 'Failure Rate') +
  geom_vline(xintercept = x_cutoff) +
  geom_text(data = df_text_no_pred, aes(y = y, x = x,
                                        label = 'No Pred.'),hjust = 0) +
  # geom_text(data = fail_plot[fail_plot$scenario_year == year_used],aes(x = 155 + 2, y = spec_loc  + ((dodge_low + dodge_up)/2), 
  #               label = paste(format(round(median_fail, digits = 0), nsmall = 0), '%')), size = 3) +
  geom_text(data = fail_2015,aes(x = x_cutoff + (heat_map_width + (heat_map_width/2)), y = spec_loc,
                                                                       label = fail_text), hjust = 1) +
  facet_grid(species~local_cultivar, scales = 'free_y', space = 'free_y') +
  #scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c("#009E73", "#009E73", "#E69F00","#E69F00" ,"#E69F00", "#56B4E9", "#56B4E9" ), name = NULL)+
  scale_shape_manual(values = 4, name = NULL) + 
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(1, 32,  60, 91, 121, 152), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  scale_y_reverse(breaks = 1:13, 
                  labels = c('Klein-Altendorf', 'Meknes', 'Klein-Altendorf', 'Zaragoza', 'Zaragoza', 'Cieza', 'Klein-Altendorf', 'Klein-Altendorf', 'Zaragoza', 'Santomera', 'Meknes', 'Sfax', 'Sfax'), 
                  minor_breaks = NULL) +
  guides( fill=guide_legend(nrow=2,byrow=TRUE, order = 1)) +
  coord_cartesian(xlim = c(0, x_cutoff + heat_map_width)) +
  theme(legend.position = 'bottom') +
  ylab('') +
  xlab('Predicted Bloom Date')
ggsave('figures/paper/2015_predictions_local-vs-foreign.jpeg', height = 35, width = 35, units = 'cm', device = 'jpeg')
  
           


#so why do the predictions of local calibrated and "foreign" cultivars differ? what makes them different?





















#change species and location
fail_2015 <- read.csv('data/failure-rate_thermal-risk_2020-sim.csv') %>% 
  filter(cultivar %in% cultivar_n) %>% 
  mutate(species = stringr::str_to_title(species)) %>% 
  group_by(species, location) %>% 
  summarise(failure_rate = median(failure_rate))

med_bloom_2015 <- median_2015 %>% 
  merge(fail_2015, by = c('species', 'location')) %>% 
  mutate(         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
                  species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
                  spec = as.numeric(species),
                  loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
                  alpha = ifelse(failure_rate >= 50, yes = 0.5, no = 1))

alpha_fail <- 0.75
#only do it for the cases when thermal requirements are not met
alpha_df <- fail_sum %>% 
  mutate(         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
                  species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
                  spec = as.numeric(species),
                  loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
                  alpha = ifelse(out >= 50, yes = alpha_fail, no = 1)) %>% 
  merge(median_2015, by = c('species', 'location'))



shift_df %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
         spec = as.numeric(species),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  merge(alpha_df, by = c('species', 'location', 'loc', 'spec', 'med_current', 'sd_current')) %>% 
  mutate(alpha = ifelse(out >= 50, yes = alpha_fail, no = 1)) %>% 
  filter(scenario_year == '2050') %>% 
  ggplot(ggplot2::aes(y = loc)) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = loc + dodge_low, ymax = loc + dodge_up, fill = ssp, alpha = alpha)) +
  geom_point(aes(x = med_future, y = loc + ((dodge_low + dodge_up)/2),
                 alpha = alpha + 0.1),
             col = 'black',
             show.legend = FALSE, shape = 4, size = 1) + 
  geom_rect(data = alpha_df, aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = loc - 0.4, ymin = loc + 0.4, fill = 'Simulation 2015',
                                 alpha = alpha + 0.1),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species~., scales = 'free_y', space = 'free_y') +
  scale_alpha_continuous(guide=FALSE) +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  scale_y_reverse(breaks = rep(1:6,7), labels = rep(c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'), 7)) +
  coord_cartesian(xlim = c(32, 155)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_med_bloom_2050_v3.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')


shift_df %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
         spec = as.numeric(species),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  merge(alpha_df, by = c('species', 'location', 'loc', 'spec', 'med_current', 'sd_current')) %>% 
  mutate(alpha = ifelse(out >= 50, yes = alpha_fail, no = 1)) %>% 
  filter(scenario_year == '2085') %>% 
  ggplot(ggplot2::aes(y = loc)) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = loc + dodge_low, ymax = loc + dodge_up, fill = ssp, alpha = alpha)) +
  geom_point(aes(x = med_future, y = loc + ((dodge_low + dodge_up)/2),
                 alpha = alpha + 0.1),
             col = 'black',
             show.legend = FALSE, shape = 4, size = 1) + 
  geom_rect(data = alpha_df, aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = loc - 0.4, ymin = loc + 0.4, fill = 'Simulation 2015',
                                 alpha = alpha + 0.1),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species~., scales = 'free_y', space = 'free_y') +
  scale_alpha_continuous(guide=FALSE) +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  scale_y_reverse(breaks = rep(1:6,7), labels = rep(c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'), 7)) +
  coord_cartesian(xlim = c(32, 155)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_med_bloom_2085_v3.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')
#make publication-level figures
library(tidyverse)
library(ggplot2)
library(patchwork)
library(LarsChill)
library(patchwork)


#setwd('../2023_adamedor-dataset_fitting/')

performance <- read.csv('data/performance_fitted_models.csv')

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

performance <- performance %>% 
  dplyr::filter(cultivar %in% cultivar_n)

performance <- performance %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum'))

performance %>% 
  filter(split == 'Validation') %>% 
  group_by(species) %>% 
  summarise(med_rmse = median(rmse),
            quan_5 = quantile(rmse, 0.05),
            quan_95 = quantile(rmse, 0.95)) %>% 
  mutate(span = quan_95 - quan_5)

performance %>% 
  filter(split == 'Validation') %>% 
  group_by(species) %>% 
  summarise(med_rpiq = median(rpiq_adj),
            quan_5 = quantile(rpiq_adj, 0.05),
            quan_95 = quantile(rpiq_adj, 0.95),
            quan_low = (sum(rpiq_adj < 1) / n())*100) %>% 
  mutate(span = quan_95 - quan_5)

table_text <- performance %>% 
  filter(split == 'Validation',
         species == 'Almond') %>% 
  group_by(cultivar) %>% 
  summarise(med_rpiq = median(rpiq_adj),
            quan_5 = quantile(rpiq_adj, 0.05),
            quan_95 = quantile(rpiq_adj, 0.95),
            quan_low = (sum(rpiq_adj < 1) / n())*100,
            
            med_rmse = median(rmse),
            quan_5_rmse = quantile(rmse, 0.05),
            quan_95_rmse = quantile(rmse, 0.95),) %>% 
  arrange(med_rpiq) %>% 
  mutate(span = quan_95 - quan_5)

summary(table_text)

rpiq_out <- performance %>% 
  filter(split == 'Validation',
         species == 'Almond') %>% 
  group_by(cultivar) %>% 
  summarise(n_low = sum(rpiq_adj < 1.0))

plot(cumsum(sort(rpiq_out$n_low / sum(rpiq_out$n_low), decreasing = TRUE)))


median_df <- performance %>% 
  filter(split == 'Validation',
         cultivar %in% cultivar_n) %>% 
  group_by(species, cultivar) %>% 
  summarise(median_rpiq = median(rpiq_adj),
            median_rmse = median(rmse),
            median_bias = median(mean_bias))

#110 cultivars
test <- performance %>% 
  group_by(species) %>% 
  summarise(n = length(unique(cultivar)))
test

sum(test$n)


#maybe change name of European Plum / Japanese Plum / Pistachio inside the plot already to a, b, c

p1 <- performance %>% 
  merge.data.frame(median_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi')) %>% 
  mutate(species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry'))) %>% 
  filter(species %in% c('Almond', 'Apple', 'Apricot')) %>% 
  ggplot(aes(y = reorder(cultivar,  median_rpiq), fill = split, x = rpiq_adj)) +
  geom_boxplot() +
  theme_bw() +
  #xlim(0, 21) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  scale_fill_manual(values = c("steelblue", "#E69F00"), name = 'Data Split') +
  theme_bw(base_size = 15) +
  ylab('Cultivar (ordered by decreasing RPIQ of validation data)') +
  xlab('Ratio of Performance to Interquartile Distance') +
  facet_grid(rows = vars(species_label), scales = 'free_y', space = "free")

p2 <- performance %>% 
  merge.data.frame(median_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi')) %>% 
  mutate(species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry'))) %>% 
  filter(!(species %in% c('Almond', 'Apple', 'Apricot'))) %>% 
  ggplot(aes(y = reorder(cultivar,  median_rpiq), fill = split, x = rpiq_adj)) +
  geom_boxplot() +
  theme_bw() +
  #xlim(0, 21) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  scale_fill_manual(values = c("steelblue", "#E69F00"), name = 'Data Split') +
  theme_bw(base_size = 15) +
  ylab('') +
  xlab('Ratio of Performance to Interquartile Distance') +
  facet_grid(rows = vars(species_label), scales = 'free_y', space = "free")

p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position= 'bottom') 
ggsave('figures/paper/rpiq_all_cult.jpeg', device = 'jpeg',
       height = 35, width = 35, units = 'cm')



p1 <- performance %>% 
  merge.data.frame(median_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi')) %>% 
  mutate(species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry'))) %>% 
  filter(species %in% c('Almond', 'Apple', 'Apricot')) %>% 
  ggplot(aes(y = reorder(cultivar,  median_rpiq), fill = split, x = rmse)) +
  geom_boxplot() +
  theme_bw() +
  #xlim(0, 21) +
  scale_fill_manual(values = c("steelblue", "#E69F00"), name = 'Data Split') +
  theme_bw(base_size = 15) +
  ylab('Cultivar (ordered by increasing RMSE of validation data)') +
  xlab('Root Mean Square Error of\nBloom Prediction (days)') +
  facet_grid(rows = vars(species_label), scales = 'free_y', space = "free")

p2 <- performance %>% 
  merge.data.frame(median_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi')) %>% 
  mutate(species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry'))) %>% 
  filter(!(species %in% c('Almond', 'Apple', 'Apricot'))) %>% 
  ggplot(aes(y = reorder(cultivar,  median_rpiq), fill = split, x = rpiq_adj)) +
  geom_boxplot() +
  theme_bw() +
  #xlim(0, 21) +
  scale_fill_manual(values = c("steelblue", "#E69F00"), name = 'Data Split') +
  theme_bw(base_size = 15) +
  ylab('') +
  xlab('Root Mean Square Error of\nBloom Prediction (days)') +
  facet_grid(rows = vars(species_label), scales = 'free_y', space = "free")

p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position= 'bottom') 
ggsave('figures/paper/rmse_all_cult.jpeg', device = 'jpeg',
       height = 35, width = 35, units = 'cm')




#----------------------------------------------#
#Performance across species ####
#----------------------------------------------#

prediction_df <- read.csv('data/predicted_flowering_vs_observed.csv') %>% 
  dplyr::filter(cultivar %in% cultivar_n)

prediction_df <- prediction_df %>% 
  filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),  #seems to be outlier
         !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014), #seems to be outlier
         !(location == 'Klein-Altendorf' & year == 1958)) %>% #weather data starts in 1958 so predicting that year does not make sense
  mutate(species = recode(species, `Japanese Plum` = 'European Plum'))
iqr_df <- prediction_df %>% 
  filter(repetition == 1) %>% 
  group_by(species, cultivar) %>% 
  summarise(iqr_obs = IQR(pheno))

performance <- prediction_df %>% 
  merge.data.frame(iqr_df, by = c('species', 'cultivar'), all.x = TRUE) %>% 
  group_by(species, cultivar, repetition, split) %>% 
  summarise(rmse = chillR::RMSEP(predicted = pred, observed = pheno),
            iqr_obs = mean(iqr_obs) ,
            rpiq_adj = iqr_obs / rmse) 



okabe <- c("#E69F00", "#56B4E9",  "#009E73", "#F0E442","#0072B2" , "#D55E00", "#CC79A7",'grey40' )
col2 <- c("#009E73", "#009E73", "#E69F00","#E69F00" ,"#E69F00", "#56B4E9", "#56B4E9" )


p1 <- performance %>% 
  filter(split == 'Validation') %>% 
  mutate(species = factor(species, levels =c('Apple','Pear',  'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))) %>% 
  ggplot(aes(y = species, x = rmse)) +
  #geom_boxplot(aes(fill = species), show.legend = FALSE)+
  geom_boxplot(show.legend = FALSE, fill = 'grey70')+
  scale_y_discrete(limits = rev)+
  ylab('') +
  xlab('Root Mean Square Error (RMSE)\nfor Validation Data') + 
  scale_fill_manual(values = col2) +
  #geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw(base_size = 15)

p2 <- performance %>% 
  filter(split == 'Validation') %>% 
  mutate(species = factor(species, levels = c('Apple','Pear',  'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))) %>% 
  ggplot(aes(y = species, x = rpiq_adj)) +
  #geom_boxplot(aes(fill = species), show.legend = FALSE)+
  geom_boxplot(show.legend = FALSE, fill = 'grey70')+
  scale_y_discrete(limits = rev)+
  ylab('') +
  xlab('Ratio of Performance to Interquartile\nDistance (RPIQ) for Validation Data') + 
  scale_fill_manual(values = col2) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw(base_size = 15)+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

p1 + p2 + plot_layout(guides = 'collect') & 
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position= 'bottom')
  
ggsave('figures/paper/performance_sum.jpeg', device = 'jpeg',
       height = 15, width = 25, units = 'cm')


#check link to temperature

master_adamedor <- read.csv('data/master_phenology_repeated_splits.csv') %>% 
  filter(repetition == 1) %>% 
  mutate(species = recode(species, `Japanese plum` = 'European plum')) %>% 
  mutate(temp = recode(location, 
                       `Klein-Altendorf` = 5.8,
                       Zaragoza = 10.1,
                       Santomera = 13.2,
                       Meknes = 11.6,
                       Cieza = 10.8, 
                       Sfax = 14.7)) %>% 
  group_by(species, cultivar) %>% 
  summarize(max_temp = max(temp))

p3 <- performance %>% 
  filter(split == 'Validation') %>% 
  merge(master_adamedor, by = c('species', 'cultivar')) %>% 
  mutate(species = factor(species, levels =c('Apple','Pear',  'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))) %>% 
  ggplot(aes(y = max_temp, x = rmse, group = max_temp)) +
  #geom_boxplot(aes(fill = species), show.legend = FALSE)+
  geom_boxplot(show.legend = FALSE, fill = 'grey70')+
  ylab('') +
  xlab('Root Mean Square Error (RMSE)\nfor Validation Data') + 
  scale_fill_manual(values = col2) +
  #geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw(base_size = 15)

p4 <- performance %>% 
  filter(split == 'Validation') %>% 
  merge(master_adamedor, by = c('species', 'cultivar')) %>% 
  mutate(species = factor(species, levels =c('Apple','Pear',  'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))) %>% 
  ggplot(aes(y = max_temp, x = rpiq_adj, group = max_temp)) +
  #geom_boxplot(aes(fill = species), show.legend = FALSE)+
  geom_boxplot(show.legend = FALSE, fill = 'grey70')+
  ylab('') +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  xlab('Ratio of Performance to Interquartile\nDistance (RPIQ) for Validation Data') + 
  scale_fill_manual(values = col2) +
  #geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw(base_size = 15)+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

p1_mod <- p1 + xlab('') + ylab('Modelled Species') + theme(plot.margin = unit(c(0,0.5,0,0), "cm"))
p2_mod <- p2 + xlab('')+ ylab('')+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
p3_mod <- p3 +  ylab('Mean Daily Minimum\nTemperature (°C) (1990 - 2020)')+ theme(plot.margin = unit(c(0,0.5,0,0), "cm"))
p4_mod <- p4 + ylab('')+ theme(plot.margin = unit(c(0,0,0,0), "cm"))

(p1_mod + p2_mod) / (p3_mod + p4_mod) +
  plot_layout(guides = 'collect') & 
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position= 'bottom')

ggsave('figures/paper/performance_sum_v2.jpeg', device = 'jpeg',
       height = 15, width = 25, units = 'cm', dpi = 600)



#eike wants to have the figure with x and y swapped
p1 <- performance %>% 
  filter(split == 'Validation') %>% 
  mutate(species = factor(species, levels =c('Apple','Pear',  'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'),
                          labels = c('Apple','Pear',  'Apricot', 'European\nPlum', 'Sweet\nCherry', 'Almond', 'Pistachio'))) %>% 
  ggplot(aes(x = species, y = rmse)) +
  #geom_boxplot(aes(fill = species), show.legend = FALSE)+
  geom_boxplot(show.legend = FALSE, fill = 'grey70')+
  scale_x_discrete(limits = rev)+
  xlab('') +
  ylab('Root Mean Square\nError (RMSE)\nfor Validation Data') + 
  scale_fill_manual(values = col2) +
  #geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw(base_size = 13)

p2 <- performance %>% 
  filter(split == 'Validation') %>% 
  mutate(species = factor(species, levels = c('Apple','Pear',  'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'),
                          labels = c('Apple','Pear',  'Apricot', 'European\nPlum', 'Sweet\nCherry', 'Almond', 'Pistachio'))) %>% 
  ggplot(aes(x = species, y = rpiq_adj)) +
  #geom_boxplot(aes(fill = species), show.legend = FALSE)+
  geom_boxplot(show.legend = FALSE, fill = 'grey70')+
  scale_x_discrete(limits = rev)+
  xlab('') +
  ylab('Ratio of Performance to\nInterquartile Distance (RPIQ)\nfor Validation Data') + 
  scale_fill_manual(values = col2) +
  geom_hline(yintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw(base_size = 13)


p3 <- performance %>% 
  filter(split == 'Validation') %>% 
  merge(master_adamedor, by = c('species', 'cultivar')) %>% 
  mutate(species = factor(species, levels =c('Apple','Pear',  'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'),
                          labels = c('Apple','Pear',  'Apricot', 'European\nPlum', 'Sweet\nCherry', 'Almond', 'Pistachio'))) %>% 
  ggplot(aes(x = max_temp, y = rmse, group = max_temp)) +
  #geom_boxplot(aes(fill = species), show.legend = FALSE)+
  geom_boxplot(show.legend = FALSE, fill = 'grey70')+
  xlab('') +
  ylab('Root Mean Square\nError (RMSE)\nfor Validation Data') + 
  scale_x_continuous(breaks = c(5.8, 10.1, 10.8, 11.6, 13.2, 14.7))+ 
  scale_fill_manual(values = col2) +
  #geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw(base_size = 13)

p4 <- performance %>% 
  filter(split == 'Validation') %>% 
  merge(master_adamedor, by = c('species', 'cultivar')) %>% 
  mutate(species = factor(species, levels =c('Apple','Pear',  'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'),
                          labels = c('Apple','Pear',  'Apricot', 'European\nPlum', 'Sweet\nCherry', 'Almond', 'Pistachio'))) %>% 
  ggplot(aes(x = max_temp, y = rpiq_adj, group = max_temp)) +
  #geom_boxplot(aes(fill = species), show.legend = FALSE)+
  geom_boxplot(show.legend = FALSE, fill = 'grey70')+
  ylab('') +
  scale_x_continuous(breaks = c(5.8, 10.1, 10.8, 11.6, 13.2, 14.7))+ 
  geom_hline(yintercept = 1, linetype = 'dashed') +
  ylab('Ratio of Performance to\nInterquartile Distance (RPIQ)\nfor Validation Data') + 
  scale_fill_manual(values = col2) +
  #geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw(base_size = 13)

p1_mod <- p1  + theme(plot.margin = unit(c(0,0.5,0,0), "cm"))
p2_mod <- p2 + xlab('Modelled Species') + theme(plot.margin = unit(c(0,0.5,0,0), "cm"))
p3_mod <- p3 + ylab('') +  theme(plot.margin = unit(c(0,0,0,0), "cm"))
p4_mod <- p4 + ylab('') + xlab('Mean Daily Minimum\nTemperature (°C) (1990 - 2020)') + theme(plot.margin = unit(c(0,0,0,0), "cm"))

(p1_mod + p3_mod) / (p2_mod + p4_mod) +
  plot_layout(guides = 'collect') & 
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position= 'bottom')

ggsave('figures/paper/performance_sum_v3.jpeg', device = 'jpeg',
       height = 15, width = 25, units = 'cm', dpi = 600)




performance %>% 
  filter(species == 'Apple', split == 'Validation') %>% 
  group_by(species, cultivar) %>% 
  summarise(med_rmse = median(rmse))

performance %>% 
  filter(species == 'Apple', split == 'Validation') %>% 
  group_by(species) %>% 
  summarise(med_rmse = median(rmse))

performance %>% 
  filter(species == 'Apricot', split == 'Validation') %>% 
  group_by(species, cultivar) %>% 
  summarise(med_rmse = median(rmse))

performance %>% 
  filter(split == 'Validation') %>% 
  group_by(species, cultivar) %>% 
  summarise(min_rmse = min(rmse),
            med_rmse = median(rmse), 
            max_rmse = max(rmse)) %>% 
  mutate(range = max_rmse - min_rmse) %>% 
  ungroup() %>%
  group_by() %>% 
  summarise(min(range),
            median(range),
            quantile(range, 0.1),
            quantile(range, 0.9))
  summarize(min(range))
  ggplot(aes(y = species, x = range)) +
  geom_boxplot()
  summarise(med_rmse = median(rmse))


#-------------------------------------------------------------#
#Ensemble prediction ####
#-------------------------------------------------------------#

#source('code/utilities/ensemble_prediction.R')
#source('code/utilities/load_fitting_result.R')

ensemble_prediction <- read.csv('data/projected_bloomdates_ensemble_observed_weather.csv') %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum'))
str(ensemble_prediction)
str(prediction_df)

enesmble_prediction_observed <- prediction_df %>%
  mutate(year = as.character(year)) %>% 
  merge.data.frame(ensemble_prediction, 
                   by.x = c('species', 'cultivar', 'location', 'year'),
                   by.y = c('species', 'cultivar', 'location', 'scenario_year'),
                   all.x =  TRUE) %>%
  rename(pred_ensemble = pheno_predicted, pred_single = pred) %>% 
  filter(repetition == 1)

length(unique(enesmble_prediction_observed$cultivar))

performance_ensemble <- enesmble_prediction_observed %>% 
  group_by(species) %>% 
  summarise(RMSE = round(chillR::RMSEP(pred_ensemble, pheno, na.rm = TRUE),1),
            RPIQ = round(chillR::RPIQ(pred_ensemble, pheno, na.rm = TRUE),1),
            mean_bias = round(mean(pheno - pred_ensemble, na.rm  = TRUE)), 1)

enesmble_prediction_observed %>% 
  ggplot(aes(x = pheno, y = pred_ensemble)) +
  geom_point() +
  geom_errorbar(aes(ymin = pred_ensemble - sd, ymax = pred_ensemble + sd)) +
  geom_abline(slope = 1) +
  facet_wrap(~species, ncol = 4)+
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  geom_text(data = performance_ensemble,  y = 150, x = 1, 
            aes(label = paste('RMSE:', format(RMSE, nsmall = 1))), hjust = 0) +
  geom_text(data = performance_ensemble,  y = 140, x = 1, 
            aes(label = paste('RPIQ:', format(RPIQ, nsmall = 1))), hjust = 0) +
  geom_text(data = performance_ensemble,  y = 130, x = 1, 
            aes(label = paste('Mean Bias:', format(mean_bias, nsmall = 1))), hjust = 0) +
  scale_x_continuous(limits = c(0, 152),
                     breaks = c(1, 32, 60,91, 121, 152), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  scale_y_continuous(limits = c(0, 152),
                     breaks = c(1, 32, 60,91, 121, 152), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  theme_bw(base_size = 15) 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('figures/paper/figure_4.tiff', height = 15, width = 25,
       units = 'cm', device = 'tiff', dpi = 500)

performance_ensemble <- performance_ensemble %>% 
  mutate(species = factor(species, levels = c('Apple','Pear',  'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio')))

enesmble_prediction_observed %>% 
  mutate(species = as.character(species)) %>% 
  mutate(species = factor(species, levels = c('Apple', 'Pear', 'Almond', 'Pistachio', 'Apricot', 'European Plum', 'Sweet Cherry'))) %>% 
  ggplot(aes(x = pheno, y = pred_ensemble)) +
  geom_point() +
  geom_errorbar(aes(ymin = pred_ensemble - sd, ymax = pred_ensemble + sd)) +
  geom_abline(slope = 1) +
  facet_wrap(~species, ncol = 4, )+
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  geom_text(data = performance_ensemble,  y = 150, x = 1, 
            aes(label = paste('RMSE:', format(RMSE, nsmall = 1))), hjust = 0) +
  geom_text(data = performance_ensemble,  y = 140, x = 1, 
            aes(label = paste('RPIQ:', format(RPIQ, nsmall = 1))), hjust = 0) +
  geom_text(data = performance_ensemble,  y = 130, x = 1, 
            aes(label = paste('Mean Bias:', format(mean_bias, nsmall = 1))), hjust = 0) +
  scale_x_continuous(limits = c(0, 152),
                     breaks = c(1, 32, 60,91, 121, 152), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  scale_y_continuous(limits = c(0, 152),
                     breaks = c(1, 32, 60,91, 121, 152), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  theme_bw(base_size = 15) 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('figures/paper/ensemble_prediction_performance_changed_order.jpeg', height = 15, width = 25,
       units = 'cm', device = 'jpeg', dpi = 600)






#have individual figures for the cultivars
enesmble_prediction_observed %>% 
  filter(species == 'Almond') %>% 
  mutate(cultivar = recode(cultivar, `Abiodh Ras Djebel` = 'A.R. Djebel',
                           `Fournat de Breznaud` = 'F. d. Breznaud')) %>% 
  ggplot(aes(x = pheno, y = pred_ensemble)) +
  geom_point() +
  geom_errorbar(aes(ymin = pred_ensemble - sd, ymax = pred_ensemble + sd)) +
  geom_abline(slope = 1) +
  facet_wrap(~cultivar)+
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  # geom_text(data = performance_ensemble,  y = 150, x = 1, 
  #           aes(label = paste('RMSE:', format(RMSE, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 140, x = 1, 
  #           aes(label = paste('RPIQ:', format(RPIQ, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 130, x = 1, 
  #           aes(label = paste('Mean Bias:', format(mean_bias, nsmall = 1))), hjust = 0) +
  scale_x_continuous(limits = c(0, 91),
                     breaks = c(1, 32, 60,91), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr')) +
  scale_y_continuous(limits = c(0, 91),
                     breaks = c(1, 32, 60,91), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr')) +
  theme_bw(base_size = 15) 
ggsave('figures/paper/ensemble_prediction_performance_almond.jpeg', height = 20, width = 27,
       units = 'cm', device = 'jpeg')

enesmble_prediction_observed %>% 
  filter(species == 'Apple') %>% 
  ggplot(aes(x = pheno, y = pred_ensemble)) +
  geom_point() +
  geom_errorbar(aes(ymin = pred_ensemble - sd, ymax = pred_ensemble + sd)) +
  geom_abline(slope = 1) +
  facet_wrap(~cultivar)+
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  # geom_text(data = performance_ensemble,  y = 150, x = 1, 
  #           aes(label = paste('RMSE:', format(RMSE, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 140, x = 1, 
  #           aes(label = paste('RPIQ:', format(RPIQ, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 130, x = 1, 
  #           aes(label = paste('Mean Bias:', format(mean_bias, nsmall = 1))), hjust = 0) +
  scale_x_continuous(limits = c(60, 152),
                     breaks = c(60,91, 121, 152), 
                     labels = c('Mar', 'Apr', 'May', 'Jun')) +
  scale_y_continuous(limits = c(60, 152),
                     breaks = c(60,91, 121, 152), 
                     labels = c('Mar', 'Apr', 'May', 'Jun')) +
  theme_bw(base_size = 15) 
ggsave('figures/paper/ensemble_prediction_performance_apple.jpeg', height = 15, width = 25,
       units = 'cm', device = 'jpeg')

enesmble_prediction_observed %>% 
  filter(species == 'Apricot') %>% 
  ggplot(aes(x = pheno, y = pred_ensemble)) +
  geom_point() +
  geom_errorbar(aes(ymin = pred_ensemble - sd, ymax = pred_ensemble + sd)) +
  geom_abline(slope = 1) +
  facet_wrap(~cultivar)+
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  # geom_text(data = performance_ensemble,  y = 150, x = 1, 
  #           aes(label = paste('RMSE:', format(RMSE, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 140, x = 1, 
  #           aes(label = paste('RPIQ:', format(RPIQ, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 130, x = 1, 
  #           aes(label = paste('Mean Bias:', format(mean_bias, nsmall = 1))), hjust = 0) +
  scale_x_continuous(limits = c(32, 91),
                     breaks = c(32, 60,91), 
                     labels = c('Feb', 'Mar', 'Apr')) +
  scale_y_continuous(limits = c(32, 91),
                     breaks = c( 32, 60,91), 
                     labels = c('Feb', 'Mar', 'Apr')) +
  theme_bw(base_size = 15) 
ggsave('figures/paper/ensemble_prediction_performance_apricot.jpeg', height = 15, width = 25,
       units = 'cm', device = 'jpeg')


enesmble_prediction_observed %>% 
  filter(species == 'European Plum') %>% 
  ggplot(aes(x = pheno, y = pred_ensemble)) +
  geom_point() +
  geom_errorbar(aes(ymin = pred_ensemble - sd, ymax = pred_ensemble + sd)) +
  geom_abline(slope = 1) +
  facet_wrap(~cultivar)+
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  # geom_text(data = performance_ensemble,  y = 150, x = 1, 
  #           aes(label = paste('RMSE:', format(RMSE, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 140, x = 1, 
  #           aes(label = paste('RPIQ:', format(RPIQ, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 130, x = 1, 
  #           aes(label = paste('Mean Bias:', format(mean_bias, nsmall = 1))), hjust = 0) +
  scale_x_continuous(limits = c(60, 152),
                     breaks = c(60,91, 121, 152), 
                     labels = c('Mar', 'Apr', 'May', 'Jun')) +
  scale_y_continuous(limits = c(60, 152),
                     breaks = c(60,91, 121, 152), 
                     labels = c('Mar', 'Apr', 'May', 'Jun')) +
  theme_bw(base_size = 15) 
ggsave('figures/paper/ensemble_prediction_performance_european-plum.jpeg', height = 10, width = 15,
       units = 'cm', device = 'jpeg')

# 
# enesmble_prediction_observed %>% 
#   filter(species == 'Japanese Plum') %>% 
#   ggplot(aes(x = pheno, y = pred_ensemble)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = pred_ensemble - sd, ymax = pred_ensemble + sd)) +
#   geom_abline(slope = 1) +
#   facet_wrap(~cultivar)+
#   ylab('Predicted Bloom Date') +
#   xlab('Observed Bloom Date') +
#   # geom_text(data = performance_ensemble,  y = 150, x = 1, 
#   #           aes(label = paste('RMSE:', format(RMSE, nsmall = 1))), hjust = 0) +
#   # geom_text(data = performance_ensemble,  y = 140, x = 1, 
#   #           aes(label = paste('RPIQ:', format(RPIQ, nsmall = 1))), hjust = 0) +
#   # geom_text(data = performance_ensemble,  y = 130, x = 1, 
#   #           aes(label = paste('Mean Bias:', format(mean_bias, nsmall = 1))), hjust = 0) +
#   scale_x_continuous(limits = c(60, 152),
#                      breaks = c(60,91, 121, 152), 
#                      labels = c('Mar', 'Apr', 'May', 'Jun')) +
#   scale_y_continuous(limits = c(60, 152),
#                      breaks = c(60,91, 121, 152), 
#                      labels = c('Mar', 'Apr', 'May', 'Jun')) +
#   theme_bw(base_size = 15) 
# ggsave('figures/paper/ensemble_prediction_performance_japanese-plum.jpeg', height = 15, width = 25,
#        units = 'cm', device = 'jpeg')



enesmble_prediction_observed %>% 
  filter(species == 'Pear') %>% 
  ggplot(aes(x = pheno, y = pred_ensemble)) +
  geom_point() +
  geom_errorbar(aes(ymin = pred_ensemble - sd, ymax = pred_ensemble + sd)) +
  geom_abline(slope = 1) +
  facet_wrap(~cultivar)+
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  # geom_text(data = performance_ensemble,  y = 150, x = 1, 
  #           aes(label = paste('RMSE:', format(RMSE, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 140, x = 1, 
  #           aes(label = paste('RPIQ:', format(RPIQ, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 130, x = 1, 
  #           aes(label = paste('Mean Bias:', format(mean_bias, nsmall = 1))), hjust = 0) +
  scale_x_continuous(limits = c(60, 152),
                     breaks = c(60,91, 121, 152), 
                     labels = c('Mar', 'Apr', 'May', 'Jun')) +
  scale_y_continuous(limits = c(60, 152),
                     breaks = c(60,91, 121, 152), 
                     labels = c('Mar', 'Apr', 'May', 'Jun')) +
  theme_bw(base_size = 15) 
ggsave('figures/paper/ensemble_prediction_performance_pear.jpeg', height = 15, width = 25,
       units = 'cm', device = 'jpeg')



enesmble_prediction_observed %>% 
  filter(species == 'Pistachio') %>% 
  ggplot(aes(x = pheno, y = pred_ensemble)) +
  geom_point() +
  geom_errorbar(aes(ymin = pred_ensemble - sd, ymax = pred_ensemble + sd)) +
  geom_abline(slope = 1) +
  facet_wrap(~cultivar)+
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  # geom_text(data = performance_ensemble,  y = 150, x = 1, 
  #           aes(label = paste('RMSE:', format(RMSE, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 140, x = 1, 
  #           aes(label = paste('RPIQ:', format(RPIQ, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 130, x = 1, 
  #           aes(label = paste('Mean Bias:', format(mean_bias, nsmall = 1))), hjust = 0) +
  scale_x_continuous(limits = c(60, 152),
                     breaks = c(60,91, 121, 152), 
                     labels = c('Mar', 'Apr', 'May', 'Jun')) +
  scale_y_continuous(limits = c(60, 152),
                     breaks = c(60,91, 121, 152), 
                     labels = c('Mar', 'Apr', 'May', 'Jun')) +
  theme_bw(base_size = 15) 
ggsave('figures/paper/ensemble_prediction_performance_pistachio.jpeg', height = 10, width = 15,
       units = 'cm', device = 'jpeg')


enesmble_prediction_observed %>% 
  filter(species == 'Sweet Cherry') %>% 
  mutate(cultivar = recode(cultivar, `Blanca de Provenza` = 'Blanca de Prov.',
                           `Early Van Compact` = 'Early V. Compact')) %>% 
  ggplot(aes(x = pheno, y = pred_ensemble)) +
  geom_point() +
  geom_errorbar(aes(ymin = pred_ensemble - sd, ymax = pred_ensemble + sd)) +
  geom_abline(slope = 1) +
  facet_wrap(~cultivar)+
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  # geom_text(data = performance_ensemble,  y = 150, x = 1, 
  #           aes(label = paste('RMSE:', format(RMSE, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 140, x = 1, 
  #           aes(label = paste('RPIQ:', format(RPIQ, nsmall = 1))), hjust = 0) +
  # geom_text(data = performance_ensemble,  y = 130, x = 1, 
  #           aes(label = paste('Mean Bias:', format(mean_bias, nsmall = 1))), hjust = 0) +
  scale_x_continuous(limits = c(60, 152),
                     breaks = c(60,91, 121, 152), 
                     labels = c('Mar', 'Apr', 'May', 'Jun')) +
  scale_y_continuous(limits = c(60, 152),
                     breaks = c(60,91, 121, 152), 
                     labels = c('Mar', 'Apr', 'May', 'Jun')) +
  theme_bw(base_size = 15) 
ggsave('figures/paper/ensemble_prediction_performance_sweet-cherry.jpeg', height = 20, width = 25,
       units = 'cm', device = 'jpeg')



#--------------------------------------------------------------#
# Time Windows Comparison of Methods####
#--------------------------------------------------------------#

#make the plot of the window comparison
#draw empty plot with the empirical time windows

#rm(list = ls())
#make plot of thermal time window

#make predictions for the actual weather data
cka <- read.csv('data/weather_ready/cka_clean.csv') %>% 
  filter(Year < 2022)
cieza <- read.csv('data/weather_ready/cieza_clean_patched.csv')
sfax <- read.csv('data/weather_ready/sfax_clean.csv')
meknes <- read.csv('data/weather_ready/meknes_clean.csv')
zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv') %>% 
  filter(Year < 2022)
santomera <- read.csv('data/weather_ready/murcia_clean.csv')


weather_list_obs <- list('Klein-Altendorf' = cka,
                         'Cieza' = cieza,
                         'Zaragoza' = zaragoza,
                         'Sfax' = sfax,
                         'Meknes' = meknes,
                         'Santomera' = santomera)
weather_list_pred <- weather_list_obs

adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv') %>% 
  filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),
         !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014)) %>% 
  mutate(species = recode(species, `Japanese plum` = 'European Plum'))


frost_threshold <- 0
heat_threshold <- 30
pad <- 0.05
observation_df <- adamedor %>% 
  filter(!(species %in% c( 'Peach', 'Olive')))

thermal_time_window <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) LarsChill::get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
                                                                                                                                weather_list_pred = weather_list_obs, 
                                                                                                                                observation_df = observation_df, 
                                                                                                                                frost_threshold = frost_threshold, 
                                                                                                                                heat_threshold = heat_threshold, 
                                                                                                                                target_col_obs = x,
                                                                                                                                padding = 0.01)) %>% 
  bind_rows()

adamedor_clean <- adamedor %>% 
  filter(!(species %in% c( 'Peach', 'Olive'))) %>% 
  dplyr::select(species, cultivar, location, begin_flowering_f5, flowering_f50) %>% 
  reshape2::melt(id.vars = c('species', 'cultivar', 'location'), variable.name = 'flowering_type') %>% 
  mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         species = stringr::str_to_title(species),
         flowering_label = recode(flowering_type, begin_flowering_f5 = '10% Flowering',
                                  flowering_f50 = '50% Flowering'),
         species_label = recode(species, 
                                `European Plum` = 'Europ. Plum'),
         value = lubridate::yday(value)) %>% 
  na.omit() %>% 
  mutate(species_label = factor(species_label, levels =c('Apple','Pear',  'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')))

#placeholder for na
na_val <- 9999

#maybe rather use the modelled data and indicate type I, type II and neutral...
pheno_current <- read.csv('data/projected_bloomdates_ensemble_observed_weather.csv') %>% 
  mutate(species = tolower(species),
         species = recode(species, `japanese plum` = 'european plum'),
         flowering_type = ifelse(species %in% c('apple', 'almond', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50')) %>% 
  mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         species = stringr::str_to_title(species),
         flowering_label = recode(flowering_type, begin_flowering_f5 = '10% Flowering',
                                  flowering_f50 = '50% Flowering'),
         species_label = recode(species, 
                                `European Plum` = 'Europ. Plum')) %>% 
  mutate(species_label = factor(species_label, levels =c('Apple','Pear',  'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
         pheno_predicted = replace_na(pheno_predicted, na_val)) %>% 
  filter(cultivar %in% cultivar_n)


length(unique(pheno_current$cultivar))
#maybe set everything after august to august 1st
#or treat na predictions in a second category

#prepare and merge data
pheno_and_window <- thermal_time_window %>% 
  mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         species = stringr::str_to_title(species),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry',  'Almond', 'Pistachio' ))), 
         flowering_label = recode(flowering_type, begin_flowering_f5 = '10% Flowering',
                                  flowering_f50 = '50% Flowering'),
         species_label = recode(species, 
                                `European Plum` = 'Europ. Plum')) %>% 
  mutate(species_label = factor(species_label, levels =c('Apple','Pear',  'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))) %>% 
  mutate(spec_loc = paste0(species, '_', location),
         fill_label = ifelse(spec_loc %in% c('Almond_Klein-Altendorf', 'Pistachio_Klein-Altendorf', 'Almond_Zaragoza', 'Pistachio_Zaragoza',
                                             'Apple_Sfax', 'Pear_Sfax', 'Apricot_Sfax', 'European Plum_Sfax', 'Sweet Cherry_Sfax'), yes = 'Type1', 
                             no = ifelse(spec_loc %in%    c('Almond_Meknes', 'Almond_Sfax', 'Almond_Santomera',
                                           'Apple_Meknes', 'Apple_Klein-Altendorf',
                                           'Pear_Klein-Altendorf', 'Pear_Zaragoza',
                                           'Apricot_Cieza', 'Apricot_Zaragoza',
                                           'Sweet Cherry_Zaragoza', 'Sweet Cherry-Klein-Altendorf',
                                           'European Plum_Klein-Altendorf',
                                           'Pistachio_Sfax'), yes = 'Type2', no = 'Other'))) %>% 
  merge(pheno_current, by = c('species', 'species_label', 'loc', 'location', 'flowering_type', 'flowering_label'), all.y = TRUE)

#summarize the failure under current conditions
fail_sum <- pheno_and_window %>% 
  mutate(out = pheno_predicted > 213) %>% 
  group_by(species, species_label, location, loc, spec) %>% 
  summarise(out = (sum(out) / n())* 100) 



# pheno_and_window %>% 
#   merge(fail_sum, by = c('species', 'species_label', 'location', 'loc', 'spec')) %>% 
#   filter(pheno_predicted <= 213) %>% 
#   ggplot(aes(y = loc)) +
#   #geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = loc-0.4, ymax = loc+0.4),fill = 'grey60') +
#   geom_rect(aes(xmin = min(pheno_predicted)-10, xmax = min_doy_padded - 1, ymin = loc-0.5, ymax = loc+0.5),fill = 'grey60') +
#   geom_rect(aes(xmin = max_doy_padded + 1, xmax = 243, ymin = loc-0.5, ymax = loc+0.5),fill = 'grey60') +
#   geom_boxplot(aes(y = loc, x = pheno_predicted, group = loc, fill = fill_label), width = 0.6) +
#   geom_point(aes(x = 213 + 15, y = loc,  size = out)) +
#   scale_y_reverse(breaks = 1:6, labels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'), ) +
#   scale_x_continuous(breaks=        c(-31,  0, 32, 60,  91, 121, 152, 182, 213), 
#                      minor_breaks = c(-45, -15, 15, 46, 74, 105, 135, 166, 194), 
#                      labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug')) +
#   #scale_fill_manual(values = c("#009E73", "#009E73", "#E69F00","#E69F00" ,"#E69F00", "#56B4E9", "#56B4E9" ) )+
#   xlab('Month') +
#   ylab('Location') +
#   facet_grid(species_label~.) +
#   theme_bw(base_size = 15) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')  +
#   coord_cartesian(xlim = c(-45, 230))


alpha_fail <- 'low'
alpha_okay <- 'high'

pheno_and_window %>% 
  merge(fail_sum, by = c('species', 'species_label', 'location', 'loc', 'spec')) %>% 
  mutate(location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         alpha = ifelse(out >= 50, yes = alpha_fail, no = alpha_okay),
         max_doy_padded = ifelse(max_doy_padded >= 182, yes = 181, no = max_doy_padded)) %>% 
  filter(pheno_predicted <= 182) %>% 
  ggplot(aes(y = spec)) +
  #geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = spec-0.4, ymax = spec+0.4),fill = 'grey60') +
  geom_rect(aes(xmin = min(pheno_predicted)-10, xmax = min_doy_padded - 1, ymin = spec-0.5, ymax = spec+0.5),fill = 'grey70') +
  geom_rect(aes(xmin = max_doy_padded + 1, xmax = 242, ymin = spec-0.5, ymax = spec+0.5),fill = 'grey70') +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.5),fill = 'grey70') +
  geom_boxplot(aes(y = spec, x = pheno_predicted, group = spec, fill = as.factor(spec),
                   alpha = alpha), width = 0.6, show.legend = FALSE) +
  scale_alpha_discrete(range=c(1,0.2)) +
  geom_text(y = 0, x = 182 + 2, 
            aes(label = 'No Prediction'),hjust = 0) +
  geom_text(aes(x = 182 + 10, y = spec,  
                label = paste(format(round(out, digits = 1), nsmall = 1), '%'))) +
  scale_y_reverse(breaks = 1:7, labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')) +
  scale_x_continuous(breaks=        c(-31,  0, 32, 60,  91, 121, 152), 
                     minor_breaks = c(-45, -15, 15, 46, 74, 105, 135, 166), 
                     labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  scale_fill_manual(values = c("#009E73", "#009E73", "#E69F00","#E69F00" ,"#E69F00", "#56B4E9", "#56B4E9" ) )+
  scale_size_area(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100),max_size = 8) +
  geom_vline(xintercept = 182, linetype = 'dashed')+
  xlab('Month') +
  ylab('') +
  facet_grid(location~.) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')  +
  coord_cartesian(xlim = c(-45, 195),
                  ylim = c(7, 0))
ggsave('figures/paper/time_window_and_current_prediction.jpeg', height = 35, width = 35, units = 'cm', device = 'jpeg')


master <- read.csv('data/master_phenology_repeated_splits.csv') %>% 
  filter(repetition == 1) %>% 
  group_by(species, cultivar) %>% 
  summarise(location = unique(location)) %>% 
  mutate(cultivar_type = 'local calibration')

fail_sum <- pheno_and_window %>% 
  merge(master, by = c('species', 'cultivar', 'location'), all.x = TRUE) %>% 
  mutate(out = pheno_predicted > 213,
         cultivar_type = replace_na(cultivar_type, 'projected')) %>% 
  group_by(species, species_label, location, loc, spec, cultivar_type) %>% 
  summarise(out = (sum(out) / n())* 100) %>% 
  mutate(location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')))





twindow <- thermal_time_window %>% 
  mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         species = stringr::str_to_title(species),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry',  'Almond', 'Pistachio' ))), 
         flowering_label = recode(flowering_type, begin_flowering_f5 = '10% Flowering',
                                  flowering_f50 = '50% Flowering'),
         species_label = recode(species, 
                                `European Plum` = 'Europ. Plum')) %>% 
  mutate(species_label = factor(species_label, levels =c('Apple','Pear',  'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))) %>% 
  mutate(spec_loc = paste0(species, '_', location),
         fill_label = ifelse(spec_loc %in% c('Almond_Klein-Altendorf', 'Pistachio_Klein-Altendorf', 'Almond_Zaragoza', 'Pistachio_Zaragoza',
                                             'Apple_Sfax', 'Pear_Sfax', 'Apricot_Sfax', 'European Plum_Sfax', 'Sweet Cherry_Sfax'), yes = 'Type1', 
                             no = ifelse(spec_loc %in%    c('Almond_Meknes', 'Almond_Sfax', 'Almond_Santomera',
                                                            'Apple_Meknes', 'Apple_Klein-Altendorf',
                                                            'Pear_Klein-Altendorf', 'Pear_Zaragoza',
                                                            'Apricot_Cieza', 'Apricot_Zaragoza',
                                                            'Sweet Cherry_Zaragoza', 'Sweet Cherry-Klein-Altendorf',
                                                            'European Plum_Klein-Altendorf',
                                                            'Pistachio_Sfax'), yes = 'Type2', no = 'Other')))

twindow2 <- twindow %>% 
  mutate(cultivar_type = 'projected')

twindow <- twindow %>% 
  mutate(cultivar_type = 'local calibration') %>% 
  rbind(twindow2) %>% 
  mutate(year = NA,
         pheno_predicted = NA,
         sd = NA,
         gcm = NA,
         ssp = NA,
         scenario_year = NA,
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         cultivar = NA,
         max_doy_padded = ifelse(max_doy_padded >= 182, yes = 181, no = max_doy_padded))

#colnames(twindow) %in% colnames(test)



pheno_and_window %>% 
  merge(master, by = c('species', 'cultivar', 'location'), all.x = TRUE) %>% 
  mutate( cultivar_type = replace_na(cultivar_type, 'projected')) %>% 
  mutate(location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         max_doy_padded = ifelse(max_doy_padded >= 182, yes = 181, no = max_doy_padded)) %>% 
  filter(pheno_predicted <= 182) %>% 
  rbind(twindow) %>% 
  ggplot(aes(y = spec)) +
  geom_rect(aes(xmin = min(pheno_predicted, na.rm = TRUE)-10, xmax = min_doy_padded - 1, ymin = spec-0.5, ymax = spec+0.5),fill = 'grey70') +
  geom_rect(aes(xmin = max_doy_padded + 1, xmax = 212, ymin = spec-0.5, ymax = spec+0.5),fill = 'grey70') +
  #geom_tile(aes(xmin = 213, xmax = 242, ymin = spec-0.5, ymax = spec+0.5, fill = out)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.5),fill = 'grey70') +
  geom_rect(aes(xmin =182, xmax = 220, ymin = -Inf, ymax = Inf),fill = 'white') +
  geom_point(data = fail_sum, aes(x=205, y= spec, color=out), shape=15, size=7.5) +
  geom_point(data = fail_sum,aes(x=202, y= spec, color=out), shape=15, size=7.5) +
  geom_point(data = fail_sum,aes(x=199, y= spec, color=out), shape=15, size=7.5) +
  geom_point(data = fail_sum,aes(x=196, y= spec, color=out), shape=15, size=7.5) +
  geom_point(data = fail_sum,aes(x=193, y= spec, color=out), shape=15, size=7.5) +
  geom_point(data = fail_sum,aes(x=190, y= spec, color=out), shape=15, size=7.5) +
  geom_point(data = fail_sum,aes(x=187, y= spec, color=out), shape=15, size=7.5) +
  #geom_point(data = fail_sum,aes(x=186, y= spec, color=out), shape=15, size=7.5) +
  geom_boxplot(aes(y = spec, x = pheno_predicted, group = spec, fill = as.factor(spec)), width = 0.6, show.legend = FALSE) +
  geom_text(y = 0, x = 182 + 1, 
            aes(label = 'No Pred.'),hjust = 0) +
  geom_text(data = fail_sum, aes(x = 182 + 13, y = spec,  
                label = paste0(format(round(out, digits = 1), nsmall = 1), '%'))) +
  scale_y_reverse(breaks = 1:7, labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')) +
  scale_x_continuous(breaks=        c(-31,  0, 32, 60,  91, 121, 152), 
                     minor_breaks = c(-45, -15, 15, 46, 74, 105, 135, 166), 
                     labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  scale_fill_manual(values = c("#009E73", "#009E73", "#E69F00","#E69F00" ,"#E69F00", "#56B4E9", "#56B4E9" ) )+
  scale_colour_gradient(low = "white", high = "firebrick2", limits = c(0, 100), name = 'Cases thermal requirements were not met (%)') +
  scale_size_area(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100),max_size = 8) +
  geom_vline(xintercept = 182)+
  xlab('Month') +
  ylab('') +
  facet_grid(location~cultivar_type) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')  +
  coord_cartesian(xlim = c(-45, 199),
                  ylim = c(7, 0))
ggsave('figures/paper/time_window_and_current_prediction_v4.jpeg', height = 35, width = 35, units = 'cm', device = 'jpeg')
















pheno_and_window %>% 
  merge(fail_sum, by = c('species', 'species_label', 'location', 'loc', 'spec')) %>% 
  mutate(location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         alpha = ifelse(out >= 50, yes = alpha_fail, no = alpha_okay),
         max_doy_padded = ifelse(max_doy_padded >= 182, yes = 181, no = max_doy_padded)) %>% 
  filter(pheno_predicted <= 182) %>% 
  ggplot(aes(y = spec)) +
  #geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = spec-0.4, ymax = spec+0.4),fill = 'grey60') +
  geom_rect(aes(xmin = min(pheno_predicted)-10, xmax = min_doy_padded - 1, ymin = spec-0.5, ymax = spec+0.5),fill = 'grey70') +
  geom_rect(aes(xmin = max_doy_padded + 1, xmax = 212, ymin = spec-0.5, ymax = spec+0.5),fill = 'grey70') +
  #geom_tile(aes(xmin = 213, xmax = 242, ymin = spec-0.5, ymax = spec+0.5, fill = out)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.5),fill = 'grey70') +
  geom_rect(aes(xmin =182, xmax = 220, ymin = -0.5, ymax = 0.5),fill = 'white') +
  geom_point(aes(x=205, y= spec, color=out), shape=15, size=7.5) +
  geom_point(aes(x=202, y= spec, color=out), shape=15, size=7.5) +
  geom_point(aes(x=199, y= spec, color=out), shape=15, size=7.5) +
  geom_point(aes(x=196, y= spec, color=out), shape=15, size=7.5) +
  geom_point(aes(x=193, y= spec, color=out), shape=15, size=7.5) +
  geom_point(aes(x=190, y= spec, color=out), shape=15, size=7.5) +
  geom_point(aes(x=187, y= spec, color=out), shape=15, size=7.5) +
  geom_point(aes(x=184.5, y= spec, color=out), shape=15, size=7.5) +
  scale_colour_gradient(low = "white", high = "firebrick2", limits = c(0, 100)) +
  geom_boxplot(aes(y = spec, x = pheno_predicted, group = spec, fill = as.factor(spec)), width = 0.6, show.legend = FALSE) +
  scale_alpha_discrete(range=c(1,0.2)) +
  geom_text(y = 0, x = 182 + 2, 
            aes(label = 'No Prediction'),hjust = 0) +
  geom_text(aes(x = 182 + 10, y = spec,  
                label = paste(format(round(out, digits = 1), nsmall = 1), '%'))) +
  scale_y_reverse(breaks = 1:7, labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')) +
  scale_x_continuous(breaks=        c(-31,  0, 32, 60,  91, 121, 152), 
                     minor_breaks = c(-45, -15, 15, 46, 74, 105, 135, 166), 
                     labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  scale_fill_manual(values = c("#009E73", "#009E73", "#E69F00","#E69F00" ,"#E69F00", "#56B4E9", "#56B4E9" ) )+
  scale_size_area(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100),max_size = 8) +
  geom_vline(xintercept = 182)+
  xlab('Month') +
  ylab('') +
  facet_grid(location~.) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')  +
  coord_cartesian(xlim = c(-45, 195),
                  ylim = c(7, 0))
ggsave('figures/paper/time_window_and_current_prediction_v3.jpeg', height = 35, width = 35, units = 'cm', device = 'jpeg')









  


pheno_and_window <- test %>% 
  mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
         species = stringr::str_to_title(species),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry',  'Almond', 'Pistachio' ))),
         species_label = recode(species, 
                                `European Plum` = 'Europ. Plum')) %>% 
  mutate(species_label = factor(species_label, levels =c('Apple','Pear',  'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))) %>% 
  mutate(spec_loc = paste0(species, '_', location),
         fill_label = ifelse(spec_loc %in% c('Almond_Klein-Altendorf', 'Pistachio_Klein-Altendorf', 'Almond_Zaragoza', 'Pistachio_Zaragoza',
                                             'Apple_Sfax', 'Pear_Sfax', 'Apricot_Sfax', 'European Plum_Sfax', 'Sweet Cherry_Sfax'), yes = 'Type1', 
                             no = ifelse(spec_loc %in%    c('Almond_Meknes', 'Almond_Sfax', 'Almond_Santomera',
                                                            'Apple_Meknes', 'Apple_Klein-Altendorf',
                                                            'Pear_Klein-Altendorf', 'Pear_Zaragoza',
                                                            'Apricot_Cieza', 'Apricot_Zaragoza',
                                                            'Sweet Cherry_Zaragoza', 'Sweet Cherry-Klein-Altendorf',
                                                            'European Plum_Klein-Altendorf',
                                                            'Pistachio_Sfax'), yes = 'Type2', no = 'Other'))) %>% 
  merge(pheno_current, by = c('species', 'species_label', 'loc', 'location'), all.y = TRUE)



# pheno_and_window %>% 
#   merge(fail_sum, by = c('species', 'species_label', 'location', 'loc', 'spec')) %>% 
#   mutate(location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
#          alpha = ifelse(out >= 50, yes = alpha_fail, no = alpha_okay),
#          max_doy_padded = ifelse(max_doy_padded >= 182, yes = 181, no = max_doy_padded)) %>% 
#   filter(pheno_predicted <= 182) %>% 
#   ggplot(aes(y = spec)) +
#   #geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = spec-0.4, ymax = spec+0.4),fill = 'grey60') +
#   geom_rect(aes(xmin = min(pheno_predicted)-10, xmax = min_doy_padded - 1, ymin = spec-0.5, ymax = spec+0.5),fill = 'grey70') +
#   geom_rect(aes(xmin = max_doy_padded + 1, xmax = 243, ymin = spec-0.5, ymax = spec+0.5),fill = 'grey70') +
#   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.5),fill = 'grey70') +
#   geom_boxplot(aes(y = spec, x = pheno_predicted, group = spec, fill = as.factor(spec),
#                    alpha = alpha), width = 0.6, show.legend = FALSE) +
#   scale_alpha_discrete(range=c(1,0.2)) +
#   geom_text(y = 0, x = 182 + 2, 
#             aes(label = 'No Prediction'),hjust = 0) +
#   geom_text(aes(x = 182 + 10, y = spec,  
#                 label = paste(format(round(out, digits = 1), nsmall = 1), '%'))) +
#   scale_y_reverse(breaks = 1:7, labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')) +
#   scale_x_continuous(breaks=        c(-31,  0, 32, 60,  91, 121, 152, 182), 
#                      minor_breaks = c(-45, -15, 15, 46, 74, 105, 135, 166), 
#                      labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul')) +
#   scale_fill_manual(values = c("#009E73", "#009E73", "#E69F00","#E69F00" ,"#E69F00", "#56B4E9", "#56B4E9" ) )+
#   scale_size_area(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100),max_size = 8) +
#   geom_vline(xintercept = 182, linetype = 'dashed')+
#   xlab('Month') +
#   ylab('') +
#   facet_grid(location~.) +
#   theme_bw(base_size = 15) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')  +
#   coord_cartesian(xlim = c(-45, 195),
#                   ylim = c(7, 0))
# ggsave('figures/paper/current_prediction_fixed_rsik_timewindow.jpeg', height = 35, width = 35, units = 'cm', device = 'jpeg')


#split the figure based for which locations the models have been fitted

cult_loc <- read.csv('data/master_phenology_repeated_splits.csv') %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum')) %>% 
  group_by(species, cultivar) %>% 
  summarise(location = unique(location)) %>% 
  mutate(type = 'l')
  


pheno_and_window %>% 
  merge(fail_sum, by = c('species', 'species_label', 'location', 'loc', 'spec')) %>% 
  mutate(location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         alpha = ifelse(out >= 50, yes = alpha_fail, no = alpha_okay),
         max_doy_padded = ifelse(max_doy_padded >= 182, yes = 181, no = max_doy_padded)) %>% 
  merge(cult_loc, by = c('species', 'location', 'cultivar'), all.x = TRUE) %>% 
  mutate(type = replace_na(type, 'p')) %>% 
  filter(pheno_predicted <= 182) %>% 
  filter((species %in% c('Apple', 'Pear', 'European Plum', 'Sweet Cherry') & location == 'Klein-Altendorf')|
           (species %in% c('Pear', 'Apricot', 'Sweet Cherry') & location == 'Zaragoza') |
           (species == 'Apricot' & location == 'Cieza') |
           (species == 'Almond' & location == 'Santomera') |
           (species %in% c('Apple', 'Almond') & location == 'Meknes') |
           (species %in% c('Almond', 'Pistachio') & location == 'Sfax')) %>% 
  ggplot(aes(y = spec)) +
  #geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = spec-0.4, ymax = spec+0.4),fill = 'grey60') +
  geom_boxplot(aes(y = spec, x = pheno_predicted, fill = paste0(type,spec), group = paste0(type,spec)), width = 0.6, show.legend = FALSE) +
  scale_alpha_discrete(range=c(1,0.2)) +
  geom_text(y = 0, x = 182 + 2, 
            aes(label = 'No Prediction'),hjust = 0) +
  geom_text(aes(x = 182 + 10, y = spec,  
                label = paste(format(round(out, digits = 1), nsmall = 1), '%'))) +
  scale_y_reverse(breaks = 1:7, labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')) +
  scale_x_continuous(breaks=        c(-31,  0, 32, 60,  91, 121, 152, 182), 
                     minor_breaks = c(-45, -15, 15, 46, 74, 105, 135, 166), 
                     labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', '')) +
  scale_fill_manual(values = c("#009E73", "#009E73", "#E69F00","#E69F00" ,"#E69F00", "#56B4E9", "#56B4E9",
                               "#00ebab", "#00ebab", "#ffc034" ,"#ffc034", "#9ad2f2", "#9ad2f2") )+
  scale_size_area(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100),max_size = 8) +
  geom_vline(xintercept = 182, linetype = 'dashed')+
  xlab('Month') +
  ylab('') +
  facet_grid(location~., scales = 'free_y') +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')  +
  coord_cartesian(xlim = c(-5, 195))
ggsave('figures/distressing_stuff.jpeg', height = 25, width = 20, units = 'cm', device = 'jpeg')



pheno_and_window %>% 
  merge(fail_sum, by = c('species', 'species_label', 'location', 'loc', 'spec')) %>% 
  mutate(location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         alpha = ifelse(out >= 50, yes = alpha_fail, no = alpha_okay),
         max_doy_padded = ifelse(max_doy_padded >= 182, yes = 181, no = max_doy_padded)) %>% 
  merge(cult_loc, by = c('species', 'location', 'cultivar'), all.x = TRUE) %>% 
  mutate(type = replace_na(type, 'p')) %>% 
  filter(pheno_predicted <= 182) %>% 
  ggplot(aes(y = spec)) +
  #geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = spec-0.4, ymax = spec+0.4),fill = 'grey60') +
  geom_boxplot(aes(y = spec, x = pheno_predicted, fill = paste0(type,spec), group = paste0(type,spec)), width = 0.6, show.legend = FALSE) +
  scale_alpha_discrete(range=c(1,0.2)) +
  geom_text(y = 0, x = 182 + 2, 
            aes(label = 'No Prediction'),hjust = 0) +
  geom_text(aes(x = 182 + 10, y = spec,  
                label = paste(format(round(out, digits = 1), nsmall = 1), '%'))) +
  scale_y_reverse(breaks = 1:7, labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')) +
  scale_x_continuous(breaks=        c(-31,  0, 32, 60,  91, 121, 152, 182), 
                     minor_breaks = c(-45, -15, 15, 46, 74, 105, 135, 166), 
                     labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul')) +
  scale_fill_manual(values = c("#009E73", "#009E73", "#E69F00","#E69F00" ,"#E69F00", "#56B4E9", "#56B4E9",
                               "#00ebab", "#00ebab", "#ffc034","#ffc034" ,"#ffc034", "#9ad2f2", "#9ad2f2") )+
  scale_size_area(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100),max_size = 8) +
  geom_vline(xintercept = 182, linetype = 'dashed')+
  xlab('Month') +
  ylab('') +
  facet_grid(location~.) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')  +
  coord_cartesian(xlim = c(-45, 195),
                  ylim = c(7, 0))


ggsave('figures/paper/current_prediction_no_timewindow.jpeg', height = 35, width = 35, units = 'cm', device = 'jpeg')



# 
# pheno_and_window %>% 
#   merge(fail_sum, by = c('species', 'species_label', 'location', 'loc', 'spec')) %>% 
#   mutate(location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
#          alpha = ifelse(out >= 50, yes = alpha_fail, no = alpha_okay),
#          max_doy_padded = ifelse(max_doy_padded >= 182, yes = 181, no = max_doy_padded)) %>% 
#   mutate(pheno_predicted = recode(pheno_predicted, `9999` = 185)) %>% 
#   ggplot(aes(y = spec)) +
#   #geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = spec-0.4, ymax = spec+0.4),fill = 'grey60') +
#   geom_rect(aes(xmin = min(pheno_predicted)-10, xmax = min_doy_padded - 1, ymin = spec-0.5, ymax = spec+0.5),fill = 'grey70') +
#   geom_rect(aes(xmin = max_doy_padded + 1, xmax = 243, ymin = spec-0.5, ymax = spec+0.5),fill = 'grey70') +
#   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.5),fill = 'grey70') +
#   geom_boxplot(aes(y = spec, x = pheno_predicted, group = spec, fill = as.factor(spec)), width = 0.6, show.legend = FALSE) +
#   geom_text(y = 0, x = 182 + 2, 
#             aes(label = 'No Prediction'),hjust = 0) +
#   geom_text(aes(x = 182 + 10, y = spec,  
#                 label = paste(format(round(out, digits = 1), nsmall = 1), '%'))) +
#   scale_y_reverse(breaks = 1:7, labels = c('Apple', 'Pear', 'Apricot', 'Europ. Plum', 'Sweet Cherry', 'Almond', 'Pistachio')) +
#   scale_x_continuous(breaks=        c(-31,  0, 32, 60,  91, 121, 152, 182), 
#                      minor_breaks = c(-45, -15, 15, 46, 74, 105, 135, 166), 
#                      labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul')) +
#   scale_fill_manual(values = c("#009E73", "#009E73", "#E69F00","#E69F00" ,"#E69F00", "#56B4E9", "#56B4E9" ) )+
#   scale_size_area(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100),max_size = 8) +
#   geom_vline(xintercept = 182, linetype = 'dashed')+
#   xlab('Month') +
#   ylab('') +
#   facet_grid(location~.) +
#   theme_bw(base_size = 15) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')  +
#   coord_cartesian(xlim = c(-45, 195),
#                   ylim = c(7, 0))
# 
# ggsave('figures/paper/time_window_and_current_prediction_v2.jpeg', height = 35, width = 35, units = 'cm', device = 'jpeg')





#----------------------------------#
#change bloom date
#----------------------------------#

pheno_2015 <- read.csv('data/projected_bloomdates_ensemble_historic_scenarios.csv') %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum')) %>% 
  filter(cultivar %in% cultivar_n)

pheno_future <- read.csv('data/projected_bloomdates_ensemble.csv') %>% 
  separate(species_cultivar, into = c('species', 'cultivar'), sep = '_') %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum')) %>% 
  filter(cultivar %in% cultivar_n) %>% 
  rename(pheno_hist = pheno_predicted) 


#calculate median for both
median_2015 <- pheno_2015 %>% 
  group_by(species, location) %>% 
  summarise(med_current = median(pheno_predicted, na.rm = TRUE),
            sd_current = sd(pheno_predicted, na.rm = TRUE))

median_future <- pheno_future %>% 
  group_by(species, location, ssp, gcm, scenario_year) %>% 
  summarise(med_future = median(pheno_hist, na.rm = TRUE),
            sd_future = sd(pheno_hist, na.rm = TRUE))

shift_df <- merge(median_2015, median_future, by = c('species','location')) %>% 
  mutate(shift_bloom = round(med_future - med_current, digits = 2))

shift_table <- shift_df %>% 
  group_by(species, location, scenario_year, ssp) %>% 
  summarise(min_shift = min(shift_bloom, na.rm = TRUE),
            max_shift = max(shift_bloom, na.rm = TRUE))

write.csv(shift_table, 'data/summary_shift_bloom_date_text.csv', row.names = FALSE)


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
ggplot2::ggsave('figures/paper/change_med_bloom_2050_v1.jpeg', device = 'jpeg',
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
ggplot2::ggsave('figures/paper/change_med_bloom_2085_v1.jpeg', device = 'jpeg',
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
ggplot2::ggsave('figures/paper/change_med_bloom_2050_v2.jpeg', device = 'jpeg',
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
ggplot2::ggsave('figures/paper/change_med_bloom_2085_v2.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')



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




# #change that everything after august is NA, this makes the plot also more readable
# #add error bar, maybe do not summarize the gcm but have them as separate points, just more clearly indicate them with black outline but same color filling or so
# 
# 
# shift_df %>% 
#   mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
#          location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
#          spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
#          loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
#          med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
#   filter(scenario_year == '2085') %>% 
#   ggplot(ggplot2::aes(y = spec)) +
#   geom_rect(aes(xmin = med_current, xmax = med_future, ymin = spec + dodge_low, ymax = spec + dodge_up, fill = ssp)) +
#   geom_point(aes(x = med_future, y = spec + ((dodge_low + dodge_up)/2),  col = ssp),
#              col = 'black',
#              show.legend = FALSE, shape = 4, size = 1) + 
#   geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = spec - 0.4, ymin = spec + 0.4, fill = 'Simulation 2015'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(location~., scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
#                      labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun'),
#                      minor_breaks = c(1, 32, 60, 91, 121, 152)) +
#   scale_y_reverse(breaks = rep(1:7,6), labels = rep(c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'), 6)) +
#   coord_cartesian(xlim = c(32, 155)) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Median Predicted Bloom Date') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# 
# 
# shift_df %>% 
#   mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
#          location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
#          spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
#          loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
#          med_future = ifelse(is.na(med_current), yes = NA, no = med_future),
#          ssp_year = factor(paste(ssp, scenario_year, sep = '_'))) %>% 
#   ggplot(ggplot2::aes(y = spec)) +
#   geom_rect(aes(xmin = med_current, xmax = med_future, ymin = spec + dodge_low, ymax = spec + dodge_up, fill = ssp_year)) +
#   geom_point(aes(x = med_future, y = spec + ((dodge_low + dodge_up)/2),  col = ssp,
#                  shape = as.character(scenario_year)),
#              col = 'black',
#              show.legend = FALSE, size = 1) + 
#   geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = spec - 0.4, ymin = spec + 0.4, fill = 'Simulation 2015'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(location~., scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c("#56B4E9", '#177BB1', "#009E73",'#007556',  "#F0E442", '#C7BA05',  "#E69F00", '#B37A00'))+
#   scale_fill_manual(values = c('black', '#177BB1', "#56B4E9", '#007556', "#009E73",  '#C7BA05', "#F0E442", '#B37A00',  "#E69F00", 'grey70'),
#                               breaks = c('Simulation 2015', 'ssp126_2085', 'ssp126_2050', 'ssp245_2085', 'ssp245_2050', 'ssp370_2085', 'ssp370_2050', 'ssp585_2085', 'ssp585_2050'))+
#   #)+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
#                      labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun'),
#                      minor_breaks = c(1, 32, 60, 91, 121, 152)) +
#   scale_y_reverse(breaks = rep(1:7,6), labels = rep(c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'), 6)) +
#   coord_cartesian(xlim = c(32, 155)) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'right') +
#   ylab('') +
#   xlab('Median Predicted Bloom Date') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# 
# 
# 
# 
# 
# 
# #get the extreme points to draw the rectangles
# ext_df <- shift_df %>% 
#   group_by(scenario_year, ssp, location,species) %>% 
#   summarise(ext_high = max(med_future, na.rm= TRUE),
#             ext_low = min(med_future, na.rm = TRUE)) %>% 
#   mutate(ext_high = replace(ext_high, is.infinite(ext_high), NA),
#          ext_low = replace(ext_low, is.infinite(ext_low), NA))


#-----------------------------#
#shift of bloom date on cultivar level
#-----------------------------#


#calculate median for both
median_2015 <- pheno_2015 %>% 
  group_by(species, cultivar, location) %>% 
  summarise(med_current = median(pheno_predicted, na.rm = TRUE),
            sd_current = sd(pheno_predicted, na.rm = TRUE))




median_future <- pheno_future %>% 
  group_by(species, cultivar, location, ssp, gcm, scenario_year) %>% 
  summarise(med_future = median(pheno_hist, na.rm = TRUE),
            sd_future = sd(pheno_hist, na.rm = TRUE))

shift_df <- merge(median_2015, median_future, by = c('species', 'cultivar', 'location')) %>% 
  mutate(shift_bloom = round(med_future - med_current, digits = 2))

#get number of cultivars
ncult <- shift_df %>% 
  group_by(species) %>% 
  summarise(cult =unique(cultivar)) %>% 
  ungroup() %>% 
  nrow()

#assign number to cultivars, keep order as in the plot
cult_num_df <- shift_df %>% 
  group_by(species) %>% 
  summarise(cultivar =unique(cultivar)) %>% 
  ungroup() %>% 
  mutate(cult_num  =ncult:1)


#position label
pos_label_almond <- c(min(which(cult_num_df$species == 'Almond')), max(which(cult_num_df$species == 'Almond')))
pos_plot_almond <- c(cult_num_df$cult_num[pos_label_almond[1]], cult_num_df$cult_num[pos_label_almond[2]])

pos_label_apple <- c(min(which(cult_num_df$species == 'Apple')), max(which(cult_num_df$species == 'Apple')))
pos_plot_apple <- c(cult_num_df$cult_num[pos_label_apple[1]], cult_num_df$cult_num[pos_label_apple[2]])

pos_label_apricot <- c(min(which(cult_num_df$species == 'Apricot')), max(which(cult_num_df$species == 'Apricot')))
pos_plot_apricot <- c(cult_num_df$cult_num[pos_label_apricot[1]], cult_num_df$cult_num[pos_label_apricot[2]])

pos_label_plum <- c(min(which(cult_num_df$species == 'European Plum')), max(which(cult_num_df$species == 'European Plum')))
pos_plot_plum <- c(cult_num_df$cult_num[pos_label_plum[1]], cult_num_df$cult_num[pos_label_plum[2]])

pos_label_sweet_cherry <- c(min(which(cult_num_df$species == 'Sweet Cherry')), max(which(cult_num_df$species == 'Sweet Cherry')))
pos_plot_sweet_cherry <- c(cult_num_df$cult_num[pos_label_sweet_cherry[1]], cult_num_df$cult_num[pos_label_sweet_cherry[2]])

pos_label_pear <- c(min(which(cult_num_df$species == 'Pear')), max(which(cult_num_df$species == 'Pear')))
pos_plot_pear <- c(cult_num_df$cult_num[pos_label_pear[1]], cult_num_df$cult_num[pos_label_pear[2]])

pos_label_pistachio <- c(min(which(cult_num_df$species == 'Pistachio')), max(which(cult_num_df$species == 'Pistachio')))
pos_plot_pistachio <- c(cult_num_df$cult_num[pos_label_pistachio[1]], cult_num_df$cult_num[pos_label_pistachio[2]])


p1 <- shift_df %>% 
  merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi'),
         species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry')),
         location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
                           Meknes = 'Mekn.',
                           Santomera = 'Santo.',
                           Zaragoza = 'Zarag.'),
         dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  mutate(location = factor(location, levels = c('Kl.-Alt.', 'Zarag.', 'Cieza', 'Santo.', 'Mekn.', 'Sfax'))) %>% 
  filter((species %in% c('Apple', 'Pear', 'Sweet Cherry')),
         scenario_year == '2050') %>% 
  ggplot(ggplot2::aes(y = cult_num)) +
  # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
  #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
  #geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
  #           col = 'black',
  #           show.legend = FALSE, shape = 4) + 
  geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', '', '', '', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  coord_cartesian(xlim = c(1, 170)) +
  ggh4x::facetted_pos_scales(y = list(
    species_label == 'Apple' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_apple[2] - 0.5, pos_plot_apple[1] + 0.5), breaks = pos_plot_apple[2]:pos_plot_apple[1], labels = cult_num_df$cultivar[pos_label_apple[2]:pos_label_apple[1]]),
    species_label == 'Pear' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_pear[2], pos_plot_pear[1]), breaks = pos_plot_pear[2]:pos_plot_pear[1], labels = cult_num_df$cultivar[pos_label_pear[2]:pos_label_pear[1]]),
    species_label == 'Sweet Cherry' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_sweet_cherry[2], pos_plot_sweet_cherry[1]), breaks = pos_plot_sweet_cherry[2]:pos_plot_sweet_cherry[1], labels = cult_num_df$cultivar[pos_label_sweet_cherry[2]:pos_label_sweet_cherry[1]]))
  ) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))


p2 <- shift_df %>% 
  merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi'),
         species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry')),
         location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
                           Meknes = 'Mekn.',
                           Santomera = 'Santo.',
                           Zaragoza = 'Zarag.'),
         dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  mutate(location = factor(location, levels = c('Kl.-Alt.', 'Zarag.', 'Cieza', 'Santo.', 'Mekn.', 'Sfax'))) %>% 
  filter(!(species %in% c('Apple', 'Pear', 'Sweet Cherry')),
         scenario_year == '2050') %>% 
  ggplot(ggplot2::aes(y = cult_num)) +
  # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
  #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
  # geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
  #            show.legend = FALSE, shape = 18) + 
  geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', '', '', '', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  coord_cartesian(xlim = c(1, 170)) +
  ggh4x::facetted_pos_scales(y = list(
    species_label == 'ep' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_plum[2] - 0.5, pos_plot_plum[1] + 0.5), breaks = pos_plot_plum[2]:pos_plot_plum[1], labels = cult_num_df$cultivar[pos_label_plum[2]:pos_label_plum[1]]),
    species_label == 'Apricot' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_apricot[2], pos_plot_apricot[1]), breaks = pos_plot_apricot[2]:pos_plot_apricot[1], labels = cult_num_df$cultivar[pos_label_apricot[2]:pos_label_apricot[1]]),
    species_label == 'Almond' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_almond[2], pos_plot_almond[1]), breaks = pos_plot_almond[2]:pos_plot_almond[1], labels = cult_num_df$cultivar[pos_label_almond[2]:pos_label_almond[1]]),
    species_label == 'pi' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_pistachio[2] - 0.5, pos_plot_pistachio[1] + 0.5), breaks = pos_plot_pistachio[2]:pos_plot_pistachio[1], labels = cult_num_df$cultivar[pos_label_pistachio[2]:pos_label_pistachio[1]]))
  ) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))


p1 + p2 + plot_layout(guides = 'collect') & ggplot2::theme(legend.position= 'bottom') 
ggplot2::ggsave('figures/paper/change_med_bloom_2050_cult.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')




p1 <- shift_df %>% 
  merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi'),
         species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry')),
         location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
                           Meknes = 'Mekn.',
                           Santomera = 'Santo.',
                           Zaragoza = 'Zarag.'),
         dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  mutate(location = factor(location, levels = c('Kl.-Alt.', 'Zarag.', 'Cieza', 'Santo.', 'Mekn.', 'Sfax'))) %>% 
  filter((species %in% c('Apple', 'Pear', 'Sweet Cherry')),
         scenario_year == '2085') %>% 
  ggplot(ggplot2::aes(y = cult_num)) +
  # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
  #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
  #geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
  #           col = 'black',
  #           show.legend = FALSE, shape = 4) + 
  geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', '', '', '', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  coord_cartesian(xlim = c(1, 170)) +
  ggh4x::facetted_pos_scales(y = list(
    species_label == 'Apple' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_apple[2] - 0.5, pos_plot_apple[1] + 0.5), breaks = pos_plot_apple[2]:pos_plot_apple[1], labels = cult_num_df$cultivar[pos_label_apple[2]:pos_label_apple[1]]),
    species_label == 'Pear' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_pear[2], pos_plot_pear[1]), breaks = pos_plot_pear[2]:pos_plot_pear[1], labels = cult_num_df$cultivar[pos_label_pear[2]:pos_label_pear[1]]),
    species_label == 'Sweet Cherry' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_sweet_cherry[2], pos_plot_sweet_cherry[1]), breaks = pos_plot_sweet_cherry[2]:pos_plot_sweet_cherry[1], labels = cult_num_df$cultivar[pos_label_sweet_cherry[2]:pos_label_sweet_cherry[1]]))
  ) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))


p2 <- shift_df %>% 
  merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi'),
         species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry')),
         location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
                           Meknes = 'Mekn.',
                           Santomera = 'Santo.',
                           Zaragoza = 'Zarag.'),
         dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         med_future = ifelse(is.na(med_current), yes = NA, no = med_future)) %>% 
  mutate(location = factor(location, levels = c('Kl.-Alt.', 'Zarag.', 'Cieza', 'Santo.', 'Mekn.', 'Sfax'))) %>% 
  filter(!(species %in% c('Apple', 'Pear', 'Sweet Cherry')),
         scenario_year == '2085') %>% 
  ggplot(ggplot2::aes(y = cult_num)) +
  # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
  #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
  geom_rect(aes(xmin = med_current, xmax = med_future, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
  # geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
  #            show.legend = FALSE, shape = 18) + 
  geom_rect(aes(xmin = med_current - 0.5, xmax = med_current + 0.5, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_x_continuous(breaks = c(32,  60, 91, 121, 152), 
                     labels = c('Feb', '', '', '', 'Jun'),
                     minor_breaks = c(1, 32, 60, 91, 121, 152)) +
  coord_cartesian(xlim = c(1, 170)) +
  ggh4x::facetted_pos_scales(y = list(
    species_label == 'ep' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_plum[2] - 0.5, pos_plot_plum[1] + 0.5), breaks = pos_plot_plum[2]:pos_plot_plum[1], labels = cult_num_df$cultivar[pos_label_plum[2]:pos_label_plum[1]]),
    species_label == 'Apricot' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_apricot[2], pos_plot_apricot[1]), breaks = pos_plot_apricot[2]:pos_plot_apricot[1], labels = cult_num_df$cultivar[pos_label_apricot[2]:pos_label_apricot[1]]),
    species_label == 'Almond' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_almond[2], pos_plot_almond[1]), breaks = pos_plot_almond[2]:pos_plot_almond[1], labels = cult_num_df$cultivar[pos_label_almond[2]:pos_label_almond[1]]),
    species_label == 'pi' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_pistachio[2] - 0.5, pos_plot_pistachio[1] + 0.5), breaks = pos_plot_pistachio[2]:pos_plot_pistachio[1], labels = cult_num_df$cultivar[pos_label_pistachio[2]:pos_label_pistachio[1]]))
  ) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Date') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))


p1 + p2 + plot_layout(guides = 'collect') & ggplot2::theme(legend.position= 'bottom') 
ggplot2::ggsave('figures/paper/change_med_bloom_2085_cult.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')



#----------------------------------#
#Change in failure rates
#----------------------------------#

rm(pheno_2015, pheno_current, pheno_future, pheno_and_window, cieza, meknes, cka, sfax, santomera, zaragoza,
   median_2015, median_future, median_df, performance, performance_ensemble, shift_df, table_text,
   weather_list_obs, weather_list_pred)


fail_2015 <- read.csv('data/failure-rate_thermal-risk_2020-sim.csv') %>% 
  filter(cultivar %in% cultivar_n)

med_hist <- fail_2015 %>% 
  group_by(species, location) %>% 
  summarise(hist_fail = median(failure_rate))

fail_future <- read.csv('data/failure-rate_thermal-risk_future.csv')%>% 
  filter(cultivar %in% cultivar_n)

#problem of future failure is, that I accedentally removed cultivars which had a 100% failure rate 
#if they never were able to fulfill thermal requirements
#--> make empty table, merge the faiure table, give remaining nas the value of 100%

spec_val_lookup <- fail_2015 %>% 
  dplyr::select(species, cultivar) %>% 
  group_by(species) %>% 
  distinct()

ssp <- unique(fail_future$ssp)
scenario_year <- unique(fail_future$scenario_year)
gcm <- unique(fail_future$gcm)
cultivar <- unique(fail_future$cultivar)
location <- unique(fail_future$location)

fail_future <- expand.grid(ssp, scenario_year, gcm, cultivar, location) %>% 
  set_colnames(c('ssp', 'scenario_year', 'gcm', 'cultivar', 'location')) %>% 
  merge(spec_val_lookup, by = 'cultivar') %>% 
  merge(fail_future, by = c('species', 'cultivar', 'location', 'scenario_year', 'ssp', 'gcm'), all.x = TRUE) %>% 
  mutate(failure_rate = replace_na(failure_rate, 100))

#the matches still miss the species name
#how can I look the values up?
#--> make lookup table of species and cultivar


ncult <- fail_future %>% 
  group_by(species) %>% 
  summarise(cult =unique(cultivar)) %>% 
  ungroup() %>% 
  nrow()



med_fut <- fail_future %>% 
  group_by(species, location, scenario_year, gcm, ssp) %>% 
  summarise(fut_fail = median(failure_rate))




shift_fail_df <- med_hist %>% 
  merge(med_fut, by = c('species',  'location')) %>% 
  mutate(species = stringr::str_to_title(species))


fail_shift_df <- shift_fail_df %>% 
  mutate(shift_fail = fut_fail - hist_fail) %>% 
  group_by(species, location, scenario_year, ssp) %>% 
  summarise(hist_fail = median(hist_fail),
            min_fut_fail = min(fut_fail),
            max_mut_fail = max(fut_fail),
            min_shift = min(shift_fail, na.rm = TRUE),
            max_shift = max(shift_fail, na.rm = TRUE))

write.csv(fail_shift_df, 'data/summary_shift_failure_text.csv', row.names = FALSE)

#I need this data also on a cultivar level!
#re-run the necissary script


shift_fail_df %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')))) %>% 
  filter(scenario_year == '2050') %>% 
  ggplot(ggplot2::aes(y = spec)) +
  geom_rect(aes(xmin = hist_fail, xmax = fut_fail, ymin = spec + dodge_low, ymax = spec + dodge_up, fill = ssp)) +
  geom_point(aes(x = fut_fail, y = spec + ((dodge_low + dodge_up)/2)),
             col = 'black',
             show.legend = FALSE, shape = 4, size = 1) + 
  geom_rect(aes(xmin = hist_fail - 0.25, xmax = hist_fail + 0.25, ymax = spec - 0.4, ymin = spec + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(location~., scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_y_reverse(breaks = rep(1:7,6), labels = rep(c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'), 6)) +
  theme(legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Failure Rate (%)') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_failure_bloom_2050_v2.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')

shift_fail_df %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')))) %>% 
  filter(scenario_year == '2085') %>% 
  ggplot(ggplot2::aes(y = spec)) +
  geom_rect(aes(xmin = hist_fail, xmax = fut_fail, ymin = spec + dodge_low, ymax = spec + dodge_up, fill = ssp)) +
  geom_point(aes(x = fut_fail, y = spec + ((dodge_low + dodge_up)/2)),
             col = 'black',
             show.legend = FALSE, shape = 4, size = 1) + 
  geom_rect(aes(xmin = hist_fail - 0.25, xmax = hist_fail + 0.25, ymax = spec - 0.4, ymin = spec + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(location~., scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_y_reverse(breaks = rep(1:7,6), labels = rep(c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'), 6)) +
  theme(legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Failure Rate (%)') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_failure_bloom_2085_v2.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')



shift_fail_df %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
         spec = as.numeric(species),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')))) %>% 
  filter(scenario_year == '2050') %>% 
  ggplot(ggplot2::aes(y = loc)) +
  geom_rect(aes(xmin = hist_fail, xmax = fut_fail, ymin = loc + dodge_low, ymax = loc + dodge_up, fill = ssp)) +
  geom_point(aes(x = fut_fail, y = loc + ((dodge_low + dodge_up)/2)),
             col = 'black',
             show.legend = FALSE, shape = 4, size = 1) + 
  geom_rect(aes(xmin = hist_fail - 0.25, xmax = hist_fail + 0.25, ymax = loc - 0.4, ymin = loc + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species~., scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_y_reverse(breaks = rep(1:6,7), labels = rep( c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'), 7)) +
  theme(legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Failure Rate (%)') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_failure_bloom_2050_v3.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')

shift_fail_df %>% 
  mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
         species = factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio')),
         spec = as.numeric(species),
         loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')))) %>% 
  filter(scenario_year == '2085') %>% 
  ggplot(ggplot2::aes(y = loc)) +
  geom_rect(aes(xmin = hist_fail, xmax = fut_fail, ymin = loc + dodge_low, ymax = loc + dodge_up, fill = ssp)) +
  geom_point(aes(x = fut_fail, y = loc + ((dodge_low + dodge_up)/2)),
             col = 'black',
             show.legend = FALSE, shape = 4, size = 1) + 
  geom_rect(aes(xmin = hist_fail - 0.25, xmax = hist_fail + 0.25, ymax = loc - 0.4, ymin = loc + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species~., scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  scale_y_reverse(breaks = rep(1:6,7), labels = rep( c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'), 7)) +
  theme(legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Failure Rate (%)') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
ggplot2::ggsave('figures/paper/change_failure_bloom_2085_v3.jpeg', device = 'jpeg',
                height = 40, width = 31, units = 'cm')


# shift_df %>% 
#   mutate(dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2),
#          location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')),
#          spec = as.numeric(factor(species, levels = c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'))),
#          loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
#          ssp_year = paste(ssp, scenario_year, sep = '-')) %>% 
#   ggplot(ggplot2::aes(y = spec)) +
#   geom_rect(aes(xmin = hist_fail, xmax = fut_fail, ymin = spec + dodge_low, ymax = spec + dodge_up, fill = ssp_year), col = 'black') +
#   geom_point(aes(x = fut_fail, y = spec + ((dodge_low + dodge_up)/2)),
#              col = 'black',
#              show.legend = FALSE, shape = 4, size = 1) + 
#   geom_rect(aes(xmin = hist_fail - 0.25, xmax = hist_fail + 0.25, ymax = spec - 0.4, ymin = spec + 0.4, fill = 'Simulation 2015'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(location~., scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c('lightblue', "#56B4E9", 'lightgreen', "#009E73", 'lightyellow', "#F0E442", 'tan1',  "#E69F00"))+
#   scale_fill_manual(values = c('black', 'lightblue', "#56B4E9", 'lightgreen', "#009E73",'lightyellow',  "#F0E442",'tan1',   "#E69F00", 'grey70'))+
#   theme_bw(base_size = 15) +
#   scale_y_reverse(breaks = rep(1:7,6), labels = rep(c('Apple', 'Pear', 'Apricot', 'European Plum', 'Sweet Cherry', 'Almond', 'Pistachio'), 6)) +
#   theme(legend.position = 'bottom') +
#   ylab('') +
#   xlab('Median Predicted Bloom Failure Rate (%)') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# ggplot2::ggsave('figures/paper/test.jpeg', device = 'jpeg',
#                 height = 27, width = 31, units = 'cm')



#-----------------------------#
#change failure on cultivar level
#------------------------------#

med_hist <- fail_2015 %>% 
  group_by(species, location) %>% 
  summarise(hist_fail = median(failure_rate))

med_fut <- fail_future %>% 
  group_by(species, location, scenario_year, gcm, ssp) %>% 
  summarise(fut_fail = median(failure_rate))

shift_fail_df <- fail_2015 %>% 
  merge(fail_future, by = c('species', 'cultivar',  'location'), all.x = TRUE) %>% 
  mutate(species = stringr::str_to_title(species)) %>% 
  rename(hist_fail = failure_rate.x,
         fut_fail = failure_rate.y)


#get number of cultivars
ncult <- shift_fail_df %>% 
  group_by(species) %>% 
  summarise(cult =unique(cultivar)) %>% 
  ungroup() %>% 
  nrow()

#assign number to cultivars, keep order as in the plot
cult_num_df <- shift_fail_df %>% 
  group_by(species) %>% 
  summarise(cultivar =unique(cultivar)) %>% 
  ungroup() %>% 
  mutate(cult_num  =ncult:1)


#position label
pos_label_almond <- c(min(which(cult_num_df$species == 'Almond')), max(which(cult_num_df$species == 'Almond')))
pos_plot_almond <- c(cult_num_df$cult_num[pos_label_almond[1]], cult_num_df$cult_num[pos_label_almond[2]])

pos_label_apple <- c(min(which(cult_num_df$species == 'Apple')), max(which(cult_num_df$species == 'Apple')))
pos_plot_apple <- c(cult_num_df$cult_num[pos_label_apple[1]], cult_num_df$cult_num[pos_label_apple[2]])

pos_label_apricot <- c(min(which(cult_num_df$species == 'Apricot')), max(which(cult_num_df$species == 'Apricot')))
pos_plot_apricot <- c(cult_num_df$cult_num[pos_label_apricot[1]], cult_num_df$cult_num[pos_label_apricot[2]])

pos_label_plum <- c(min(which(cult_num_df$species == 'European Plum')), max(which(cult_num_df$species == 'European Plum')))
pos_plot_plum <- c(cult_num_df$cult_num[pos_label_plum[1]], cult_num_df$cult_num[pos_label_plum[2]])

pos_label_sweet_cherry <- c(min(which(cult_num_df$species == 'Sweet Cherry')), max(which(cult_num_df$species == 'Sweet Cherry')))
pos_plot_sweet_cherry <- c(cult_num_df$cult_num[pos_label_sweet_cherry[1]], cult_num_df$cult_num[pos_label_sweet_cherry[2]])

pos_label_pear <- c(min(which(cult_num_df$species == 'Pear')), max(which(cult_num_df$species == 'Pear')))
pos_plot_pear <- c(cult_num_df$cult_num[pos_label_pear[1]], cult_num_df$cult_num[pos_label_pear[2]])

pos_label_pistachio <- c(min(which(cult_num_df$species == 'Pistachio')), max(which(cult_num_df$species == 'Pistachio')))
pos_plot_pistachio <- c(cult_num_df$cult_num[pos_label_pistachio[1]], cult_num_df$cult_num[pos_label_pistachio[2]])


p1 <- shift_fail_df %>% 
  merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi'),
         species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry')),
         location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
                           Meknes = 'Mekn.',
                           Santomera = 'Santo.',
                           Zaragoza = 'Zarag.'),
         dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
  mutate(location = factor(location, levels = c('Kl.-Alt.', 'Zarag.', 'Cieza', 'Santo.', 'Mekn.', 'Sfax'))) %>% 
  filter((species %in% c('Apple', 'Pear', 'Sweet Cherry')),
         scenario_year == '2050') %>% 
  ggplot(ggplot2::aes(y = cult_num)) +
  # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
  #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
  geom_rect(aes(xmin = hist_fail, xmax = fut_fail, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
  #geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
  #           col = 'black',
  #           show.legend = FALSE, shape = 4) + 
  geom_rect(aes(xmin = hist_fail - 1, xmax = hist_fail + 1, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  ggh4x::facetted_pos_scales(y = list(
    species_label == 'Apple' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_apple[2] - 0.5, pos_plot_apple[1] + 0.5), breaks = pos_plot_apple[2]:pos_plot_apple[1], labels = cult_num_df$cultivar[pos_label_apple[2]:pos_label_apple[1]]),
    species_label == 'Pear' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_pear[2], pos_plot_pear[1]), breaks = pos_plot_pear[2]:pos_plot_pear[1], labels = cult_num_df$cultivar[pos_label_pear[2]:pos_label_pear[1]]),
    species_label == 'Sweet Cherry' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_sweet_cherry[2], pos_plot_sweet_cherry[1]), breaks = pos_plot_sweet_cherry[2]:pos_plot_sweet_cherry[1], labels = cult_num_df$cultivar[pos_label_sweet_cherry[2]:pos_label_sweet_cherry[1]]))
  ) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Failure Rate (%)') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))



p2 <- shift_fail_df %>% 
  merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi'),
         species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry')),
         location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
                           Meknes = 'Mekn.',
                           Santomera = 'Santo.',
                           Zaragoza = 'Zarag.'),
         dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
  mutate(location = factor(location, levels = c('Kl.-Alt.', 'Zarag.', 'Cieza', 'Santo.', 'Mekn.', 'Sfax'))) %>% 
  filter(!(species %in% c('Apple', 'Pear', 'Sweet Cherry')),
         scenario_year == '2050') %>% 
  ggplot(ggplot2::aes(y = cult_num)) +
  # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
  #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
  geom_rect(aes(xmin = hist_fail, xmax = fut_fail, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
  #geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
  #           col = 'black',
  #           show.legend = FALSE, shape = 4) + 
  geom_rect(aes(xmin = hist_fail - 1, xmax = hist_fail + 1, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  ggh4x::facetted_pos_scales(y = list(
    species_label == 'ep' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_plum[2] - 0.5, pos_plot_plum[1] + 0.5), breaks = pos_plot_plum[2]:pos_plot_plum[1], labels = cult_num_df$cultivar[pos_label_plum[2]:pos_label_plum[1]]),
    species_label == 'Apricot' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_apricot[2], pos_plot_apricot[1]), breaks = pos_plot_apricot[2]:pos_plot_apricot[1], labels = cult_num_df$cultivar[pos_label_apricot[2]:pos_label_apricot[1]]),
    species_label == 'Almond' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_almond[2], pos_plot_almond[1]), breaks = pos_plot_almond[2]:pos_plot_almond[1], labels = cult_num_df$cultivar[pos_label_almond[2]:pos_label_almond[1]]),
    species_label == 'pi' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_pistachio[2] - 0.5, pos_plot_pistachio[1] + 0.5), breaks = pos_plot_pistachio[2]:pos_plot_pistachio[1], labels = cult_num_df$cultivar[pos_label_pistachio[2]:pos_label_pistachio[1]]))
  ) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Failure Rate (%)') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))

p1 + p2 + plot_layout(guides = 'collect') & ggplot2::theme(legend.position= 'bottom') 
ggplot2::ggsave('figures/paper/change_fail_2050_cult.jpeg', device = 'jpeg',
                height = 40, width = 35, units = 'cm')




p1 <- shift_fail_df %>% 
  merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'EP',
                                Pistachio = 'Pi'),
         species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'EP', 'Pear', 'Pi', 'Sweet Cherry')),
         location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
                           Meknes = 'Mekn.',
                           Santomera = 'Santo.',
                           Zaragoza = 'Zarag.'),
         dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
  mutate(location = factor(location, levels = c('Kl.-Alt.', 'Zarag.', 'Cieza', 'Santo.', 'Mekn.', 'Sfax'))) %>% 
  filter((species %in% c('Apple', 'Pear', 'Sweet Cherry')),
         scenario_year == '2085') %>% 
  ggplot(ggplot2::aes(y = cult_num)) +
  # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
  #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
  geom_rect(aes(xmin = hist_fail, xmax = fut_fail, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
  #geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
  #           col = 'black',
  #           show.legend = FALSE, shape = 4) + 
  geom_rect(aes(xmin = hist_fail - 1, xmax = hist_fail + 1, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  ggh4x::facetted_pos_scales(y = list(
    species_label == 'Apple' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_apple[2] - 0.5, pos_plot_apple[1] + 0.5), breaks = pos_plot_apple[2]:pos_plot_apple[1], labels = cult_num_df$cultivar[pos_label_apple[2]:pos_label_apple[1]]),
    species_label == 'Pear' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_pear[2], pos_plot_pear[1]), breaks = pos_plot_pear[2]:pos_plot_pear[1], labels = cult_num_df$cultivar[pos_label_pear[2]:pos_label_pear[1]]),
    species_label == 'Sweet Cherry' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_sweet_cherry[2], pos_plot_sweet_cherry[1]), breaks = pos_plot_sweet_cherry[2]:pos_plot_sweet_cherry[1], labels = cult_num_df$cultivar[pos_label_sweet_cherry[2]:pos_label_sweet_cherry[1]]))
  ) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Failure Rate (%)') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))



p2 <- shift_fail_df %>% 
  merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
  mutate(species_label = recode(species, `European Plum` = 'ep',
                                Pistachio = 'pi'),
         species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'ep', 'Pear', 'pi', 'Sweet Cherry')),
         location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
                           Meknes = 'Mekn.',
                           Santomera = 'Santo.',
                           Zaragoza = 'Zarag.'),
         dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
         dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
  mutate(location = factor(location, levels = c('Kl.-Alt.', 'Zarag.', 'Cieza', 'Santo.', 'Mekn.', 'Sfax'))) %>% 
  filter(!(species %in% c('Apple', 'Pear', 'Sweet Cherry')),
         scenario_year == '2085') %>% 
  ggplot(ggplot2::aes(y = cult_num)) +
  # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
  #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
  geom_rect(aes(xmin = hist_fail, xmax = fut_fail, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
  #geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
  #           col = 'black',
  #           show.legend = FALSE, shape = 4) + 
  geom_rect(aes(xmin = hist_fail - 1, xmax = hist_fail + 1, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2015'),  size = 2) +
  # geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
  scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
  scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
  theme_bw(base_size = 15) +
  ggh4x::facetted_pos_scales(y = list(
    species_label == 'ep' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_plum[2] - 0.5, pos_plot_plum[1] + 0.5), breaks = pos_plot_plum[2]:pos_plot_plum[1], labels = cult_num_df$cultivar[pos_label_plum[2]:pos_label_plum[1]]),
    species_label == 'Apricot' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_apricot[2], pos_plot_apricot[1]), breaks = pos_plot_apricot[2]:pos_plot_apricot[1], labels = cult_num_df$cultivar[pos_label_apricot[2]:pos_label_apricot[1]]),
    species_label == 'Almond' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_almond[2], pos_plot_almond[1]), breaks = pos_plot_almond[2]:pos_plot_almond[1], labels = cult_num_df$cultivar[pos_label_almond[2]:pos_label_almond[1]]),
    species_label == 'pi' ~ ggplot2::scale_y_continuous(limits = c(pos_plot_pistachio[2] - 0.5, pos_plot_pistachio[1] + 0.5), breaks = pos_plot_pistachio[2]:pos_plot_pistachio[1], labels = cult_num_df$cultivar[pos_label_pistachio[2]:pos_label_pistachio[1]]))
  ) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  ylab('') +
  xlab('Median Predicted Bloom Failure Rate (%)') +
  guides(fill=ggplot2::guide_legend(title="Weather Scenario"))

p1 + p2 + plot_layout(guides = 'collect') & ggplot2::theme(legend.position= 'bottom') 
ggplot2::ggsave('figures/paper/change_fail_2085_cult.jpeg', device = 'jpeg',
                height = 40, width = 35, units = 'cm')






# #------------------------------------#
# #summarize the the figure in a boxplot
# #------------------------------------#
# 
# hist_gen_weather <- chillR::load_temperature_scenarios('data/hist-sim-weather/', prefix = 'hist_gen_2015')
# 
# for(i in 1:length(hist_gen_weather)){
#   colnames(hist_gen_weather[[i]]) <- c('DATE', 'Year', 'Month', 'Day', 'nodata', 'Tmin', 'Tmax')
# }
# 
# thermal_window_2015 <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
#                                                                                                                 weather_list_pred = weather_list_obs, 
#                                                                                                                 observation_df = observation_df, 
#                                                                                                                 frost_threshold = frost_threshold, 
#                                                                                                                 heat_threshold = heat_threshold, 
#                                                                                                                 target_col_obs = x,
#                                                                                                                 padding = 0.03)) %>% 
#   bind_rows() %>% 
#   mutate(species = tolower(species))
# 
# rm(hist_gen_weather)
# 
# 
# shift_df %>% 
#   mutate(flowering_type = ifelse(species %in% c('European Plum', 'Japanese Plum', 'Apple'), yes = 'begin_flowering_f5', no = 'flowering_f50'),
#          species = tolower(species)) %>% 
#   merge(thermal_window_2015, by = c('species', 'location', 'flowering_type')) %>% 
#   mutate(postion_hist_window = ifelse(med_current < min_doy_padded, yes = 'earlier', no = ifelse(med_current > max_doy_padded, yes = 'later', no = 'within')),
#          col_fil = paste(scenario_year, postion_hist_window, sep = '_'),
#          col_fil = factor(col_fil, levels = c('2050_earlier', '2085_earlier', '2050_within', '2085_within', '2050_later', '2085_later')),
#          species = stringr::str_to_title(species)) %>% 
#   ggplot(aes(x = species, y = shift_bloom, fill = col_fil)) +
#   geom_boxplot() +
#   scale_fill_manual(breaks = c('2050_earlier', '2085_earlier', '2050_within', '2085_within', '2050_later', '2085_later'),
#                     values = c('lightblue', 'steelblue', 'lightgreen', 'limegreen', 'lightpink', 'lightcoral'),
#                     labels = c('2050 Earlier', '2085 Earlier', '2050 Within', '2085 Within', '2050 Later', '2085 Later')) +
#   facet_grid(location ~ ssp) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom')
# 
# 
# shift_df %>% 
#   mutate(flowering_type = ifelse(species %in% c('European Plum', 'Japanese Plum', 'Apple'), yes = 'begin_flowering_f5', no = 'flowering_f50'),
#          species = tolower(species)) %>% 
#   merge(thermal_window_2015, by = c('species', 'location', 'flowering_type')) %>% 
#   mutate(postion_hist_window = ifelse(med_current < min_doy_padded, yes = 'earlier', no = ifelse(med_current > max_doy_padded, yes = 'later', no = 'within')),
#          col_fil = paste(ssp, scenario_year, sep = '_'),
#          col_fil = factor(col_fil, levels = c('ssp126_2050', 'ssp126_2085', 'ssp245_2050', 'ssp245_2085', 'ssp370_2050', 'ssp370_2085', 'ssp585_2050', 'ssp585_2085')),
#          species = stringr::str_to_title(species)) %>% 
#   mutate(species = recode(species, 
#                           `European Plum` = 'Europ Plum',
#                           `Japanese Plum` = 'Jap. Plum',
#                           `Sweet Cherry` = 'Sw. Cherry')) %>% 
#   ggplot(aes(x = species, y = shift_bloom, fill = col_fil)) +
#   geom_boxplot() +
#   ylab('Shift in Median Bloom Dates (Days)\ncompared to 2015') +
#   xlab('')+
#   scale_fill_manual(values = c('lightblue', 'steelblue', 'lightgreen', 'limegreen', 'darkgoldenrod1', 'darkorange', 'thistle', 'violetred'),
#                     breaks = c('ssp126_2050', 'ssp126_2085', 'ssp245_2050', 'ssp245_2085', 'ssp370_2050', 'ssp370_2085', 'ssp585_2050', 'ssp585_2085'),
#                     labels = c('SSP126 2050', 'SSP126 2085', 'SSP245 2050', 'SSP245 2085', 'SSP370 2050', 'SSP370 2085', 'SSP585 2050', 'SSP585 2085'),
#                     name = 'Climate Change\nScenario and Year') +
#   facet_wrap( ~ location, ncol = 2) +
#   theme_bw(base_size = 15) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'right')
# ggsave('figures/paper/shift_median_flowering_summarized.jpeg', height = 20, width = 30, units = 'cm', device = 'jpeg')
# 
# 
# 
# #------------------------#
# #change in failure
# #------------------------#
# fail_2015 <- read.csv('data/failure-rate_thermal-risk_2020-sim.csv')
# fail_future <- read.csv('data/failure-rate_thermal-risk_future.csv')
# 
# unique(fail_2015$species)
# unique(fail_future$species)
# unique(pheno_2015$species)
# unique(pheno_future$species)
# unique(shift_df$species)
# unique(thermal_window_2015$species)
# 
# 
# fail_shift <- fail_2015 %>% 
#   rename(failure_hist = failure_rate) %>% 
#   merge(fail_future, by = c('species', 'cultivar', 'location')) %>% 
#   mutate(change_fail = failure_rate - failure_hist)
#   
# 
# 
# #2050
#   
# p1 <- fail_shift %>% 
#   mutate(species = stringr::str_to_title(species)) %>% 
#   merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
#   mutate(species_label = recode(species, `European Plum` = 'ep',
#                                 `Japanese Plum` = 'jp',
#                                 Pistachio = 'pi'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'ep', 'jp', 'Pear', 'pi', 'Sweet Cherry')),
#          location = recode(location, 
#                            `Klein-Altendorf` = 'Kl.-Alt.',
#                            Meknes = 'Mekn.',
#                            Santomera = 'Santo.',
#                            Zaragoza = 'Zarag.'),
#          dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
#   filter((species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2050') %>% 
#   ggplot(ggplot2::aes(y = cult_num)) +
#   # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
#   #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
#   geom_rect(aes(xmin = failure_hist, xmax = failure_rate, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
#   geom_point(aes(x = failure_rate, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
#              show.legend = FALSE, shape = 18) + 
#   geom_rect(aes(xmin = failure_hist - 1, xmax = failure_hist + 1, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(0, 50, 100), 
#                      labels = c('0%', '50%', '100%'),
#                      minor_breaks = c(0, 25, 50, 75, 100)) +
#   ggh4x::facetted_pos_scales(y = list(
#     species_label == 'Almond' ~ scale_y_continuous(limits = c(72,110), breaks = 72:110, labels = cult_num_df$cultivar[39:1]),
#     species_label == 'Apple' ~ scale_y_continuous(limits = c(67-0.5, 71+0.5), breaks = 67:71, labels = cult_num_df$cultivar[44:40]),
#     species_label == 'Apricot' ~ scale_y_continuous(limits = c(54-0.5, 66+0.5), breaks = 54:66, labels = cult_num_df$cultivar[57:45]))
#   ) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Share of Predicted Flowering outside of Bloom Window') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# 
# p2 <- fail_shift %>% 
#   mutate(species = stringr::str_to_title(species)) %>% 
#   merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
#   mutate(species_label = recode(species, `European Plum` = 'ep',
#                                 `Japanese Plum` = 'jp',
#                                 Pistachio = 'pi'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'ep', 'jp', 'Pear', 'pi', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Meknes = 'Mekn.',
#                            Santomera = 'Santo.',
#                            Zaragoza = 'Zarag.'),
#          dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2050') %>% 
#   ggplot(ggplot2::aes(y = cult_num)) +
#   # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
#   #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
#   geom_rect(aes(xmin = failure_hist, xmax = failure_rate, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
#   geom_point(aes(x = failure_rate, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
#              show.legend = FALSE, shape = 18) + 
#   geom_rect(aes(xmin = failure_hist - 1, xmax = failure_hist + 1, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(0, 50, 100), 
#                      labels = c('0%', '50%', '100%'),
#                      minor_breaks = c(0, 25, 50, 75, 100)) +
#   ggh4x::facetted_pos_scales(y = list(
#     species_label == 'ep' ~ ggplot2::scale_y_continuous(limits = c(53-0.5, 53+ 0.5), breaks = 53, labels = cult_num_df$cultivar[58]),
#     species_label == 'jp' ~ ggplot2::scale_y_continuous(limits = (c(52-0.5, 52+0.5)), breaks = 52, labels = cult_num_df$cultivar[59]),
#     species_label == 'Pear' ~ ggplot2::scale_y_continuous(limits = (c(37, 51)), breaks = 37:51, labels = cult_num_df$cultivar[74:60]),
#     species_label == 'pi' ~ ggplot2::scale_y_continuous(limits = (c(35-0.5, 36+0.5)), breaks = 35:36, labels = cult_num_df$cultivar[76:75]),
#     species_label == 'Sweet Cherry' ~ ggplot2::scale_y_continuous(limits = (c(1, 34)), breaks = 1:34, labels = cult_num_df$cultivar[110:77]))
#   ) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Share of Predicted Flowering outside of Bloom Window') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# 
# p1 + p2 + plot_layout(guides = 'collect') & ggplot2::theme(legend.position= 'bottom') 
# ggplot2::ggsave('figures/paper/change_failure_rate_2050.jpeg', device = 'jpeg',
#                 height = 27, width = 31, units = 'cm')
# 
# ###2085
# 
# p1 <- fail_shift %>% 
#   mutate(species = stringr::str_to_title(species)) %>% 
#   merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
#   mutate(species_label = recode(species, `European Plum` = 'ep',
#                                 `Japanese Plum` = 'jp',
#                                 Pistachio = 'pi'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'ep', 'jp', 'Pear', 'pi', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Meknes = 'Mekn.',
#                            Santomera = 'Santo.',
#                            Zaragoza = 'Zarag.'),
#          dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
#   filter((species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2085') %>% 
#   ggplot(ggplot2::aes(y = cult_num)) +
#   # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
#   #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
#   geom_rect(aes(xmin = failure_hist, xmax = failure_rate, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
#   geom_point(aes(x = failure_rate, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
#              show.legend = FALSE, shape = 18) + 
#   geom_rect(aes(xmin = failure_hist - 1, xmax = failure_hist + 1, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(0, 50, 100), 
#                      labels = c('0%', '50%', '100%'),
#                      minor_breaks = c(0, 25, 50, 75, 100)) +
#   ggh4x::facetted_pos_scales(y = list(
#     species_label == 'Almond' ~ scale_y_continuous(limits = c(72,110), breaks = 72:110, labels = cult_num_df$cultivar[39:1]),
#     species_label == 'Apple' ~ scale_y_continuous(limits = c(67-0.5, 71+0.5), breaks = 67:71, labels = cult_num_df$cultivar[44:40]),
#     species_label == 'Apricot' ~ scale_y_continuous(limits = c(54-0.5, 66+0.5), breaks = 54:66, labels = cult_num_df$cultivar[57:45]))
#   ) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Share of Predicted Flowering outside of Bloom Window') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# 
# p2 <- fail_shift %>% 
#   mutate(species = stringr::str_to_title(species)) %>% 
#   merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
#   mutate(species_label = recode(species, `European Plum` = 'ep',
#                                 `Japanese Plum` = 'jp',
#                                 Pistachio = 'pi'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'ep', 'jp', 'Pear', 'pi', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Meknes = 'Mekn.',
#                            Santomera = 'Santo.',
#                            Zaragoza = 'Zarag.'),
#          dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2085') %>% 
#   ggplot(ggplot2::aes(y = cult_num)) +
#   # geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
#   #               fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
#   geom_rect(aes(xmin = failure_hist, xmax = failure_rate, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
#   geom_point(aes(x = failure_rate, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
#              show.legend = FALSE, shape = 18) + 
#   geom_rect(aes(xmin = failure_hist - 1, xmax = failure_hist + 1, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(0, 50, 100), 
#                      labels = c('0%', '50%', '100%'),
#                      minor_breaks = c(0, 25, 50, 75, 100)) +
#   ggh4x::facetted_pos_scales(y = list(
#     species_label == 'ep' ~ ggplot2::scale_y_continuous(limits = c(53-0.5, 53+ 0.5), breaks = 53, labels = cult_num_df$cultivar[58]),
#     species_label == 'jp' ~ ggplot2::scale_y_continuous(limits = (c(52-0.5, 52+0.5)), breaks = 52, labels = cult_num_df$cultivar[59]),
#     species_label == 'Pear' ~ ggplot2::scale_y_continuous(limits = (c(37, 51)), breaks = 37:51, labels = cult_num_df$cultivar[74:60]),
#     species_label == 'pi' ~ ggplot2::scale_y_continuous(limits = (c(35-0.5, 36+0.5)), breaks = 35:36, labels = cult_num_df$cultivar[76:75]),
#     species_label == 'Sweet Cherry' ~ ggplot2::scale_y_continuous(limits = (c(1, 34)), breaks = 1:34, labels = cult_num_df$cultivar[110:77]))
#   ) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Share of Predicted Flowering outside of Bloom Window') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# 
# p1 + p2 + plot_layout(guides = 'collect') & ggplot2::theme(legend.position= 'bottom') 
# ggplot2::ggsave('figures/paper/change_failure_rate_2085.jpeg', device = 'jpeg',
#                 height = 27, width = 31, units = 'cm')
# 
# 
# 
# 
# 
# #summary plot change failure rate 
# 
# fail_shift %>% 
#   mutate(col_fil = paste(ssp, scenario_year, sep = '_'),
#          col_fil = factor(col_fil, levels = c('ssp126_2050', 'ssp126_2085', 'ssp245_2050', 'ssp245_2085', 'ssp370_2050', 'ssp370_2085', 'ssp585_2050', 'ssp585_2085')),
#          species = stringr::str_to_title(species)) %>% 
#   mutate(species = recode(species, 
#                           `European Plum` = 'Europ Plum',
#                           `Japanese Plum` = 'Jap. Plum',
#                           `Sweet Cherry` = 'Sw. Cherry')) %>% 
#   ggplot(aes(x = species, y = change_fail, fill = col_fil)) +
#   geom_boxplot() +
#   ylab('Change in Predicted Flowering Outside\nBloom Time Window (%) compared to 2015') +
#   xlab('')+
#   scale_fill_manual(values = c('lightblue', 'steelblue', 'lightgreen', 'limegreen', 'darkgoldenrod1', 'darkorange', 'thistle', 'violetred'),
#                     breaks = c('ssp126_2050', 'ssp126_2085', 'ssp245_2050', 'ssp245_2085', 'ssp370_2050', 'ssp370_2085', 'ssp585_2050', 'ssp585_2085'),
#                     labels = c('SSP126 2050', 'SSP126 2085', 'SSP245 2050', 'SSP245 2085', 'SSP370 2050', 'SSP370 2085', 'SSP585 2050', 'SSP585 2085'), 
#                     name = 'Climate Change\nScenario and Year') +
#   facet_wrap( ~ location, ncol = 2) +
#   theme_bw(base_size = 15) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'right')
# ggsave('figures/paper/shift_median_failure_summarized.jpeg', height = 20, width = 30, units = 'cm', device = 'jpeg')
# 
# 
# 
# 
# 
# fail_current <- read.csv('data/failure-rate_thermal-risk_current.csv')
# library(tidyverse)
# fail_current %>% 
#   filter(cultivar %in% cultivar_n) %>% 
#   filter(location == 'Zaragoza') %>% 
#   group_by(species) %>% 
#   summarise(med = median(failure_rate))
# 
# fail_2105 <- read.csv('data/failure-rate_thermal-risk_2020-sim.csv')
# 
# fail_2105 %>% 
#   filter(location == 'Sfax') %>% 
#   group_by(species) %>% 
#   summarize(med = median(failure_rate))
# 
# fail_future <- read.csv('data/failure-rate_thermal-risk_future.csv')
# 
# fail_future %>% 
#   filter(location == 'Sfax') %>% 
#   group_by(species, ssp, scenario_year) %>% 
#   summarise(med = median(failure_rate))
# 
# 
# ###make the plot with change in predicted bloom date
# 
# 
# 
# ###make plot with change in predicted failure rate
# 
# 
# data.frame(a = 1, b = 1) %>% 
#   rename(dgdg = a)
# 
# 
# 
# 
# rm(list = ls())
# 
# adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv') %>% 
#   filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),
#          !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014))
# 
# sub <- adamedor %>% 
#   filter(species != 'Olive') %>% 
#   group_by(species, cultivar) %>% 
#   summarise(n = n()) %>% 
#   filter(n >= 20)
# 
# sub %>% 
#   group_by(species) %>% 
#   summarise(n = length(unique(cultivar)))
# 
# 
# # sub_per_loc <- adamedor %>% 
# #   filter(cultivar %in% unique(sub$cultivar)) %>% 
# #   group_by(cultivar, species, location) %>% 
# #   summarise(min_year = min(year),
# #             max_year = max(year),
# #             n = n())
# # write.csv(sub_per_loc, 'data/summary_table_cultivars.csv', row.names = FALSE)
# 
# flower_summarized <- adamedor %>% 
#   group_by(species, location) %>% 
#   mutate(begin_flowering_f5 = lubridate::yday(begin_flowering_f5), 
#          flowering_f50 = lubridate::yday(flowering_f50)) %>% 
#   summarize(mean.begin_flowering_f5 = mean(begin_flowering_f5, na.rm = TRUE),
#             sd.begin_flowering_f5 = sd(begin_flowering_f5, na.rm = TRUE),
#             max_dist.begin_flowering_f5 = max(abs(begin_flowering_f5 - mean(begin_flowering_f5, na.rm = TRUE)), na.rm = TRUE) * 1.5,
#             mean.flowering_f50 = mean(flowering_f50, na.rm = TRUE),
#             sd.flowering_f50 = sd(flowering_f50, na.rm = TRUE),
#             max_dist.flowering_f50 = max(abs(flowering_f50 - mean(flowering_f50, na.rm = TRUE)), na.rm = TRUE) * 1.5) %>% 
#   reshape2::melt(id.vars = c('species', 'location')) %>% 
#   mutate(value = replace(value, value %in% c(NaN, NA, -Inf), NA)) %>% 
#   separate(col = variable, into = c('variable', 'flowering_type'), sep = '\\.') %>% 
#   reshape2::dcast(species + location + flowering_type ~ variable, value.var = 'value') %>% 
#   relocate(species, location, flowering_type, mean, sd, max_dist) %>% 
#   mutate(location = as.factor(location),
#          species = recode(species, 
#                           `European plum` = "European Plum", 
#                           `Japanese plum` = "Japanese Plum") ) %>% 
#   mutate(upper = mean + max_dist,
#          lower = mean - max_dist) %>% 
#   dplyr::filter(location %in% c('Sfax', 'Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf', 'Santomera'),
#                 !(species %in% c('Peach', 'Olive'))) %>% 
#   mutate(location = factor(location, levels = c('Klein-Altendorf','Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))) %>% 
#   na.omit()
# 
# source('code/utilities/time_window_translation.R')
# 
# f_mean <- est_phen_gaps(target_df = flower_summarized, target_col = 'mean', split_col = 'flowering_type')
# f_sd <- est_phen_gaps(target_df = flower_summarized, target_col = 'sd', split_col = 'flowering_type')
# f_lower <- est_phen_gaps(target_df = flower_summarized, target_col = 'lower', split_col = 'flowering_type')
# f_upper <- est_phen_gaps(target_df = flower_summarized, target_col = 'upper', split_col = 'flowering_type')
# 
# 
# #maybe take median and then look how the time windows would be
# 
# f_lower_sum <- f_lower %>% 
#   group_by(species, location, flowering_type, source) %>% 
#   summarise(lower = median(value),
#             lower_plus = quantile(value, 0.25, na.rm = TRUE))
# 
# f_upper_sum <- f_upper %>% 
#   group_by(species, location, flowering_type, source) %>% 
#   summarise(upper = median(value),
#             upper_plus = quantile(value, 0.75, na.rm = TRUE))
# 
# timewindow_df <- merge.data.frame(f_lower_sum, f_upper_sum, by = c('species', 'location', 'flowering_type', 'source'))
# 
# 
# stations <-  read.csv('data/weather_ready/weather_station_phenological_observations.csv')
# 
# 
# 
#make predictions for the actual weather data
# library(tidyverse)
# cka <- read.csv('data/weather_ready/cka_clean.csv') %>%
#   filter(Year < 2022)
# cieza <- read.csv('data/weather_ready/cieza_clean_patched.csv')
# sfax <- read.csv('data/weather_ready/sfax_clean.csv')
# meknes <- read.csv('data/weather_ready/meknes_clean.csv')
# zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv') %>%
#   filter(Year < 2022)
# santomera <- read.csv('data/weather_ready/murcia_clean.csv')
# 
# 
# weather_list_obs <- list('Klein-Altendorf' = cka,
#                          'Cieza' = cieza,
#                          'Zaragoza' = zaragoza,
#                          'Sfax' = sfax,
#                          'Meknes' = meknes,
#                          'Santomera' = santomera)
# weather_list_pred <- weather_list_obs
# 
# 
# #numbers for table 1
# purrr::map(weather_list_pred, function(x){
#   x %>%
#     filter(Year >= 1990, Year <= 2020) %>%
#     summarise(mean_Tmin = mean(Tmin),
#               mean_Tmax = mean(Tmax))
# })
# 
# 
# 
# frost_threshold <- 0
# heat_threshold <- 32
# observation_df <- adamedor %>% 
#   filter(!(species %in% c( 'Peach', 'Olive')))
# 
# 
# source('code/utilities/get_thermal_timewindow.R')
# 
# thermal_window <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
#                                                                                                                   weather_list_pred = weather_list_obs, 
#                                                                                                                   observation_df = observation_df, 
#                                                                                                                   frost_threshold = frost_threshold, 
#                                                                                                                   heat_threshold = heat_threshold, 
#                                                                                                                   target_col_obs = x,
#                                                                                                                   padding = 0.05)) %>% 
#   bind_rows() %>% 
#   mutate(padding = 0.05)
# 
# 
# 
# #redid the anaylsis with some extra tolerance
# window_df <- thermal_window %>% 
#   mutate(flowering_type = recode(flowering_type, f5 = 'begin_flowering_f5', f50 = 'flowering_f50'),
#          lower =  min_doy_padded,
#          upper = max_doy_padded,
#          method = 'frost / heat risk',
#          species = tolower(species)) %>% 
#   dplyr::select(location, species, flowering_type, method, lower, upper)
# 
# window_df <- timewindow_df %>% 
#   mutate(method = 'transfer empirical windows',
#          species = tolower(species)) %>% 
#   dplyr::select(location, species, flowering_type, method, lower, upper) %>% 
#   rbind(window_df)
# 
# 
# 
# window_df %>% 
#   mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
#          species = stringr::str_to_title(species),
#          flowering_label = recode(flowering_type, begin_flowering_f5 = '10% Flowering',
#                                   flowering_f50 = '50% Flowering'),
#          method_label = recode(method, `frost / heat risk` = 'Thermal Risk', 
#                                `transfer empirical windows` = 'Transfer Ranges'),
#          species_label = recode(species, 
#                                 `European Plum` = 'Europ. Plum',
#                                 `Japanese Plum` = 'Jap. Plum')) %>% 
#   ggplot(aes(x = loc, fill = method_label)) +
#   geom_rect(aes(ymin = lower, ymax = upper, xmin = loc- 0.2, xmax = loc + 0.2), position = position_dodge()) +
#   scale_x_continuous(breaks = 1:6, labels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')) +
#   scale_y_continuous(breaks= c(-31, 0, 32, 60, 91, 121, 152, 182, 213), 
#                      minor_breaks = c(-15, 15, 46, 74, 105, 135, 166, 194), 
#                      labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug')) +
#   scale_fill_manual(values = c("steelblue", "#E69F00"), name = 'Flowering Window Method') +
#   ylab('Month') +
#   xlab('Location') +
#   facet_grid(flowering_label~species_label) +
#   theme_bw(base_size = 15) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')
# ggsave('figures/paper/example_timewindow.jpeg', height = 20, width = 27, units = 'cm', device = 'jpeg')
# 
# 
# 
# 
# 
# #---------------------------------------------------------#
# # Visualize the frost and heat risk calculation
# #---------------------------------------------------------#
# 
# 
# #species to do the analysis for
# species <- unique(observation_df$species)
# 
# 
# #check if DATE is present, if so make it to Date
# weather_list_obs <- purrr::map(weather_list_obs, function(x){
#   if('date' %in% tolower(colnames(x)) & ('Date' %in% colnames(x)) == FALSE){
#     
#     #find out which version is present
#     i <- which(tolower(colnames(x)) == 'date')[1]
#     
#     
#     x$Date <- x[,i]
#     
#     return(x)
#   } else if('date' %in% tolower(colnames(x)) == FALSE){
#     stop('weather_list_obs does not contain a column which is called "Date", "DATE" or written in a different case. All weather data need a date column for the function to work')
#   } else { 
#     return(x)}
# })
# 
# #check if DATE is present, if so make it to Date
# weather_list_pred <- purrr::map(weather_list_pred, function(x){
#   if('date' %in% tolower(colnames(x)) & ('Date' %in% colnames(x)) == FALSE){
#     
#     #find out which version is present
#     i <- which(tolower(colnames(x)) == 'date')[1]
#     
#     
#     x$Date <- x[,i]
#     
#     return(x)
#   } else if('date' %in% tolower(colnames(x)) == FALSE){
#     stop('weather_list_obs does not contain a column which is called "Date", "DATE" or written in a different case. All weather data need a date column for the function to work')
#   } else { 
#     return(x)}
# })
# 
# 
# target_col_obs <- 'flowering_f50'
# 
# #get frost and heat risk for each day of the year
# frost_risk <- purrr::map(weather_list_obs, function(x){
#   x %>% 
#     mutate(doy = lubridate::yday(Date)) %>% 
#     group_by(doy) %>% 
#     summarise(chance_frost = sum(Tmin <= frost_threshold) / n()) %>% 
#     mutate(run_mean_frost = chillR::runn_mean(chance_frost, runn_mean = run_mean_window))
# }) %>% 
#   bind_rows(.id = 'location') %>% 
#   tidyr::expand_grid(species = species)
# 
# 
# #heat risk for each day of the year
# heat_risk <- purrr::map(weather_list_obs, function(x){
#   x %>% 
#     mutate(doy = lubridate::yday(Date)) %>% 
#     group_by(doy) %>% 
#     summarise(chance_heat = sum(Tmax >= heat_threshold) / n()) %>% 
#     mutate(run_mean_heat = chillR::runn_mean(chance_heat, runn_mean = run_mean_window))
# }) %>% 
#   bind_rows(.id = 'location') %>% 
#   tidyr::expand_grid(species = species)
# 
# 
# #save the target column in a different column, which I can access by name
# observation_df$target_pheno <- observation_df[, target_col_obs]
# 
# 
# #summarize the phenology data, know for each day of the year how many flowering records were measured for each species and location
# flower_sum <- observation_df %>% 
#   mutate(doy_pheno = lubridate::yday(target_pheno)) %>% 
#   group_by(species, location, doy_pheno) %>% 
#   summarise(n_flower = n())
# 
# 
# 
# max_frost <- observation_df %>%
#   mutate(doy_pheno = lubridate::yday(target_pheno)) %>%
#   dplyr::select(species, location, cultivar, flowering_f50, doy_pheno) %>%
#   merge.data.frame(frost_risk, by.x = c('species', 'location', 'doy_pheno'), by.y = c('species', 'location', 'doy'), all = TRUE) %>%
#   na.omit() %>%
#   group_by(species) %>%
#   summarise(max_risk_frost = max(run_mean_frost))
# 
# max_heat <- observation_df %>%
#   mutate(doy_pheno = lubridate::yday(target_pheno)) %>%
#   dplyr::select(species, location, cultivar, flowering_f50, doy_pheno) %>%
#   merge.data.frame(heat_risk, by.x = c('species', 'location', 'doy_pheno'), by.y = c('species', 'location', 'doy'), all = TRUE) %>%
#   na.omit() %>%
#   group_by(species) %>%
#   summarise(max_risk_heat = max(run_mean_heat))
# 
# observation_df$target_doy <- lubridate::yday(observation_df$target_pheno)
# 
# 
# 
# frost_risk %>% 
#   #merge(by.x  = c('species', 'location', 'doy'), by.y  = c('species', 'location', 'doy_pheno'), all.x = TRUE) %>% 
#   ggplot(aes(x = doy, y = run_mean_frost, col = location)) +
#   geom_line() +
#   geom_point(data = merge.data.frame(flower_sum, frost_risk, by.y  = c('species', 'location', 'doy'), by.x  = c('species', 'location', 'doy_pheno'), all.x = TRUE), 
#              aes(x = doy_pheno))+
#   geom_hline(data = max_frost, aes(yintercept = max_risk_frost), linetype = 'dashed') +
#   geom_hline(data = max_frost, aes(yintercept = max_risk_frost + 0.05)) +
#   scale_color_discrete(breaks = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')) +
#   facet_wrap(~species, ncol = 4) +
#   coord_cartesian(xlim = c(0, 170)) +
#   scale_x_continuous(breaks= c(0, 32, 60, 91, 121, 152), 
#                      #minor_breaks = c(15, 46, 74, 105, 135), 
#                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
#   xlab('')+
#   ylab('Risk to get temperature < 0\u00b0C') +
#   theme_bw(base_size = 15)
# ggsave('figures/frost_risk_for_timewindow.jpeg', height = 20, width = 30, units = 'cm', device = 'jpeg')
# 
# heat_risk %>% 
#   #merge(by.x  = c('species', 'location', 'doy'), by.y  = c('species', 'location', 'doy_pheno'), all.x = TRUE) %>% 
#   ggplot(aes(x = doy, y = run_mean_heat, col = location)) +
#   geom_line() +
#   geom_point(data = merge.data.frame(flower_sum, heat_risk, by.y  = c('species', 'location', 'doy'), by.x  = c('species', 'location', 'doy_pheno'), all.x = TRUE), 
#              aes(x = doy_pheno))+
#   geom_hline(data = max_heat, aes(yintercept = max_risk_heat), linetype = 'dashed') +
#   geom_hline(data = max_heat, aes(yintercept = max_risk_heat + 0.05)) +
#   scale_color_discrete(breaks = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')) +
#   coord_cartesian(xlim = c(0, 170)) +
#   scale_x_continuous(breaks= c(0, 32, 60, 91, 121, 152), 
#                      #minor_breaks = c(15, 46, 74, 105, 135), 
#                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
#   facet_wrap(~species, ncol = 4) +
#   xlab('')+
#   ylab('Risk to get temperature > 32\u00b0C') +
#   theme_bw(base_size = 15)
# ggsave('figures/heat_risk_for_timewindow.jpeg', height = 20, width = 30, units = 'cm', device = 'jpeg')
# 
# 
# 
# 
# #--------------------------------------------------------------#
# # Thermal Risk: Padding ####
# #--------------------------------------------------------------#
# 
# #make the plot of the window comparison
# #draw empty plot with the empirical time windows
# 
# 
# rm(list = ls())
# 
# adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv') %>% 
#   filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),
#          !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014))
# 
# sub <- adamedor %>% 
#   filter(species != 'Olive') %>% 
#   group_by(species, cultivar) %>% 
#   summarise(n = n()) %>% 
#   filter(n >= 20)
# 
# sub %>% 
#   group_by(species) %>% 
#   summarise(n = length(unique(cultivar)))
# 
# 
# # sub_per_loc <- adamedor %>% 
# #   filter(cultivar %in% unique(sub$cultivar)) %>% 
# #   group_by(cultivar, species, location) %>% 
# #   summarise(min_year = min(year),
# #             max_year = max(year),
# #             n = n())
# # write.csv(sub_per_loc, 'data/summary_table_cultivars.csv', row.names = FALSE)
# 
# flower_summarized <- adamedor %>% 
#   group_by(species, location) %>% 
#   mutate(begin_flowering_f5 = lubridate::yday(begin_flowering_f5), 
#          flowering_f50 = lubridate::yday(flowering_f50)) %>% 
#   summarize(mean.begin_flowering_f5 = mean(begin_flowering_f5, na.rm = TRUE),
#             sd.begin_flowering_f5 = sd(begin_flowering_f5, na.rm = TRUE),
#             max_dist.begin_flowering_f5 = max(abs(begin_flowering_f5 - mean(begin_flowering_f5, na.rm = TRUE)), na.rm = TRUE) * 1.5,
#             mean.flowering_f50 = mean(flowering_f50, na.rm = TRUE),
#             sd.flowering_f50 = sd(flowering_f50, na.rm = TRUE),
#             max_dist.flowering_f50 = max(abs(flowering_f50 - mean(flowering_f50, na.rm = TRUE)), na.rm = TRUE) * 1.5) %>% 
#   reshape2::melt(id.vars = c('species', 'location')) %>% 
#   mutate(value = replace(value, value %in% c(NaN, NA, -Inf), NA)) %>% 
#   separate(col = variable, into = c('variable', 'flowering_type'), sep = '\\.') %>% 
#   reshape2::dcast(species + location + flowering_type ~ variable, value.var = 'value') %>% 
#   relocate(species, location, flowering_type, mean, sd, max_dist) %>% 
#   mutate(location = as.factor(location),
#          species = recode(species, 
#                           `European plum` = "European Plum", 
#                           `Japanese plum` = "Japanese Plum") ) %>% 
#   mutate(upper = mean + max_dist,
#          lower = mean - max_dist) %>% 
#   dplyr::filter(location %in% c('Sfax', 'Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf', 'Santomera'),
#                 !(species %in% c('Peach', 'Olive'))) %>% 
#   mutate(location = factor(location, levels = c('Klein-Altendorf','Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))) %>% 
#   na.omit()
# 
# source('code/utilities/time_window_translation.R')
# 
# f_mean <- est_phen_gaps(target_df = flower_summarized, target_col = 'mean', split_col = 'flowering_type')
# f_sd <- est_phen_gaps(target_df = flower_summarized, target_col = 'sd', split_col = 'flowering_type')
# f_lower <- est_phen_gaps(target_df = flower_summarized, target_col = 'lower', split_col = 'flowering_type')
# f_upper <- est_phen_gaps(target_df = flower_summarized, target_col = 'upper', split_col = 'flowering_type')
# 
# 
# #maybe take median and then look how the time windows would be
# 
# f_lower_sum <- f_lower %>% 
#   group_by(species, location, flowering_type, source) %>% 
#   summarise(lower = median(value),
#             lower_plus = quantile(value, 0.25, na.rm = TRUE))
# 
# f_upper_sum <- f_upper %>% 
#   group_by(species, location, flowering_type, source) %>% 
#   summarise(upper = median(value),
#             upper_plus = quantile(value, 0.75, na.rm = TRUE))
# 
# timewindow_df <- merge.data.frame(f_lower_sum, f_upper_sum, by = c('species', 'location', 'flowering_type', 'source'))
# 
# 
# stations <-  read.csv('data/weather_ready/weather_station_phenological_observations.csv')
# 
# 
# 
# #make predictions for the actual weather data
# cka <- read.csv('data/weather_ready/cka_clean.csv') %>% 
#   filter(Year < 2022)
# cieza <- read.csv('data/weather_ready/cieza_clean_patched.csv')
# sfax <- read.csv('data/weather_ready/sfax_clean.csv')
# meknes <- read.csv('data/weather_ready/meknes_clean.csv')
# zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv') %>% 
#   filter(Year < 2022)
# santomera <- read.csv('data/weather_ready/murcia_clean.csv')
# 
# 
# weather_list_obs <- list('Klein-Altendorf' = cka,
#                          'Cieza' = cieza,
#                          'Zaragoza' = zaragoza,
#                          'Sfax' = sfax,
#                          'Meknes' = meknes,
#                          'Santomera' = santomera)
# weather_list_pred <- weather_list_obs
# 
# 
# #numbers for table 1
# # purrr::map(weather_list_pred, function(x){
# #   x %>% 
# #     filter(Year >= 1990, Year <= 2020) %>% 
# #     summarise(mean_Tmin = mean(Tmin),
# #               mean_Tmax = mean(Tmax))
# # })
# 
# 
# 
# frost_threshold <- 0
# heat_threshold <- 32
# observation_df <- adamedor %>% 
#   filter(!(species %in% c( 'Peach', 'Olive')))
# 
# 
# source('code/utilities/get_thermal_timewindow.R')
# 
# thermal_window_0 <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
#                                                                                                                   weather_list_pred = weather_list_obs, 
#                                                                                                                   observation_df = observation_df, 
#                                                                                                                   frost_threshold = frost_threshold, 
#                                                                                                                   heat_threshold = heat_threshold, 
#                                                                                                                   target_col_obs = x,
#                                                                                                                   padding = 0.0)) %>% 
#   bind_rows() %>% 
#   mutate(padding = 0.0)
# 
# thermal_window_2 <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
#                                                                                                                   weather_list_pred = weather_list_obs, 
#                                                                                                                   observation_df = observation_df, 
#                                                                                                                   frost_threshold = frost_threshold, 
#                                                                                                                   heat_threshold = heat_threshold, 
#                                                                                                                   target_col_obs = x,
#                                                                                                                   padding = 0.02)) %>% 
#   bind_rows() %>% 
#   mutate(padding = 0.02)
# 
# thermal_window_5 <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
#                                                                                                                   weather_list_pred = weather_list_obs, 
#                                                                                                                   observation_df = observation_df, 
#                                                                                                                   frost_threshold = frost_threshold, 
#                                                                                                                   heat_threshold = heat_threshold, 
#                                                                                                                   target_col_obs = x,
#                                                                                                                   padding = 0.05)) %>% 
#   bind_rows() %>% 
#   mutate(padding = 0.05)
# 
# thermal_window_10 <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
#                                                                                                                    weather_list_pred = weather_list_obs, 
#                                                                                                                    observation_df = observation_df, 
#                                                                                                                    frost_threshold = frost_threshold, 
#                                                                                                                    heat_threshold = heat_threshold, 
#                                                                                                                    target_col_obs = x,
#                                                                                                                    padding = 0.1)) %>% 
#   bind_rows() %>% 
#   mutate(padding = 0.1)
# 
# 
# 
# window_df <- thermal_window_0 %>%
#   rbind(thermal_window_2) %>% 
#   rbind(thermal_window_5) %>% 
#   rbind(thermal_window_10) %>% 
#   mutate(flowering_type = recode(flowering_type, f5 = 'begin_flowering_f5', f50 = 'flowering_f50'),
#          lower =  min_doy_padded,
#          upper = max_doy_padded,
#          method = 'frost / heat risk',
#          species = tolower(species)) %>% 
#   dplyr::select(location, species, flowering_type, method, lower, upper, padding)
# 
# 
# 
# window_df %>% 
#   mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))),
#          species = stringr::str_to_title(species),
#          flowering_label = recode(flowering_type, begin_flowering_f5 = '10% Flowering',
#                                   flowering_f50 = '50% Flowering'),
#          method_label = recode(method, `frost / heat risk` = 'Thermal Risk', 
#                                `transfer empirical windows` = 'Transfer Ranges'),
#          species_label = recode(species, 
#                                 `European Plum` = 'Europ. Plum',
#                                 `Japanese Plum` = 'Jap. Plum')) %>% 
#   ggplot(aes(x = loc, fill = as.factor(padding))) +
#   geom_rect(aes(ymin = lower, ymax = upper, xmin = loc- 0.2, xmax = loc + 0.2), position = position_dodge()) +
#   scale_x_continuous(breaks = 1:6, labels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')) +
#   scale_y_continuous(breaks= c(-31, 0, 32, 60, 91, 121, 152, 182, 213), 
#                      minor_breaks = c(-15, 15, 46, 74, 105, 135, 166, 194), 
#                      labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug')) +
#   #scale_fill_manual(values = c("steelblue", "#E69F00"), name = 'Flowering Window Method') +
#   ylab('Month') +
#   xlab('Location') +
#   facet_grid(flowering_label~species_label) +
#   theme_bw(base_size = 15) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')
# 
# 
# 
# 
# 
# pheno_current <- read.csv('data/projected_bloomdates_ensemble_observed_weather.csv') %>% 
#   mutate(species = tolower(species),
#          pheno_predicted = pred,
#          flowering_type = ifelse(species %in% c('apple', 'almond', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50'))
# 
# 
# 
# fail_ther_current <- pheno_current %>% 
#   merge.data.frame(window_df, by = c('species', 'location', 'flowering_type')) %>% 
#   group_by(location, species, cultivar, padding) %>% 
#   summarise(failure_rate = sum(pheno_predicted < lower | pheno_predicted > upper) / n(),
#             dist_window = min(c(abs(pheno_predicted - lower), abs(pheno_predicted - upper))),
#             too_early = sum(pheno_predicted < lower) / n(),
#             too_late = sum(pheno_predicted > upper) / n(),
#             window_type = 'thermal')
# 
# 
# p1 <- fail_ther_current %>% 
#   mutate(species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(species %in% c('Almond', 'Apple', 'Apricot')) %>% 
#   ggplot(aes(x = failure_rate, y = cultivar, fill = factor(padding))) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_y_discrete(limits=rev) +
#   theme_bw(base_size = 15) +
#   ylab('Cultivar') +
#   xlab('Share of Predicted Flowering Outside Time Window') +
#   scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') 
# #scale_fill_manual(values = c("steelblue", "#E69F00"), breaks = c('thermal', 'sudoku'), labels = c('Thermal Risk', 'Transfer Ranges'))+
# #theme(legend.title = 'Method to construct\ntime window') +
# #guides(fill=guide_legend(title="Flowering Window Method"))
# p1
# 
# p2 <- fail_ther_current %>% 
#   mutate(species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot'))) %>% 
#   ggplot(aes(x = failure_rate, y = cultivar, fill = factor(padding))) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_y_discrete(limits=rev) +
#   theme_bw(base_size = 15) +
#   ylab('Cultivar') +
#   xlab('Share of Predicted Flowering Outside Time Window') +
#   scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') 
# p2
# #padding by 2% did not solve the problem.........
# 
# 
# p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position= 'bottom') 
# ggsave('figures/paper/failure_rate_current.jpeg', device = 'jpeg',
#        height = 25, width = 30, units = 'cm')
# 
# 
# 
# 
# #look how the missclassifications change with different padding
# 
# 
# 
# 
# 
# # window_df <- thermal_window %>% 
# #   mutate(flowering_type = recode(flowering_type, f5 = 'begin_flowering_f5', f50 = 'flowering_f50'),
# #          lower =  min_doy,
# #          upper = max_doy,
# #          method = 'frost / heat risk',
# #          species = tolower(species)) %>% 
# #   dplyr::select(location, species, flowering_type, method, lower, upper)
# 
# #redid the anaylsis with some extra tolerance
# window_df <- thermal_window %>% 
#   mutate(flowering_type = recode(flowering_type, f5 = 'begin_flowering_f5', f50 = 'flowering_f50'),
#          lower =  min_doy_padded,
#          upper = max_doy_padded,
#          method = 'frost / heat risk',
#          species = tolower(species)) %>% 
#   dplyr::select(location, species, flowering_type, method, lower, upper)
# 
# window_df <- timewindow_df %>% 
#   mutate(method = 'transfer empirical windows',
#          species = tolower(species)) %>% 
#   dplyr::select(location, species, flowering_type, method, lower, upper) %>% 
#   rbind(window_df)
# 
# 
# 
# window_df %>% 
#   mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Meknes', 'Sfax'))),
#          species = stringr::str_to_title(species),
#          flowering_label = recode(flowering_type, begin_flowering_f5 = '10% Flowering',
#                                   flowering_f50 = '50% Flowering'),
#          method_label = recode(method, `frost / heat risk` = 'Thermal Risk', 
#                                `transfer empirical windows` = 'Transfer Ranges'),
#          species_label = recode(species, 
#                                 `European Plum` = 'Europ. Plum',
#                                 `Japanese Plum` = 'Jap. Plum')) %>% 
#   ggplot(aes(x = loc, fill = method_label)) +
#   geom_rect(aes(ymin = lower, ymax = upper, xmin = loc- 0.2, xmax = loc + 0.2), position = position_dodge()) +
#   scale_x_continuous(breaks = 1:5, labels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Meknes', 'Sfax')) +
#   scale_y_continuous(breaks= c(-31, 0, 32, 60, 91, 121, 152, 182, 213), 
#                      minor_breaks = c(-15, 15, 46, 74, 105, 135, 166, 194), 
#                      labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug')) +
#   scale_fill_manual(values = c("steelblue", "#E69F00"), name = 'Flowering Window Method') +
#   ylab('Month') +
#   xlab('Location') +
#   facet_grid(flowering_label~species_label) +
#   theme_bw(base_size = 15) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')
# ggsave('figures/paper/example_timewindow.jpeg', height = 20, width = 27, units = 'cm', device = 'jpeg')
# 
# 
# 
# 
# 
# 
# 
# 
# #------------------------------------#
# # Phenology for observed weather #####
# #------------------------------------#
# 
# rm(cieza, cka, f_lower, f_lower_sum, f_upper, f_upper_sum, f_mean, f_sd, flower_summarized, meknes, observation_df, sfax, stations,
#         weather_list_obs, weather_list_pred, zaragoza)
# 
# pheno_current <-  read.csv('data/projected_bloomdates_ensemble_observed_weather.csv') %>% 
#   mutate(species = tolower(species)) 
# 
# 
# thermal_window <- thermal_window %>% 
#   mutate(species = tolower(species))
# 
# 
# #--> plot thermal window with predicted flowering dates
# p1 <- pheno_current %>% 
#   merge.data.frame(thermal_window, by = c('species', 'location', 'flowering_type')) %>% 
#   mutate(species = stringr::str_to_title(species)) %>% 
#   mutate(species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c')) %>% 
#   mutate(species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry'))) %>% 
#   mutate(location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(species %in% c('Almond', 'Apple', 'Apricot')) %>% 
#   ggplot(aes(x = pheno_predicted, y = cultivar)) +
#   geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
#                 fill = 'Thermal Risk')) +
#   geom_point(alpha = 0.2) +
#   scale_fill_manual(values = 'steelblue') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   theme_bw(base_size = 15) +
#   ylab('Cultivar') +
#   xlab('Predicted Day of Flowering') +
#   scale_x_continuous(breaks = c(1,  182, 335), 
#                      labels = c('Jan', 'Jul', 'Dec'),
#                      minor_breaks = c(32, 60,91, 121, 152, 213, 244, 274, 305, 365)) +
#   scale_y_discrete(limits=rev) +
#   guides(fill=guide_legend(title="Flowering Window Method")) +
#   coord_cartesian(xlim = c(0, 365)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')
# 
# p2 <- pheno_current %>% 
#   merge.data.frame(thermal_window, by = c('species', 'location', 'flowering_type')) %>% 
#   mutate(species = stringr::str_to_title(species)) %>% 
#   mutate(species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c')) %>% 
#   mutate(species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry'))) %>% 
#   mutate(location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot'))) %>% 
#   ggplot(aes(x = pheno_predicted, y = cultivar)) +
#   geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
#                 fill = 'Thermal Risk')) +
#   geom_point(alpha = 0.2) +
#   scale_fill_manual(values = 'steelblue') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   theme_bw(base_size = 15) +
#   ylab('') +
#   xlab('Predicted Day of Flowering') +
#   scale_x_continuous(breaks = c(1,  182, 335), 
#                      labels = c('Jan', 'Jul', 'Dec'),
#                      minor_breaks = c(32, 60,91, 121, 152, 213, 244, 274, 305, 365)) +
#   scale_y_discrete(limits=rev) +
#   guides(fill=guide_legend(title="Flowering Window Method")) +
#   coord_cartesian(xlim = c(0, 365)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')
# 
# p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position= 'bottom') 
# ggsave('figures/paper/current_flowering_thermal_risk_time_window.jpeg', device = 'jpeg',
#        height = 25, width = 30, units = 'cm')
# 
# 
# timewindow_df <- timewindow_df %>% 
#   mutate(species = tolower(species))
# 
# #sudoku
# p1 <- pheno_current %>% 
#   merge.data.frame(timewindow_df, by = c('species', 'location', 'flowering_type')) %>% 
#   mutate(species = stringr::str_to_title(species)) %>% 
#   mutate(species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c')) %>% 
#   mutate(species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry'))) %>% 
#   mutate(location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(species %in% c('Almond', 'Apple', 'Apricot')) %>% 
#   ggplot(aes(x = pheno_predicted, y = cultivar)) +
#   geom_rect(aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf,
#                 fill = 'Transfer Ranges')) +
#   geom_point(alpha = 0.2) +
#   scale_fill_manual(values = "#E69F00") +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   theme_bw(base_size = 15) +
#   ylab('Cultivar') +
#   xlab('Predicted Day of Flowering') +
#   scale_x_continuous(breaks = c(1,  182, 335), 
#                      labels = c('Jan', 'Jul', 'Dec'),
#                      minor_breaks = c(32, 60,91, 121, 152, 213, 244, 274, 305, 365)) +
#   scale_y_discrete(limits=rev) +
#   guides(fill=guide_legend(title="Flowering Window Method")) +
#   coord_cartesian(xlim = c(0, 365)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')
# 
# p2 <- pheno_current %>% 
#   merge.data.frame(thermal_window, by = c('species', 'location', 'flowering_type')) %>% 
#   mutate(species = stringr::str_to_title(species)) %>% 
#   mutate(species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c')) %>% 
#   mutate(species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry'))) %>% 
#   mutate(location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot'))) %>% 
#   ggplot(aes(x = pheno_predicted, y = cultivar)) +
#   geom_rect(aes(xmin = min_doy, xmax = max_doy, ymin = -Inf, ymax = Inf,
#                 fill = 'Transfer Ranges')) +
#   geom_point(alpha = 0.2) +
#   scale_fill_manual(values = "#E69F00") +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   theme_bw(base_size = 15) +
#   ylab('') +
#   xlab('Predicted Day of Flowering') +
#   scale_x_continuous(breaks = c(1,  182, 335), 
#                      labels = c('Jan', 'Jul', 'Dec'),
#                      minor_breaks = c(32, 60,91, 121, 152, 213, 244, 274, 305, 365)) +
#   scale_y_discrete(limits=rev) +
#   guides(fill=guide_legend(title="Flowering Window Method")) +
#   coord_cartesian(xlim = c(0, 365)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')
# 
# p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position= 'bottom') 
# ggsave('figures/paper/current_flowering_sudoku_time_window.jpeg', device = 'jpeg',
#        height = 25, width = 30, units = 'cm')
# 
# 
# 
# #if needed I can make these plots for each scenario and weather source
# 
# 
# 
# 
# #-------------------------------------------#
# # Shift in bloom date ####
# #------------------------------------------#
# 
# #make sudoku time window easier to work with
# sudoku_timewindow <- timewindow_df %>% 
#   mutate(species = tolower(species),
#          min_doy = lower,
#          max_doy = upper) %>% 
#   dplyr::select(species, location, flowering_type, min_doy, max_doy)
# 
# 
# 
# pheno_hist <- read.csv('data/projected_bloomdates_ensemble_historic_scenarios.csv')  %>% 
#   mutate(species = tolower(species)) 
# 
# pheno_future <- read.csv('data/projected_bloomdates_ensemble.csv') %>% 
#   tidyr::separate(species_cultivar, into = c('species', 'cultivar'), sep = '_' ) %>% 
#   mutate(species = tolower(species)) 
# 
# 
# #get median flowering date of historic simulation
# pheno_hist_sum <- pheno_hist %>% 
#   group_by(species, cultivar, location) %>% 
#   summarise(med_current = median(pheno_predicted))
# 
# 
# 
# 
# 
# 
# 
# 
# pheno_future_sum <- pheno_future %>% 
#   mutate(flowering_type = ifelse(species %in% c('apple', 'almond', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50')) %>% 
#   group_by(species, cultivar, location, ssp, scenario_year, gcm, flowering_type) %>% 
#   summarise(med_future = median(pheno_predicted)) %>% 
#   merge.data.frame(sudoku_timewindow, by = c('species', 'flowering_type', 'location'), all.x = TRUE) %>% 
#   merge.data.frame(pheno_hist_sum, by = c('species', 'cultivar', 'location')) %>% 
#   #calculate based on the position of median what a safe shift would be (to not leave the window)
#   mutate(safe_shift_lower = min_doy - med_current,
#          safe_shift_upper = max_doy - med_current,
#          shift = med_future - med_current,
#          cultivar = tolower(cultivar),
#          species = tolower(species),
#          location = tolower(location),
#          year = scenario_year,
#          safe_shift_happend = shift >= safe_shift_lower & shift <= safe_shift_upper) 
# 
# 
# p1 <- pheno_future_sum %>% 
#   mutate(species = stringr::str_to_title(species),
#          location = stringr::str_to_title(location),
#          species_label = recode(species, 
#                                 `European Plum` = 'a',
#                                 `Japanese Plum`  = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          cultivar = stringr::str_to_title(cultivar),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(year == '2050') %>% 
#   filter(species %in% c('Almond', 'Apple', 'Apricot')) %>% 
#   ggplot(aes(x = shift, y = cultivar, col = ssp)) +
#   geom_rect(aes(xmin = safe_shift_lower, xmax = safe_shift_upper, ymin = cultivar, ymax = cultivar ), col = 'grey50') + 
#   geom_point(aes(alpha = safe_shift_happend)) +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y')+
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_y_discrete(limits=rev) +
#   ylab('Cultivar') +
#   xlab('Shift in median bloom date (days)') +
#   theme_bw(base_size = 15)
# 
# p2 <- pheno_future_sum %>% 
#   mutate(species = stringr::str_to_title(species),
#          location = stringr::str_to_title(location),
#          species_label = recode(species, 
#                                 `European Plum` = 'a',
#                                 `Japanese Plum`  = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          cultivar = stringr::str_to_title(cultivar),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(year == '2050') %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot'))) %>% 
#   ggplot(aes(x = shift, y = cultivar, col = ssp)) +
#   geom_rect(aes(xmin = safe_shift_lower, xmax = safe_shift_upper, ymin = cultivar-0.1, ymax = cultivar+0.1 ), col = 'grey50') + 
#   geom_point(aes(alpha = safe_shift_happend)) +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y')+
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_y_discrete(limits=rev) +
#   ylab('') +
#   xlab('Shift in median bloom date (days)') +
#   theme_bw(base_size = 15)
# 
# p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position= 'bottom') 
# ggsave('figures/paper/shift_bloom_date_sudoku.jpeg', device = 'jpeg',
#        height = 25, width = 30, units = 'cm')
# 
# 
# 
# 
# 
# 
# 
# 
# pheno_fut_sum <- pheno_future %>% 
#   mutate(flowering_type = ifelse(species %in% c('apple', 'almond', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50')) %>% 
#   group_by(species, cultivar, location, ssp, scenario_year, flowering_type) %>% 
#   summarise(med_future = median(pheno_predicted))
# 
# shift_df <- pheno_fut_sum %>% 
#   merge.data.frame(pheno_hist_sum, by = c('species', 'cultivar', 'location')) %>% 
#   merge.data.frame(sudoku_timewindow, by = c('species', 'flowering_type', 'location'), all.x = TRUE)
# 
# 
# 
# 
# #same information but as a barplot
# 
# #get number of cultivars
# ncult <- shift_df %>% 
#   group_by(species) %>% 
#   summarise(cult =unique(cultivar)) %>% 
#   ungroup() %>% 
#   nrow()
# 
# #assign number to cultivars, keep order as in the plot
# cult_num_df <- shift_df %>% 
#   group_by(species) %>% 
#   summarise(cultivar =unique(cultivar)) %>% 
#   ungroup() %>% 
#   mutate(cult_num  =ncult:1)
# 
# 
# 
# 
# 
# p1 <- shift_df %>% 
#   merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
#   mutate(species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.'),
#          dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
#   filter((species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2050') %>% 
#   ggplot(ggplot2::aes(y = cult_num)) +
#   geom_rect(aes(xmin = min_doy, xmax = max_doy, ymin = -Inf, ymax = Inf,
#                 fill = 'Time Window: "Transfer Ranges"'), alpha = 0.3) +
#   geom_rect(aes(xmin = med_current, xmax = med_future, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
#   geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
#                       show.legend = FALSE, shape = 18) + 
#   geom_rect(aes(xmin = med_current - 2.5, xmax = med_current + 2.5, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(1,  182, 335), 
#                      labels = c('Jan', 'Jul', 'Dec'),
#                      minor_breaks = c(32, 60,91, 121, 152, 213, 244, 274, 305, 365)) +
#   coord_cartesian(xlim = c(1, 365)) +
#   ggh4x::facetted_pos_scales(y = list(
#     species_label == 'Almond' ~ scale_y_continuous(limits = c(80,117), breaks = 80:117, labels = cult_num_df$cultivar[38:1]),
#     species_label == 'Apple' ~ scale_y_continuous(limits = c(71, 79), breaks = 71:79, labels = cult_num_df$cultivar[47:39]),
#     species_label == 'Apricot' ~ scale_y_continuous(limits = c(58, 70), breaks = 58:70, labels = cult_num_df$cultivar[60:48]))
#   ) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Median Predicted Bloomdate') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# p1
# 
# #have different values for up and down depending on ssp?
# 
# 
# 
# 
# p2 <- shift_df %>% 
#   merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
#   mutate(species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.'),
#          dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2050') %>% 
#   ggplot(ggplot2::aes(y = cult_num)) +
#   geom_rect(aes(xmin = min_doy, xmax = max_doy, ymin = -Inf, ymax = Inf,
#                 fill = 'Time Window: "Transfer Ranges"'), alpha = 0.3) +
#   geom_rect(aes(xmin = med_current, xmax = med_future, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
#   geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
#              show.legend = FALSE, shape = 18) + 
#   geom_rect(aes(xmin = med_current - 2.5, xmax = med_current + 2.5, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(1,  182, 335), 
#                      labels = c('Jan', 'Jul', 'Dec'),
#                      minor_breaks = c(32, 60,91, 121, 152, 213, 244, 274, 305, 365)) +
#   coord_cartesian(xlim = c(1, 365)) +
#   ggh4x::facetted_pos_scales(y = list(
#     species_label == 'a' ~ ggplot2::scale_y_continuous(limits = c(57-0.5, 57+ 0.5), breaks = 57, labels = cult_num_df$cultivar[61]),
#     species_label == 'b' ~ ggplot2::scale_y_continuous(limits = (c(56-0.5, 56+0.5)), breaks = 56, labels = cult_num_df$cultivar[62]),
#     species_label == 'Pear' ~ ggplot2::scale_y_continuous(limits = (c(40, 55)), breaks = 40:55, labels = cult_num_df$cultivar[63:78]),
#     species_label == 'c' ~ ggplot2::scale_y_continuous(limits = (c(38-0.5, 39+0.5)), breaks = 38:39, labels = cult_num_df$cultivar[80:79]),
#     species_label == 'Sweet Cherry' ~ ggplot2::scale_y_continuous(limits = (c(1, 37)), breaks = 1:37, labels = cult_num_df$cultivar[117:81]))
#   ) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Median Predicted Bloomdate') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# 
# 
# p1 + p2 + plot_layout(guides = 'collect') & ggplot2::theme(legend.position= 'bottom') 
# ggplot2::ggsave('figures/paper/change_bloomdate_sudoku_v2.jpeg', device = 'jpeg',
#                 height = 25, width = 30, units = 'cm')
# 
# 
# 
# #I would need to calculate the thermal window for each scenario seperately but in the plot
# # I am using the current thermal risks instead
# # for the actual failure calculations I did that
# 
# shift_df <- pheno_fut_sum %>% 
#   merge.data.frame(pheno_hist_sum, by = c('species', 'cultivar', 'location')) %>% 
#   merge.data.frame(thermal_window, by = c('species', 'flowering_type', 'location'), all.x = TRUE)
# 
# 
# p1 <- shift_df %>% 
#   merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
#   mutate(species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.'),
#          dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
#   filter((species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2050') %>% 
#   ggplot(ggplot2::aes(y = cult_num)) +
#   geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
#                 fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
#   geom_rect(aes(xmin = med_current, xmax = med_future, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
#   geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
#              show.legend = FALSE, shape = 18) + 
#   geom_rect(aes(xmin = med_current - 2.5, xmax = med_current + 2.5, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(1,  182, 335), 
#                      labels = c('Jan', 'Jul', 'Dec'),
#                      minor_breaks = c(32, 60,91, 121, 152, 213, 244, 274, 305, 365)) +
#   coord_cartesian(xlim = c(1, 365)) +
#   ggh4x::facetted_pos_scales(y = list(
#     species_label == 'Almond' ~ scale_y_continuous(limits = c(80,117), breaks = 80:117, labels = cult_num_df$cultivar[38:1]),
#     species_label == 'Apple' ~ scale_y_continuous(limits = c(71, 79), breaks = 71:79, labels = cult_num_df$cultivar[47:39]),
#     species_label == 'Apricot' ~ scale_y_continuous(limits = c(58, 70), breaks = 58:70, labels = cult_num_df$cultivar[60:48]))
#   ) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Median Predicted Bloomdate') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# p1
# 
# #have different values for up and down depending on ssp?
# 
# 
# 
# 
# p2 <- shift_df %>% 
#   merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
#   mutate(species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.'),
#          dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2050') %>% 
#   ggplot(ggplot2::aes(y = cult_num)) +
#   geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
#                 fill = 'Time Window: "Thermal Risk"'), alpha = 0.3) +
#   geom_rect(aes(xmin = med_current, xmax = med_future, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
#   geom_point(aes(x = med_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
#              show.legend = FALSE, shape = 18) + 
#   geom_rect(aes(xmin = med_current - 2.5, xmax = med_current + 2.5, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00", 'grey70'))+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(1,  182, 335), 
#                      labels = c('Jan', 'Jul', 'Dec'),
#                      minor_breaks = c(32, 60,91, 121, 152, 213, 244, 274, 305, 365)) +
#   coord_cartesian(xlim = c(1, 365)) +
#   ggh4x::facetted_pos_scales(y = list(
#     species_label == 'a' ~ ggplot2::scale_y_continuous(limits = c(57-0.5, 57+ 0.5), breaks = 57, labels = cult_num_df$cultivar[61]),
#     species_label == 'b' ~ ggplot2::scale_y_continuous(limits = (c(56-0.5, 56+0.5)), breaks = 56, labels = cult_num_df$cultivar[62]),
#     species_label == 'Pear' ~ ggplot2::scale_y_continuous(limits = (c(40, 55)), breaks = 40:55, labels = cult_num_df$cultivar[63:78]),
#     species_label == 'c' ~ ggplot2::scale_y_continuous(limits = (c(38-0.5, 39+0.5)), breaks = 38:39, labels = cult_num_df$cultivar[80:79]),
#     species_label == 'Sweet Cherry' ~ ggplot2::scale_y_continuous(limits = (c(1, 37)), breaks = 1:37, labels = cult_num_df$cultivar[117:81]))
#   ) +
#   theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Median Predicted Bloomdate') +
#   guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# 
# 
# p1 + p2 + plot_layout(guides = 'collect') & ggplot2::theme(legend.position= 'bottom') 
# ggplot2::ggsave('figures/paper/change_bloomdate_thermal_v2.jpeg', device = 'jpeg',
#                 height = 25, width = 30, units = 'cm')
# 
# 
# 
# 
# 
# 
# 
# #----------------------------------------------------------#
# # Failure rates ####
# #----------------------------------------------------------#
# 
# rm(list = ls())
# 
# adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv') %>% 
#   filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),
#          !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014))
# 
# 
# flower_summarized <- adamedor %>% 
#   group_by(species, location) %>% 
#   mutate(begin_flowering_f5 = lubridate::yday(begin_flowering_f5), 
#          flowering_f50 = lubridate::yday(flowering_f50)) %>% 
#   summarize(mean.begin_flowering_f5 = mean(begin_flowering_f5, na.rm = TRUE),
#             sd.begin_flowering_f5 = sd(begin_flowering_f5, na.rm = TRUE),
#             max_dist.begin_flowering_f5 = max(abs(begin_flowering_f5 - mean(begin_flowering_f5, na.rm = TRUE)), na.rm = TRUE) * 1.5,
#             mean.flowering_f50 = mean(flowering_f50, na.rm = TRUE),
#             sd.flowering_f50 = sd(flowering_f50, na.rm = TRUE),
#             max_dist.flowering_f50 = max(abs(flowering_f50 - mean(flowering_f50, na.rm = TRUE)), na.rm = TRUE) * 1.5) %>% 
#   reshape2::melt(id.vars = c('species', 'location')) %>% 
#   mutate(value = replace(value, value %in% c(NaN, NA, -Inf), NA)) %>% 
#   separate(col = variable, into = c('variable', 'flowering_type'), sep = '\\.') %>% 
#   reshape2::dcast(species + location + flowering_type ~ variable, value.var = 'value') %>% 
#   relocate(species, location, flowering_type, mean, sd, max_dist) %>% 
#   mutate(location = as.factor(location),
#          species = recode(species, 
#                           `European plum` = "European Plum", 
#                           `Japanese plum` = "Japanese Plum") ) %>% 
#   mutate(upper = mean + max_dist,
#          lower = mean - max_dist) %>% 
#   dplyr::filter(location %in% c('Sfax', 'Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf', 'Santomera'),
#                 !(species %in% c('Peach', 'Olive'))) %>% 
#   mutate(location = factor(location, levels = c('Klein-Altendorf','Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))) %>% 
#   na.omit()
# 
# source('code/utilities/time_window_translation.R')
# 
# f_lower <- est_phen_gaps(target_df = flower_summarized, target_col = 'lower', split_col = 'flowering_type')
# f_upper <- est_phen_gaps(target_df = flower_summarized, target_col = 'upper', split_col = 'flowering_type')
# 
# 
# #maybe take median and then look how the time windows would be
# 
# f_lower_sum <- f_lower %>% 
#   group_by(species, location, flowering_type, source) %>% 
#   summarise(lower = median(value),
#             lower_plus = quantile(value, 0.25, na.rm = TRUE))
# 
# f_upper_sum <- f_upper %>% 
#   group_by(species, location, flowering_type, source) %>% 
#   summarise(upper = median(value),
#             upper_plus = quantile(value, 0.75, na.rm = TRUE))
# 
# sudoku_timewindow <- merge(f_lower_sum, f_upper_sum, 
#                            by = c('species', 'location', 'flowering_type', 'source')) %>% 
#   mutate(species = tolower(species),
#          min_doy = lower,
#          max_doy = upper) %>% 
#   dplyr::select(species, location, flowering_type, min_doy, max_doy)
# 
# 
# stations <-  read.csv('data/weather_ready/weather_station_phenological_observations.csv')
# 
# 
# 
# #make predictions for the actual weather data
# cka <- read.csv('data/weather_ready/cka_clean.csv') %>% 
#   filter(Year < 2022)
# cieza <- read.csv('data/weather_ready/cieza_clean_patched.csv')
# sfax <- read.csv('data/weather_ready/sfax_clean.csv')
# meknes <- read.csv('data/weather_ready/meknes_clean.csv')
# zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv') %>% 
#   filter(Year < 2022)
# santomera <- read.csv('data/weather_ready/murcia_clean.csv')
# 
# 
# weather_list_obs <- list('Klein-Altendorf' = cka,
#                          'Cieza' = cieza,
#                          'Zaragoza' = zaragoza,
#                          'Sfax' = sfax,
#                          'Meknes' = meknes,
#                          'Santomera' = santomera)
# weather_list_pred <- weather_list_obs
# 
# 
# 
# #numbers for table 1
# # purrr::map(weather_list_pred, function(x){
# #   x %>% 
# #     filter(Year >= 1990, Year <= 2020) %>% 
# #     summarise(mean_Tmin = mean(Tmin),
# #               mean_Tmax = mean(Tmax))
# # })
# 
# 
# 
# frost_threshold <- 0
# heat_threshold <- 32
# observation_df <- adamedor %>% 
#   filter(!(species %in% c( 'Peach', 'Olive')))
# 
# 
# source('code/utilities/get_thermal_timewindow.R')
# 
# thermal_window <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
#                                                                                                                 weather_list_pred = weather_list_obs, 
#                                                                                                                 observation_df = observation_df, 
#                                                                                                                 frost_threshold = frost_threshold, 
#                                                                                                                 heat_threshold = heat_threshold, 
#                                                                                                                 target_col_obs = x,
#                                                                                                                 padding = 0.05)) %>% 
#   bind_rows() %>% 
#   mutate(species = tolower(species))
# 
# 
# 
# 
# pheno_current <-  read.csv('data/projected_bloomdates_ensemble_observed_weather.csv') %>% 
#   mutate(species = tolower(species)) %>% 
#   mutate(flowering_type = ifelse(species %in% c('almond', 'apple', 'european plum', 'japanese plum'), 
#                                  yes = 'begin_flowering_f5', 
#                                  no = 'flowering_f50'))
# 
# 
# 
# fail_su_current <- pheno_current %>% 
#   merge(sudoku_timewindow, by = c('species', 'location', 'flowering_type')) %>% 
#   group_by(location, species, cultivar) %>% 
#   summarise(failure_rate = sum(pred < min_doy | pred > max_doy) / n(),
#             dist_window = min(c(abs(pred - min_doy), abs(pred - max_doy))),
#             too_early = sum(pred < min_doy) / n(),
#             too_late = sum(pred > max_doy) / n(),
#             window_type = 'sudoku')
# 
# fail_ther_current <- pheno_current %>% 
#   merge(thermal_window, by = c('species', 'location', 'flowering_type')) %>% 
#   group_by(location, species, cultivar) %>% 
#   summarise(failure_rate = sum(pred < min_doy_padded | pred > max_doy_padded) / n(),
#             dist_window = min(c(abs(pred - min_doy_padded), abs(pred - max_doy_padded))),
#             too_early = sum(pred < min_doy_padded) / n(),
#             too_late = sum(pred > max_doy_padded) / n(),
#             window_type = 'thermal')
# 
# rm(pheno_current, cka, santomera, cieza, zaragoza, sfax, meknes, stations, flower_summarized, f_upper, f_sd, f_lower,
#    f_lower_sum, f_upper_sum, observation_df, weather_list_obs, weather_list_pred)
# 
# #make type 1 error
# 
# type2 <- fail_su_current %>% 
#   rbind(fail_ther_current) %>% 
#   mutate(species = stringr::str_to_title(species),
#          location_label = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter((species %in% c('Apricot', 'Apple', 'European Plum', 'Japanese Plum', 'Pear', 'Sweet Cherry') & location == 'Sfax') |
#          (species == 'Almond' & location %in% c('Klein-Altendorf', 'Zaragoza'))) %>% 
#   mutate(type_two_error = 1 - failure_rate,
#          location_label = factor(location_label, levels = c('Kl.-Alt.', 'Zarag.', 'Sfax'))) %>% 
#   ggplot(aes(y = type_two_error, x = species, fill = window_type)) +
#   geom_boxplot() +
#   facet_grid(~location_label, scales = 'free_x', space = 'free_x') +
#   theme_bw(base_size = 15) +
#   xlab('') +
#   ylab('Type II error') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   scale_fill_manual(values = c("steelblue", "#E69F00"), breaks = c('thermal', 'sudoku'), labels = c('Thermal Risk', 'Transfer Ranges'))+
#   #theme(legend.title = 'Method to construct\ntime window') +
#   guides(fill=guide_legend(title="Flowering Window Method"))
# 
# type1 <- fail_su_current %>% 
#   rbind(fail_ther_current) %>% 
#   mutate(species = stringr::str_to_title(species),
#          location_label = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                                  Zaragoza = 'Zarag.'),
#          location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Meknes', 'Sfax'))) %>% 
#   filter((location == 'Klein-Altendorf' & species %in% c('Apple', 'European Plum', 'Japanese Plum', 'Pear', 'Sweet Cherry')) |
#            (location == 'Cieza' & species %in% c('Apricot')) |
#            (location == 'Zaragoza' & species %in% c('Apricot', 'Sweet Cherry', 'Pear')) |
#            (location == 'Meknes' & species %in% c('Almond', 'Apple')) |
#            (location == 'Sfax' & species %in% c('Almond', 'Pistachio'))) %>% 
#   ggplot(aes(y = failure_rate, x = species, fill = window_type)) +
#   geom_boxplot() +
#   facet_grid(~location, scales = 'free_x', space = 'free_x') +
#   theme_bw(base_size = 15) +
#   xlab('') +
#   ylab('Type I error') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   scale_fill_manual(values = c("steelblue", "#E69F00"), breaks = c('thermal', 'sudoku'), labels = c('Thermal Risk', 'Transfer Ranges')) +
#   #theme(legend.title = 'Method to construct\ntime window') +
#   guides(fill=guide_legend(title="Flowering Window Method"))
# 
# design <- 
# "
# AB
# "
# 
# list(type1, type2) |>
#   wrap_plots() +
#   plot_layout(guides = 'collect', widths = c(1.5, 1), design = design) & theme(legend.position= 'bottom') & plot_annotation(tag_levels = 'A') 
# ggsave('figures/paper/error_types_timewindow.jpeg', height = 20, width = 30, units = 'cm', device = 'jpeg')
# 
# 
# 
# p1 <- fail_su_current %>% 
#   rbind(fail_ther_current) %>% 
#   mutate(species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'ep',
#                                 `Japanese Plum` = 'jp',
#                                 Pistachio = 'pi'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'ep', 'jp', 'Pear', 'pi', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(species %in% c('Almond', 'Apple', 'Apricot')) %>% 
#   ggplot(aes(x = failure_rate, y = cultivar, fill = window_type)) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_y_discrete(limits=rev) +
#   theme_bw(base_size = 15) +
#   ylab('Cultivar') +
#   xlab('Share of Predicted Flowering Outside Time Window') +
#   scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   scale_fill_manual(values = c("steelblue", "#E69F00"), breaks = c('thermal', 'sudoku'), labels = c('Thermal Risk', 'Transfer Ranges'))+
#   #theme(legend.title = 'Method to construct\ntime window') +
#   guides(fill=guide_legend(title="Flowering Window Method"))
# 
# p2 <- fail_su_current %>% 
#   rbind(fail_ther_current) %>% 
#   mutate(species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot'))) %>% 
#   ggplot(aes(x = failure_rate, y = cultivar, fill = window_type)) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_y_discrete(limits=rev) +
#   theme_bw(base_size = 15) +
#   ylab('') +
#   xlab('Share of Predicted Flowering Outside Time Window') +
#   scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   scale_fill_manual(values = c("steelblue", "#E69F00"), breaks = c('thermal', 'sudoku'), labels = c('Thermal Risk', 'Transfer Ranges'))+
#   #theme(legend.title = 'Method to construct\ntime window') +
#   guides(fill=guide_legend(title="Flowering Window Method"))
# 
# #padding by 2% did not solve the problem.........
# 
# 
# p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position= 'bottom') 
# ggsave('figures/paper/failure_rate_current.jpeg', device = 'jpeg',
#        height = 25, width = 30, units = 'cm')
# 
# 
# 
# #change in failure rate using the sudoku approach
# 
# pheno_hist <- read.csv('data/projected_bloomdates_ensemble_historic_scenarios.csv')  %>% 
#   mutate(species = tolower(species)) 
# 
# pheno_future <- read.csv('data/projected_bloomdates_ensemble.csv') %>% 
#   tidyr::separate(species_cultivar, into = c('species', 'cultivar'), sep = '_' ) %>% 
#   mutate(species = tolower(species)) 
# 
# 
# #summarize historic failure rate
# fail_su_hist <- pheno_hist %>% 
#   merge.data.frame(sudoku_timewindow, by = c('species', 'location', 'flowering_type')) %>% 
#   group_by(location, species, cultivar) %>% 
#   summarise(failure_rate_hist = sum(pheno_predicted < min_doy | pheno_predicted > max_doy) / n())
# 
# #summarize future failure rate
# fail_su_future <- pheno_future %>% 
#   mutate(flowering_type = ifelse(species %in% c('apple', 'almond', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50')) %>% 
#   merge.data.frame(sudoku_timewindow, by = c('species', 'location', 'flowering_type')) %>% 
#   group_by(location, species, cultivar, ssp, scenario_year, gcm) %>% 
#   summarise(failure_rate_future = sum(pheno_predicted < min_doy | pheno_predicted > max_doy) / n()) %>% 
#   group_by(location, species, cultivar, ssp, scenario_year) %>% 
#   summarise(sd = sd(failure_rate_future),
#             failure_rate_future = median(failure_rate_future))
# 
# 
# p1 <- fail_su_future %>% 
#   merge.data.frame(fail_su_hist, by = c('species', 'cultivar', 'location')) %>% 
#   mutate(change_failure = failure_rate_future - failure_rate_hist) %>% 
#   mutate(species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter((species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2050') %>% 
#   ggplot(aes(y = cultivar)) +
#   geom_point(aes(x = failure_rate_future, col = ssp)) + 
#   geom_segment(aes(x = failure_rate_hist, xend = failure_rate_future, y = cultivar, yend = cultivar, col = ssp),
#                arrow = arrow(length = unit(0.15, "cm")), show.legend = FALSE) +
#   geom_point(aes(x = failure_rate_hist, col = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_y_discrete(limits=rev) +
#   scale_color_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Share of Predicted Flowering Outside Time Window') +
#   guides(fill=guide_legend(title="Weather Scenario"))
# 
# p2 <- fail_su_future %>% 
#   merge.data.frame(fail_su_hist, by = c('species', 'cultivar', 'location')) %>% 
#   mutate(change_failure = failure_rate_future - failure_rate_hist) %>% 
#   mutate(species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.')) %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2050') %>% 
#   ggplot(aes(y = cultivar)) +
#   geom_point(aes(x = failure_rate_future, col = ssp)) + 
#   geom_segment(aes(x = failure_rate_hist, xend = failure_rate_future, y = cultivar, yend = cultivar, col = ssp),
#                arrow = arrow(length = unit(0.15, "cm")), show.legend = FALSE) +
#   geom_point(aes(x = failure_rate_hist, col = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   scale_y_discrete(limits=rev) +
#   scale_color_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00"), )+
#   theme_bw(base_size = 15) +
#   scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ylab('') +
#   xlab('Share of Predicted Flowering Outside Time Window') +
#   guides(fill=guide_legend(title="Weather Scenario"))
# 
# 
# p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position= 'bottom') 
# ggsave('figures/paper/change_failure_rate_sudoku.jpeg', device = 'jpeg',
#        height = 25, width = 30, units = 'cm')
# 
# 
# 
# 
# 
# #same information but as a barplot
# 
# #get number of cultivars
# ncult <- fail_su_future %>% 
#   group_by(species) %>% 
#   summarise(cult =unique(cultivar)) %>% 
#   ungroup() %>% 
#   nrow()
# 
# #assign number to cultivars, keep order as in the plot
# cult_num_df <- fail_su_future %>% 
#   group_by(species) %>% 
#   summarise(cultivar =unique(cultivar)) %>% 
#   ungroup() %>% 
#   mutate(cult_num  =ncult:1)
# 
# 
# #install.packages('ggh4x')
# 
# p1 <- fail_su_future %>% 
#   merge.data.frame(fail_su_hist, by = c('species', 'cultivar', 'location')) %>% 
#   merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
#   mutate(change_failure = failure_rate_future - failure_rate_hist,
#          species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.'),
#          dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
#   filter((species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2050') %>% 
#   ggplot2::ggplot(ggplot2::aes(y = cult_num)) +
#   ggplot2::geom_rect(ggplot2::aes(xmin = failure_rate_hist, xmax = failure_rate_future, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
#   ggplot2::geom_point(ggplot2::aes(x = failure_rate_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
#              show.legend = FALSE, shape = 18) + 
#   ggplot2::geom_rect(ggplot2::aes(xmin = failure_rate_hist - .01, xmax = failure_rate_hist + .01, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   ggplot2::facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   ggplot2::scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   ggplot2::scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   ggplot2::theme_bw(base_size = 15) +
#   ggplot2::scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
#   ggh4x::facetted_pos_scales(y = list(
#     species_label == 'Almond' ~ ggplot2::scale_y_continuous(limits = c(80,117), breaks = 80:117, labels = cult_num_df$cultivar[38:1]),
#     species_label == 'Apple' ~ ggplot2::scale_y_continuous(limits = c(71, 79), breaks = 71:79, labels = cult_num_df$cultivar[47:39]),
#     species_label == 'Apricot' ~ ggplot2::scale_y_continuous(limits = c(58, 70), breaks = 58:70, labels = cult_num_df$cultivar[60:48]))
#   ) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ggplot2::ylab('') +
#   ggplot2::xlab('Share of Predicted Flowering Outside Time Window') +
#   ggplot2::guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# p1
# 
# #have different values for up and down depending on ssp?
# 
# 
# 
# 
# p2 <- fail_su_future %>% 
#   merge.data.frame(fail_su_hist, by = c('species', 'cultivar', 'location')) %>% 
#   merge.data.frame(cult_num_df, by = c('species', 'cultivar')) %>% 
#   mutate(change_failure = failure_rate_future - failure_rate_hist,
#          species = stringr::str_to_title(species),
#          species_label = recode(species, `European Plum` = 'a',
#                                 `Japanese Plum` = 'b',
#                                 Pistachio = 'c'),
#          species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
#          location = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
#                            Zaragoza = 'Zarag.'),
#          dodge_up = recode(ssp, ssp126 = -0.2, ssp245 = 0, ssp370 = 0.2, ssp585 = 0.4),
#          dodge_low = recode(ssp, ssp126 = -0.4, ssp245 = -0.2, ssp370 = 0, ssp585 = 0.2)) %>% 
#   filter(!(species %in% c('Almond', 'Apple', 'Apricot')),
#          scenario_year == '2050') %>% 
#   ggplot2::ggplot(ggplot2::aes(y = cult_num)) +
#   ggplot2::geom_rect(ggplot2::aes(xmin = failure_rate_hist, xmax = failure_rate_future, ymin = cult_num + dodge_low, ymax = cult_num + dodge_up, fill = ssp)) +
#   ggplot2::geom_point(ggplot2::aes(x = failure_rate_future, y = cult_num + ((dodge_low + dodge_up)/2),  col = ssp, fill = ssp), 
#                       show.legend = FALSE, shape = 18) + 
#   ggplot2::geom_rect(ggplot2::aes(xmin = failure_rate_hist - .01, xmax = failure_rate_hist + .01, ymax = cult_num - 0.4, ymin = cult_num + 0.4, fill = 'Simulation 2020'),  size = 2) +
#   # geom_bar(stat = 'identity', position = 'dodge') +
#   ggplot2::facet_grid(species_label~location, scales = 'free_y', space = 'free_y') +
#   ggplot2::scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   ggplot2::scale_fill_manual(values = c('black', "#56B4E9", "#009E73","#F0E442",  "#E69F00"))+
#   ggplot2::theme_bw(base_size = 15) +
#   ggplot2::scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
#   ggh4x::facetted_pos_scales(y = list(
#     species_label == 'a' ~ ggplot2::scale_y_continuous(limits = c(57-0.5, 57+ 0.5), breaks = 57, labels = cult_num_df$cultivar[61]),
#     species_label == 'b' ~ ggplot2::scale_y_continuous(limits = (c(56-0.5, 56+0.5)), breaks = 56, labels = cult_num_df$cultivar[62]),
#     species_label == 'Pear' ~ ggplot2::scale_y_continuous(limits = (c(40, 55)), breaks = 40:55, labels = cult_num_df$cultivar[63:78]),
#     species_label == 'c' ~ ggplot2::scale_y_continuous(limits = (c(38-0.5, 39+0.5)), breaks = 38:39, labels = cult_num_df$cultivar[80:79]),
#     species_label == 'Sweet Cherry' ~ ggplot2::scale_y_continuous(limits = (c(1, 37)), breaks = 1:37, labels = cult_num_df$cultivar[117:81]))
#   ) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
#   ggplot2::ylab('') +
#   ggplot2::xlab('Share of Predicted Flowering Outside Time Window') +
#   ggplot2::guides(fill=ggplot2::guide_legend(title="Weather Scenario"))
# 
# 
# library(patchwork)
# p1 + p2 + plot_layout(guides = 'collect') & ggplot2::theme(legend.position= 'bottom') 
# ggplot2::ggsave('figures/paper/change_failure_rate_sudoku_v2.jpeg', device = 'jpeg',
#        height = 25, width = 30, units = 'cm')
# 
# remove.packages('ggh4x')
# 

  # scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
  # scale_fill_manual(values = c("steelblue", "#E69F00"), breaks = c('thermal', 'sudoku'), labels = c('Thermal Risk', 'Transfer Ranges'))+
  #theme(legend.title = 'Method to construct\ntime window') +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  # guides(fill=guide_legend(title="Flowering Window Method"))

#
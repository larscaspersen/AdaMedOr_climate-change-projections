library(tidyverse)

performance <- read.csv('data/performance_fitted_models.csv')


performance %>% 
  ggplot(aes(x = rpiq_adj, fill = split)) +
  geom_histogram(position = 'identity', alpha = 0.5) +
  geom_vline(data = median_df, aes(xintercept = median_rpiq, linetype = split)) +
  facet_wrap(~species, scales = 'free_y')+
  coord_cartesian(xlim = c(0, 12))+
  theme_bw()



median_df <- performance %>% 
  group_by(species, split) %>% 
  summarize(median_rpiq =  median(rpiq_adj),
            median_rmse = median(rmse))

performance %>% 
  ggplot(aes(x = rmse, fill = split)) +
  geom_histogram(position = 'identity', alpha = 0.5) +
  geom_vline(data = median_df, aes(xintercept = median_rmse, linetype = split)) +
  facet_wrap(~species, scales = 'free_y')

#--> check the really poor predictions for apples?
#--> same for sweet cherry?

#seems there is a stronger mismatch between validation and calibration for apricot, pistachio than in almond, apple, plum, pear
#--> signs of overfitting?

#I think I should remove 





prediction_df <- read.csv('data/predicted_flowering_vs_observed.csv')

prediction_df <- prediction_df %>% 
  filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),  #seems to be outlier
         !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014), #seems to be outlier
         !(location == 'Klein-Altendorf' & year == 1958))  #weather data starts in 1958 so predicting that year does not make sense


iqr_df <- prediction_df %>% 
  filter(repetition == 1) %>% 
  group_by(species, cultivar) %>% 
  summarise(iqr_obs = IQR(pheno))


median_df <- performance %>% 
  group_by(species, split) %>% 
  summarize(median_rpiq =  median(rpiq_adj),
            median_rmse = median(rmse))

performance <- prediction_df %>% 
  merge.data.frame(iqr_df, by = c('species', 'cultivar'), all.x = TRUE) %>% 
  group_by(species, cultivar, repetition, split) %>% 
  summarise(rmse = chillR::RMSEP(predicted = pred, observed = pheno),
            iqr_obs = mean(iqr_obs) ,
            rpiq_adj = iqr_obs / rmse) 

median_df <- performance %>% 
  group_by(species, split) %>% 
  summarize(median_rpiq =  median(rpiq_adj),
            median_rmse = median(rmse))


performance %>% 
  ggplot(aes(x = rpiq_adj, fill = split)) +
  geom_histogram(position = 'identity', alpha = 0.5) +
  geom_vline(data = median_df, aes(xintercept = median_rpiq, linetype = split)) +
  facet_wrap(~species, scales = 'free_y')+
  theme_bw()



performance %>% 
  ggplot(aes(x = rmse, fill = split)) +
  geom_histogram(position = 'identity', alpha = 0.5) +
  geom_vline(data = median_df, aes(xintercept = median_rmse, linetype = split)) +
  facet_wrap(~species, scales = 'free_y')+
  theme_bw()



performance %>% 
  ggplot(aes(x = species, y = rmse, fill = split)) +
  geom_boxplot()+
  theme_bw()

p1 <- performance %>% 
  filter(split == 'Validation') %>% 
  ggplot(aes(y = species, x = rpiq_adj)) +
  geom_boxplot(aes(fill = species), show.legend = FALSE)+
  scale_y_discrete(limits = rev)+
  ylab('') +
  xlab('Ratio of Performance to Interquartile\nDistance (RPIQ) for Validation Data') + 
  geom_vline(xintercept = 1, linetype = 'dashed') +
#  ggsci::scale_fill_tron()+
#  scale_fill_manual(values = viridis::viridis(8))+
#  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw(base_size = 15) 
#ggsave('figures/paper/RPIQ_all_cultivars.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')


p2 <- performance %>% 
  filter(split == 'Validation') %>% 
  ggplot(aes(y = species, x = rmse)) +
  geom_boxplot(aes(fill = species), show.legend = FALSE)+
  scale_y_discrete(limits = rev)+
  ylab('') +
  xlab('Root Mean Square Error (RMSE) \nfor Validation Data') + 
  #geom_vline(xintercept = 1, linetype = 'dashed') +
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw(base_size = 15) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
#ggsave('figures/paper/RMSE_all_cultivars.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')

library(patchwork)
p1 + p2 + plot_annotation(tag_levels = 'A')
ggsave('figures/paper/performance_rpiq_rmse_population_individual.jpeg', 
       height = 15, width = 25, device = 'jpeg', units = 'cm')

performance %>% 
  filter(split == 'Validation') %>% 
  group_by(species) %>% 
  summarise(RMSE_q5 = quantile(rmse, 0.05),
            RMSE_q50 = median(rmse),
            RMSE_q95 = quantile(rmse, 0.95))

performance %>% 
  filter(split == 'Validation') %>% 
  group_by(species) %>% 
  summarise(RPIQ_q5 = quantile(rpiq_adj, 0.05),
            RPIQ_q50 = median(rpiq_adj),
            RPIQ_q95 = quantile(rpiq_adj, 0.95),
            share_RPIQ_low = sum(rpiq_adj < 1) / n(),
            n = n())



adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv') %>% 
  filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),
         !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014))


adamedor %>% 
  filter(cultivar %in% performance$cultivar,
         location %in% prediction_df$location,
         species == 'Almond') %>% 
  mutate(doy = lubridate::yday(begin_flowering_f5),
         performance = ifelse(cultivar %in% c('Cristomorto', 'Ksontini', 'Ne Plus Ultra', 'Cavaliera', 'Ramlet R249', 'Tuono', 'Marcona'),
                              yes = 'bad', 
                              no = ifelse(cultivar %in% c('Abiodh de Sfax', 'Achaak', 'Fakhfekh', 'Genco Taronto', 'Trell'), 
                                          yes = 'medium', no = 'good'))) %>% 
  ggplot(aes(y = cultivar, x = doy, fill = performance)) +
  geom_boxplot() +
  facet_grid(~location)



adamedor %>% 
  filter(cultivar %in% performance$cultivar,
         location %in% prediction_df$location,
         species == 'Almond') %>% 
  mutate(doy = lubridate::yday(begin_flowering_f5),
         performance = ifelse(cultivar %in% c('Cristomorto', 'Ksontini', 'Ne Plus Ultra', 'Cavaliera', 'Ramlet R249', 'Tuono', 'Marcona'),
                              yes = 'bad', 
                              no = ifelse(cultivar %in% c('Abiodh de Sfax', 'Achaak', 'Fakhfekh', 'Genco Taronto', 'Trell'), 
                                          yes = 'medium', no = 'good'))) %>% 
  group_by(cultivar) %>% 
  mutate(iqr = IQR(doy, na.rm = TRUE),
         range = max(doy, na.rm = TRUE) - min(doy, na.rm = TRUE),
         n = sum(is.na(doy) == FALSE)) %>% 
  ggplot(aes(y = cultivar, x = n, col = performance)) +
  geom_point(size =2) +
  facet_grid(~location)




performance %>% 
  filter(split == 'Validation') %>% 
  group_by(species) %>% 
  summarise(RPIQ_q5 = quantile(rpiq_adj, 0.05),
            RPIQ_q50 = median(rpiq_adj),
            RPIQ_q95 = quantile(rpiq_adj, 0.95),
            share_RPIQ_low = sum(rpiq_adj < 1) / n(),
            n = n()) %>% 
  ggplot(aes(x = cultivar, y = RPIQ_q50))

performance %>% 
  filter(species == 'Almond') %>% 
  filter(split == 'Validation') %>% 
  ggplot(aes(y = cultivar, x = rpiq_adj)) +
  geom_boxplot() +
  geom_vline(xintercept = 1)


performance %>% 
  filter(species == 'Almond') %>% 
  filter(split == 'Validation') %>% 
  group_by(cultivar) %>% 
  summarise(share_higher = sum(rpiq_adj >= 1) / n(),
            share_lower = sum(rpiq_adj < 1) / n()) %>% 
  reshape2::melt(id.vars = c('cultivar')) %>% 
  ggplot(aes(y = cultivar, x = value)) +
  geom_bar(stat = 'identity', aes(fill = variable))

performance %>% 
  filter(species == 'Apricot') %>% 
  filter(split == 'Validation') %>% 
  ggplot(aes(y = cultivar, x = rpiq_adj)) +
  geom_boxplot() +
  geom_vline(xintercept = 1)

performance %>% 
  filter(species == 'Apricot') %>% 
  filter(split == 'Validation') %>% 
  group_by(cultivar) %>% 
  summarise(share_higher = sum(rpiq_adj >= 1) / n(),
            share_lower = sum(rpiq_adj < 1) / n()) %>% 
  reshape2::melt(id.vars = c('cultivar')) %>% 
  ggplot(aes(y = cultivar, x = value)) +
  geom_bar(stat = 'identity', aes(fill = variable))




performance %>% 
  filter(species == 'Sweet Cherry') %>% 
  filter(split == 'Validation') %>% 
  ggplot(aes(y = cultivar, x = rpiq_adj)) +
  geom_boxplot() +
  geom_vline(xintercept = 1)

performance %>% 
  filter(species == 'Sweet Cherry') %>% 
  filter(split == 'Validation') %>% 
  group_by(cultivar) %>% 
  summarise(share_higher = sum(rpiq_adj >= 1) / n(),
            share_lower = sum(rpiq_adj < 1) / n()) %>% 
  reshape2::melt(id.vars = c('cultivar')) %>% 
  ggplot(aes(y = cultivar, x = value)) +
  geom_bar(stat = 'identity', aes(fill = variable))


performance %>% 
  filter(species == 'Pear') %>% 
  filter(split == 'Validation') %>% 
  ggplot(aes(y = cultivar, x = rpiq_adj)) +
  geom_boxplot() +
  geom_vline(xintercept = 1)

performance %>% 
  filter(species == 'Pear') %>% 
  filter(split == 'Validation') %>% 
  group_by(cultivar) %>% 
  summarise(share_higher = sum(rpiq_adj >= 1) / n(),
            share_lower = sum(rpiq_adj < 1) / n()) %>% 
  reshape2::melt(id.vars = c('cultivar')) %>% 
  ggplot(aes(y = cultivar, x = value)) +
  geom_bar(stat = 'identity', aes(fill = variable))

adamedor %>% 
  filter(cultivar %in% performance$cultivar,
         location %in% prediction_df$location,
         species == 'Pear') %>% 
  mutate(doy = lubridate::yday(flowering_f50),
         performance = ifelse(cultivar %in% c('Pasa Crasana', 'Highland'), yes = 'bad', no = 'good')) %>% 
  # group_by(cultivar) %>% 
  # mutate(iqr = IQR(doy, na.rm = TRUE),
  #        range = max(doy, na.rm = TRUE) - min(doy, na.rm = TRUE),
  #        n = sum(is.na(doy) == FALSE)) %>% 
  ggplot(aes(y = cultivar, x = doy, col = performance)) +
  # geom_point(size =2) +
  geom_boxplot() +
  facet_grid(~location)

performance %>% 
  filter(species == 'Apple') %>% 
  filter(split == 'Validation') %>% 
  ggplot(aes(y = cultivar, x = rpiq_adj)) +
  geom_boxplot() +
  geom_vline(xintercept = 1)

performance %>% 
  filter(species == 'Apple') %>% 
  filter(split == 'Validation') %>% 
  group_by(cultivar) %>% 
  summarise(share_higher = sum(rpiq_adj >= 1) / n(),
            share_lower = sum(rpiq_adj < 1) / n()) %>% 
  reshape2::melt(id.vars = c('cultivar')) %>% 
  ggplot(aes(y = cultivar, x = value)) +
  geom_bar(stat = 'identity', aes(fill = variable))

adamedor %>% 
  filter(cultivar %in% performance$cultivar,
         location %in% prediction_df$location,
         species == 'Apple') %>% 
  mutate(doy = lubridate::yday(flowering_f50),
         performance = ifelse(cultivar %in% c('Topaz', 'Elstar'), yes = 'bad', no = 'good')) %>% 
  # group_by(cultivar) %>% 
  # mutate(iqr = IQR(doy, na.rm = TRUE),
  #        range = max(doy, na.rm = TRUE) - min(doy, na.rm = TRUE),
  #        n = sum(is.na(doy) == FALSE)) %>% 
  ggplot(aes(y = cultivar, x = doy, col = performance)) +
  # geom_point(size =2) +
  geom_boxplot() +
  facet_grid(~location)


performance %>% 
  filter(species == 'Pistachio') %>% 
  filter(split == 'Validation') %>% 
  ggplot(aes(y = cultivar, x = rpiq_adj)) +
  geom_boxplot() +
  geom_vline(xintercept = 1)

performance %>% 
  filter(species == 'Pistachio') %>% 
  filter(split == 'Validation') %>% 
  group_by(cultivar) %>% 
  summarise(share_higher = sum(rpiq_adj >= 1) / n(),
            share_lower = sum(rpiq_adj < 1) / n()) %>% 
  reshape2::melt(id.vars = c('cultivar')) %>% 
  ggplot(aes(y = cultivar, x = value)) +
  geom_bar(stat = 'identity', aes(fill = variable))

adamedor %>% 
  filter(cultivar %in% performance$cultivar,
         location %in% prediction_df$location,
         species == 'Pistachio') %>% 
  mutate(doy = lubridate::yday(flowering_f50),
         performance = ifelse(cultivar %in% c('Topaz', 'Elstar'), yes = 'bad', no = 'good')) %>% 
  # group_by(cultivar) %>% 
  # mutate(iqr = IQR(doy, na.rm = TRUE),
  #        range = max(doy, na.rm = TRUE) - min(doy, na.rm = TRUE),
  #        n = sum(is.na(doy) == FALSE)) %>% 
  ggplot(aes(y = cultivar, x = doy, col = performance)) +
  # geom_point(size =2) +
  geom_boxplot() +
  facet_grid(~location)





adamedor %>% 
  filter(cultivar %in% performance$cultivar,
         location %in% prediction_df$location,
         species == 'Apricot') %>% 
  mutate(doy = lubridate::yday(flowering_f50),
         performance = ifelse(cultivar %in% c('Búlida', 'Pepito del Rubio'),
                              yes = 'bad', 
                              no = 'medium')) %>% 
  ggplot(aes(y = cultivar, x = doy, fill = performance)) +
  geom_boxplot() +
  facet_grid(~location)

adamedor %>% 
  filter(cultivar %in% performance$cultivar,
         location %in% prediction_df$location,
         species == 'Sweet Cherry') %>% 
  mutate(doy = lubridate::yday(flowering_f50),
         performance = ifelse(cultivar %in% c('Blanca de Provenza', 'Sandom Rose', 'Lapins', 'Lambert'),
                              yes = 'bad', 
                              no = 'medium')) %>% 
  ggplot(aes(y = cultivar, x = doy, fill = performance)) +
  geom_boxplot() +
  facet_grid(~location)






performance %>% 
  filter(split == 'Validation') %>% 
  group_by(species) %>% 
  summarise(share_lower_one = sum(rpiq_adj < 1) / n(),
            share_larger_one = sum(rpiq_adj >= 1) / n()) %>% 
  reshape2::melt(id.var = c('species')) %>% 
  ggplot(aes(y = species, x = value, fill = variable)) +
  geom_bar(stat = 'identity')+
  scale_y_discrete(limits = rev)+
  ylab('') +
  xlab('Share of fitted models with validation RPIQ larger than 1') + 
  #  ggsci::scale_fill_tron()+
  #  scale_fill_manual(values = viridis::viridis(8))+
  #  scale_fill_brewer(type = 'discrete', palette = 'Pastel2')+
  theme_bw()

  RColorBrewer::brewer.pal(6, "Accent")






performance %>% 
  group_by(species, split) %>% 
  summarise(min_rsme = min(rmse),
            q_05 = quantile(rmse, probs = 0.05),
            median_rmse = median(rmse),
            q_95 = quantile(rmse, probs = 0.95),
            max_rmse = max(rmse))


out <- performance %>% 
  filter(species == 'Almond', split == 'Calibration') %>% 
  group_by(cultivar, split) %>% 
  summarise(min_rpiq = min(rpiq_adj),
            median_rpiq = median(rpiq_adj),
            max_rpiq = max(rpiq_adj),
            case_below_zero = sum(rpiq_adj <= 1) / n()) %>% 
  arrange(case_below_zero)

out1 <- performance %>% 
  filter(species == 'Apple', split == 'Calibration') %>% 
  group_by(cultivar, split) %>% 
  summarise(min_rpiq = min(rpiq_adj),
            median_rpiq = median(rpiq_adj),
            max_rpiq = max(rpiq_adj),
            case_below_zero = sum(rpiq_adj <= 1) / n(),
            min_rmse = min(rmse),
            median_rmse = median(rmse),
            max_rmse = max(rmse),) %>% 
  arrange(case_below_zero)

out2 <- performance %>% 
  filter(species == 'Apple', split == 'Validation') %>% 
  group_by(cultivar, split) %>% 
  summarise(min_rpiq = min(rpiq_adj),
            median_rpiq = median(rpiq_adj),
            max_rpiq = max(rpiq_adj),
            case_below_zero = sum(rpiq_adj <= 1) / n(),
            min_rmse = min(rmse),
            median_rmse = median(rmse),
            max_rmse = max(rmse),) %>% 
  arrange(case_below_zero)




performance %>% 
  group_by(species, split) %>% 
  summarise(share_bad_rpiq = (sum(rpiq_adj < 1) / n()) * 100)

summary_rpiq <- performance %>% 
  filter(rpiq_adj < 1) %>% 
  group_by(species, cultivar, split) %>%
  summarise(n = n())




performance %>% 
  group_by(species, split) %>% 
  summarise(share_bad_rmse = (sum(rmse >= 10) / n()) * 100)

summary_rmse <- performance %>% 
  filter(rmse >= 10) %>% 
  group_by(species, cultivar, split) %>%
  summarise(n = n())

summary_rmse %>% 
  filter(species == 'Almond',
         cultivar %in% c('Cavaliera', 'Pizzuta', 'Ramlet R249', 'Tuono Taronto')) %>% 
  group_by(split) %>% 
  summarize(sum = sum(n))

summary_rmse %>% 
  filter(species == 'Almond') %>% 
  group_by(split) %>% 
  summarize(sum = sum(n))
#70% of CALIBRATION
#20% of validation cases


sum_species_level = prediction_df %>% 
  merge.data.frame(iqr_df, by = c('species', 'cultivar')) %>% 
  group_by(species, split) %>% 
  summarise(rmse = round(chillR::RMSEP(predicted = pred, observed = pheno),digits = 1),
            iqr_obs = mean(iqr_obs) ,
            rpiq_adj = round(iqr_obs / rmse, digits = 1),
            mean_bias = round(mean(pred - pheno), digits = 1))

prediction_df %>% 
  ggplot(aes(x = pheno, y = pred, col = split)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1) +
  facet_wrap(~species) +
  theme_bw()


performance %>% 
  group_by(species)




#---> strong differences in validation performance and calibration performance in apricots and to a lower extend sweeet cherry
#what could be the reason?

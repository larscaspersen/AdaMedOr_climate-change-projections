library(tidyverse)
library(ggplot2)
#library(readr)
library(chillR)
#library(devtools)
# install_github("larscaspersen/addition_chillR")
library(LarsChill)

#---------------------------------------------#
#decide on padding based on the error type 1 and type 2
#---------------------------------------------#

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
         !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014))


frost_threshold <- 0
heat_threshold <- 32
observation_df <- adamedor %>% 
  filter(!(species %in% c( 'Peach', 'Olive')))

pad <- seq(0, 0.1, by = 0.01)
therm_windows <- purrr::map(pad, function(p){
  
  purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) LarsChill::get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
                                                                                                                             weather_list_pred = weather_list_obs, 
                                                                                                                             observation_df = observation_df, 
                                                                                                                             frost_threshold = frost_threshold, 
                                                                                                                             heat_threshold = heat_threshold, 
                                                                                                                             target_col_obs = x,
                                                                                                                             padding = p)) %>% 
    bind_rows() %>% 
    mutate(padding = p)
}) %>% 
  bind_rows()


pheno_current <-  read.csv('data/projected_bloomdates_ensemble_observed_weather.csv') %>% 
  mutate(species = tolower(species),
         flowering_type = ifelse(species %in% c('apple', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50')) 



master_pheno <- read.csv('data/master_phenology_repeated_splits.csv') %>% 
  mutate(species = tolower(species)) %>% 
  filter(repetition == 1) %>% 
  merge(pheno_current, by.x = c('species', 'cultivar', 'location', 'measurement_type', 'year'), by = c('species', 'cultivar', 'location', 'flowering_type', 'scenario_year'),all.x = TRUE, all.y = FALSE) %>% 
  mutate(flowering_type = measurement_type) %>% 
  na.omit()


okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",'grey40' )


err1_df <- therm_windows %>% 
  mutate(species = tolower(species)) %>% 
  merge(master_pheno, by = c('species', 'flowering_type', 'location')) %>% 
  mutate(failure = pheno_predicted < min_doy_padded | pheno_predicted > max_doy_padded) %>% 
  group_by(species, cultivar, location, padding) %>% 
  summarise(failure_rate = sum(failure) / n())

err1_plot <- err1_df %>% 
  mutate(failure_rate = failure_rate * 100,
         padding = padding * 100) %>% 
  group_by(species, location, padding) %>% 
  summarise(mean_fail_rate = mean(failure_rate),
            sd_fail_rate = sd(failure_rate)) %>% 
  ungroup() %>% 
  mutate(species_label = recode(species, `european plum` = "europ. plum", `japanese plum` = "jap. plum"),
         species_label = str_to_title(species_label)) %>% 
  ggplot(aes(x = padding, y = mean_fail_rate)) +
  geom_point(aes(col = location), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean_fail_rate - sd_fail_rate, ymax = mean_fail_rate + sd_fail_rate,
                    col = location), position = position_dodge(width = 0.5))+
  geom_line(aes(col = location), position = position_dodge(width = 0.5))+
  coord_cartesian(ylim = c(0,112))+
  facet_grid(~species_label) +
  scale_x_continuous(breaks = c(0, 5, 10), minor_breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  ylab('Type 1 Error (%)') +
  xlab('') +
  scale_color_manual(breaks = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'),
                     labels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'),
                       values = okabe) +
  theme_bw(base_size = 15) 

#fail rate never higher than 3%, no failure at padding of 3% present
#only failures in meknes almond and apple

#look at type 2 error



err2_df <- therm_windows %>% 
  mutate(species = tolower(species)) %>% 
  merge(pheno_current, by = c('species', 'flowering_type', 'location')) %>% 
  mutate(failure = pheno_predicted < min_doy_padded | pheno_predicted > max_doy_padded) %>% 
  group_by(species, cultivar, location, padding) %>% 
  summarise(failure_rate = sum(failure) / n()) %>% 
  filter((species %in% c('apricot', 'apple', 'european plum', 'japanese plum', 'pear', 'sweet cherry') & location == 'Sfax') |
           (species %in% c('almond', 'pistachio') & location %in% c('Klein-Altendorf', 'Zaragoza'))) 

test <- err2_df %>% 
  group_by(species, location, padding) %>% 
  summarise(mean_fail_rate = mean(failure_rate),
            sd_fail_rate = sd(failure_rate))

err2_plot <- err2_df %>% 
  mutate(failure_rate = failure_rate * 100,
         padding = padding * 100) %>% 
  group_by(species, location, padding) %>% 
  summarise(mean_fail_rate = mean(failure_rate),
            sd_fail_rate = sd(failure_rate)) %>% 
  ungroup() %>% 
  mutate(species_label = recode(species, `european plum` = "europ. plum", `japanese plum` = "jap. plum"),
         species_label = str_to_title(species_label)) %>% 
  ggplot(aes(x = padding, y = mean_fail_rate)) +
  geom_point(aes(col = location), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean_fail_rate - sd_fail_rate, ymax = mean_fail_rate + sd_fail_rate,
                    col = location), position = position_dodge(width = 0.5))+
  geom_line(aes(col = location), position = position_dodge(width = 0.5))+
  coord_cartesian(ylim = c(0,112))+
  facet_grid(~species_label) +
  scale_x_continuous(breaks = c(0, 5, 10), minor_breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_color_manual(breaks = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'), 
                     labels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'),
                     values = okabe) +
  ylab('Predicted Failure Rate of Bloom (%)') +
  xlab('Added Thermal Risk Tolerance (%)') +
  theme_bw(base_size = 15) +
  guides(colour = "none")
#--> take padding of 3% and make this the standard. it minimizes the type 1 error and does not strongly increase the type 2 error

library(patchwork)

design <- 
  "
A
B
"

list(err1_plot, err2_plot) |>
  wrap_plots() +
  plot_layout(guides = 'collect', design = design) & theme(legend.position= 'bottom') 
ggsave('figures/effect_padding_errrortype.jpeg', height = 20, width = 30, units = 'cm', device = 'jpeg')



#choose 3% padding 




#-------------------#
#make the error plot of for 3% padding
#-------------------#

thermal_time_window <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) LarsChill::get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
                                                                                                         weather_list_pred = weather_list_obs, 
                                                                                                         observation_df = observation_df, 
                                                                                                         frost_threshold = frost_threshold, 
                                                                                                         heat_threshold = heat_threshold, 
                                                                                                         target_col_obs = x,
                                                                                                         padding = 0.03)) %>% 
  bind_rows()


err1_df <- thermal_time_window %>% 
  mutate(species = tolower(species)) %>% 
  merge(master_pheno, by = c('species', 'flowering_type', 'location')) %>% 
  mutate(failure = pheno_predicted < min_doy_padded | pheno_predicted > max_doy_padded) %>% 
  group_by(species, cultivar, location) %>% 
  summarise(failure_rate = (sum(failure) / n())*100 ) %>% 
  mutate(failure_type = 'Type I')

err2_df <- thermal_time_window %>% 
  mutate(species = tolower(species)) %>% 
  merge(pheno_current, by = c('species', 'flowering_type', 'location')) %>% 
  mutate(failure = pheno_predicted < min_doy_padded | pheno_predicted > max_doy_padded) %>% 
  group_by(species, cultivar, location) %>% 
  summarise(failure_rate = (sum(failure) / n())*100 ) %>% 
  filter((species %in% c('apricot', 'apple', 'european plum', 'japanese plum', 'pear', 'sweet cherry') & location == 'Sfax') |
           (species %in% c('almond', 'pistachio') & location %in% c('Klein-Altendorf', 'Zaragoza'))) %>% 
  mutate(failure_type = 'Type II')


err1_plot <- err1_df %>% 
  mutate(species = stringr::str_to_title(species),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))) %>% 
  mutate(species = recode(species, `European Plum` = 'Europ. Plum',
                          `Japanese Plum` = 'Jap. Plum')) %>% 
  ggplot(aes(x = species, y = failure_rate, fill = species)) +
  geom_boxplot() +
  ylim(c(0,100))+
  ylab('Type 1 Error (%)') +
  xlab('') +
  facet_grid(~location, scales = 'free_x', space = 'free_x') +
  scale_fill_manual(values = okabe) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

err2_plot <- err2_df %>% 
  mutate(species = stringr::str_to_title(species),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))) %>% 
  mutate(species = recode(species, `European Plum` = 'Europ. Plum',
                          `Japanese Plum` = 'Jap. Plum')) %>% 
  ggplot(aes(x = species, y = failure_rate, fill = species)) +
  geom_boxplot() +
  ylim(c(0,100))+
  ylab('Predicted Failure Rate of Bloom (%)') +
  xlab('')+
  scale_fill_manual(values = okabe) +
  facet_grid(~location, scales = 'free_x', space = 'free_x') +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


design <- 
  "
A
B
"

list(err1_plot, err2_plot) |>
  wrap_plots() +
  plot_layout(guides = 'collect', design = design) &
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position= 'none') 
ggsave('figures/paper/error_types.jpeg', height = 20, width = 30, units = 'cm', device = 'jpeg')


#-------------------------------#
#Calculate Failure Rate For Each Species x Cultivar
#-------------------------------#


#observed weather
failure_df_current <- thermal_time_window %>% 
  mutate(species = tolower(species)) %>% 
  merge(pheno_current, by = c('species', 'flowering_type', 'location')) %>% 
  mutate(failure = pheno_predicted < min_doy_padded | pheno_predicted > max_doy_padded) %>% 
  group_by(species, cultivar, location) %>% 
  summarise(failure_rate = (sum(failure) / n())*100 )


#modelled 2020
pheno_2020 <- read.csv('data/projected_bloomdates_ensemble_historic_scenarios.csv') %>% 
  mutate(species = tolower(species))

#make time window based on 2020 weather
hist_gen_weather <- chillR::load_temperature_scenarios('data/hist-sim-weather/', prefix = 'hist_gen_2015')

#adjust the column names
for(i in 1:length(hist_gen_weather)){
  colnames(hist_gen_weather[[i]]) <- c('DATE', 'Year', 'Month', 'Day', 'nodata', 'Tmin', 'Tmax')
}

#calculate time window for 2020 conditions
thermal_time_window_2020 <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) LarsChill::get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
                                                                                                                                weather_list_pred = hist_gen_weather, 
                                                                                                                                observation_df = observation_df, 
                                                                                                                                frost_threshold = frost_threshold, 
                                                                                                                                heat_threshold = heat_threshold, 
                                                                                                                                target_col_obs = x,
                                                                                                                                padding = 0.03)) %>% 
  bind_rows() %>% 
  mutate(species = tolower(species))

#calculate failure rates
failure_df_2020 <- thermal_time_window_2020 %>% 
  merge(pheno_2020, by = c('species', 'location', 'flowering_type')) %>% 
  mutate(failure = pheno_predicted < min_doy_padded | pheno_predicted > max_doy_padded) %>% 
  group_by(species, cultivar, location) %>% 
  summarise(failure_rate = (sum(failure) / n())*100 )




#------------------------------#
#failure rate future conditions
#------------------------------#

rm(err2_df, err1_df, err1_plot, err2_plot, adamedor, hist_gen_weather, master_pheno, pheno_2020, pheno_current,
   test, therm_windows, thermal_time_window, thermal_time_window_2020)

#read the future pheno data
pheno_future <- read.csv('data/projected_bloomdates_ensemble.csv')

#read the future weather data
future_weather_list <- chillR::load_temperature_scenarios('data/future_weather/', prefix = '')


#I need bloom windows for each ssp - gcm - time combination
location_vec <- names(future_weather_list) %>% 
  stringr::str_split('_') %>% 
  purrr::map_chr(1)

ssp_vec <- names(future_weather_list) %>% 
  stringr::str_split('_') %>% 
  purrr::map_chr(2) %>% 
  stringr::str_split('\\.') %>% 
  purrr::map_chr(1)

gcm_vec <- names(future_weather_list) %>% 
  stringr::str_split('_') %>% 
  purrr::map_chr(2) %>% 
  stringr::str_split('\\.') %>% 
  purrr::map_chr(2)

time_vec <- names(future_weather_list) %>% 
  stringr::str_split('_') %>% 
  purrr::map_chr(2) %>% 
  stringr::str_split('\\.') %>% 
  purrr::map_chr(3)


future_failure_df <- data.frame()

for(ssp in unique(ssp_vec)){
  #ssp <- 'ssp126'
  for(gcm in unique(gcm_vec)){
    #gcm <- 'CAS-ESM2-0'
    for(time in unique(time_vec)){
      #time <- '2085'
      
      #which ones to select
      select_element <- (ssp_vec == ssp) & (gcm_vec == gcm) & (time_vec == time)
      
      #bind the correct elements to a list
      weather_list_pred <- future_weather_list[select_element]  
      names(weather_list_pred) <- location_vec[select_element]
      
      #calculate thermal time window
      thermal_time_window <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) LarsChill::get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
                                                                                                               weather_list_pred = weather_list_pred, 
                                                                                                               observation_df = observation_df, 
                                                                                                               frost_threshold = frost_threshold, 
                                                                                                               heat_threshold = heat_threshold, 
                                                                                                               target_col_obs = x,
                                                                                                               padding = 0.03)) %>% 
        bind_rows() %>% 
        mutate(gcm = gcm,
               ssp = ssp,
               scenario_year = time,
               species = tolower(species))
      
      #calculate failure rate
      fail_df <- pheno_future %>% 
        separate(species_cultivar, into = c('species', 'cultivar'), sep = '_') %>% 
        mutate(species = tolower(species)) %>% 
        merge(thermal_time_window, by = c('species', 'location', 'gcm', 'ssp', 'scenario_year'), all.y = TRUE) %>% 
        na.omit() %>% 
        mutate(failure = pheno_predicted < min_doy_padded | pheno_predicted > max_doy_padded) %>% 
        group_by(species, cultivar, location) %>% 
        summarise(failure_rate = (sum(failure) / n())*100 ) %>% 
        mutate(ssp = ssp,
               gcm = gcm,
               scenario_year = time)
      
      #append to the overall data.frame
      future_failure_df <- rbind(future_failure_df, fail_df)
      
      
      
    }
  }
}

write.csv(future_failure_df, 'data/failure-rate_thermal-risk_future.csv', row.names = FALSE)
write.csv(failure_df_current, 'data/failure-rate_thermal-risk_current.csv', row.names = FALSE)
write.csv(failure_df_2020, 'data/failure-rate_thermal-risk_2020-sim.csv', row.names = FALSE)

#unique(future_failure_df$species)

#save the failure rates in files
#make the final figures in the figures script




unique(thermal_time_window_2020$species)
unique(pheno_2020$species)



  ggplot(aes(x = pheno_predicted, y = cultivar)) +
  geom_rect(aes(xmin = min_doy_padded, xmax = max_doy_padded, ymin = -Inf, ymax = Inf,
                fill = '"Sudoku"')) +
  geom_point(alpha = 0.2) +
  scale_fill_manual(values = 'lightsteelblue') +
  facet_grid(species~location, scales = 'free_y', space = 'free_y') +
  
  theme_bw(base_size = 15) +
  #theme(legend.title = 'Method to construct\ntime window') +
  guides(fill=guide_legend(title="Method to construct\ntime window")) +
  coord_cartesian(xlim = c(0, 365))
ggsave(filename = 'figures/timewindow_sudoku_current.jpeg',
       height = 40, width = 30, units = 'cm', device = 'jpeg')











# almond_sub <- adamedor %>% 
#   filter(species == 'Almond')
# 
# almond_sub %>% 
#   dplyr::select(species, cultivar, location, year, begin_flowering_f5) %>% 
#   na.omit() %>% 
#   group_by(species, cultivar) %>% 
#   summarise(n = n()) %>% 
#   filter(n >= 20) %>% 
#   summarise(n = sum(n))
# 
# almond_sub %>% 
#   dplyr::select(species, cultivar, location, year, flowering_f50) %>% 
#   na.omit() %>% 
#   group_by(species, cultivar) %>% 
#   summarise(n = n()) %>% 
#   filter(n >= 20) %>% 
#   summarise(n = sum(n))
# 
# 
# #calculate time window
# adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv') %>% 
#   filter(!(species == 'Apple' & location == 'Klein-Altendorf' & cultivar == 'Elstar' & year %in% c(2008, 2010)),
#          !(species == 'Apricot' & location == 'Cieza' & cultivar == 'Sublime' & year == 2014))



flower_summarized <- adamedor %>% 
  group_by(species, location) %>% 
  mutate(begin_flowering_f5 = lubridate::yday(begin_flowering_f5), 
         flowering_f50 = lubridate::yday(flowering_f50)) %>% 
  summarize(mean.begin_flowering_f5 = mean(begin_flowering_f5, na.rm = TRUE),
            sd.begin_flowering_f5 = sd(begin_flowering_f5, na.rm = TRUE),
            max_dist.begin_flowering_f5 = max(abs(begin_flowering_f5 - mean(begin_flowering_f5, na.rm = TRUE)), na.rm = TRUE) * 1.5,
            mean.flowering_f50 = mean(flowering_f50, na.rm = TRUE),
            sd.flowering_f50 = sd(flowering_f50, na.rm = TRUE),
            max_dist.flowering_f50 = max(abs(flowering_f50 - mean(flowering_f50, na.rm = TRUE)), na.rm = TRUE) * 1.5) %>% 
  reshape2::melt(id.vars = c('species', 'location')) %>% 
  mutate(value = replace(value, value %in% c(NaN, NA, -Inf), NA)) %>% 
  separate(col = variable, into = c('variable', 'flowering_type'), sep = '\\.') %>% 
  reshape2::dcast(species + location + flowering_type ~ variable, value.var = 'value') %>% 
  relocate(species, location, flowering_type, mean, sd, max_dist) %>% 
  mutate(location = as.factor(location),
         species = recode(species, 
                          `European plum` = "European Plum", 
                          `Japanese plum` = "Japanese Plum") ) %>% 
  mutate(upper = mean + max_dist,
         lower = mean - max_dist) %>% 
  dplyr::filter(location %in% c('Sfax', 'Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf', 'Santomera'),
                !(species %in% c('Peach', 'Olive'))) %>% 
  mutate(location = factor(location, levels = c('Klein-Altendorf','Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax'))) %>% 
  na.omit()

source('code/utilities/time_window_translation.R')

f_lower <- est_phen_gaps(target_df = flower_summarized, target_col = 'lower', split_col = 'flowering_type')
f_upper <- est_phen_gaps(target_df = flower_summarized, target_col = 'upper', split_col = 'flowering_type')


#maybe take median and then look how the time windows would be

f_lower_sum <- f_lower %>% 
  group_by(species, location, flowering_type, source) %>% 
  summarise(lower = median(value),
            lower_plus = quantile(value, 0.25, na.rm = TRUE))

f_upper_sum <- f_upper %>% 
  group_by(species, location, flowering_type, source) %>% 
  summarise(upper = median(value),
            upper_plus = quantile(value, 0.75, na.rm = TRUE))

transfer_ranges_df <- merge.data.frame(f_lower_sum, f_upper_sum, by = c('species', 'location', 'flowering_type', 'source'))


stations <-  read.csv('data/weather_ready/weather_station_phenological_observations.csv')



frost_threshold <- 0
heat_threshold <- 32
observation_df <- adamedor %>% 
  filter(!(species %in% c( 'Peach', 'Olive')))


thermal_window <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) LarsChill::get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
                                                                                                                           weather_list_pred = weather_list_obs, 
                                                                                                                           observation_df = observation_df, 
                                                                                                                           frost_threshold = frost_threshold, 
                                                                                                                           heat_threshold = heat_threshold, 
                                                                                                                           target_col_obs = x,
                                                                                                                           padding = 0.03)) %>% 
  bind_rows() 



#illustrate thermal time window
thermal_window %>% 
  mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')))) %>% 
  ggplot(aes(x = loc, fill = location)) +
  geom_rect(aes(ymin = min_doy, ymax = max_doy, xmin = loc- 0.2, xmax = loc + 0.2)) +
  scale_x_continuous(breaks = 1:6, labels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')) +
  facet_grid(flowering_type~species)

#illustrate "sudoku" time window
transfer_ranges_df %>% 
  mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')))) %>% 
  ggplot(aes(x = loc, fill = source)) +
  geom_rect(aes(ymax = upper_plus, ymin = lower_plus, xmin = loc- 0.2, xmax = loc + 0.2), col = 'grey10', fill = 'grey70') +
  geom_rect(aes(ymax = upper, ymin = lower, xmin = loc- 0.2, xmax = loc + 0.2), col = 'grey10') +
  geom_point(data = f_lower, aes(x = as.numeric(as.factor(location)), y = value), col = 'grey30') +
  geom_point(data = f_upper, aes(x = as.numeric(as.factor(location)), y = value), col = 'grey30') +
  scale_fill_manual(breaks = c('observed', 'deg_1', 'deg_2', 'deg_3'), values = c('grey50', 'palegreen4', 'steelblue', 'salmon')) +
  scale_x_continuous(breaks = 1:6, labels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Meknes', 'Sfax')) +
  facet_grid(flowering_type~species)


#compare estimated time windows

window_df <- thermal_window %>% 
  mutate(flowering_type = recode(flowering_type, f5 = 'begin_flowering_f5', f50 = 'flowering_f50'),
         lower =  min_doy_padded,
         upper = max_doy_padded,
         method = 'frost / heat risk',
         species = tolower(species)) %>% 
  dplyr::select(location, species, flowering_type, method, lower, upper)

window_df <- transfer_ranges_df %>% 
  mutate(method = 'transfer empirical windows',
         species = tolower(species)) %>% 
  dplyr::select(location, species, flowering_type, method, lower, upper) %>% 
  rbind(window_df)



write.csv(window_df, 'data/bloom_timewindow_current_conditions.csv', row.names = FALSE)



window_df %>% 
  mutate(loc = as.numeric(factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Meknes', 'Sfax')))) %>% 
  ggplot(aes(x = loc, fill = method)) +
  geom_rect(aes(ymin = lower, ymax = upper, xmin = loc- 0.2, xmax = loc + 0.2), position = position_dodge()) +
  scale_x_continuous(breaks = 1:5, labels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Meknes', 'Sfax')) +
  facet_grid(flowering_type~species) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('figures/example_timewindow.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')





#-----------------------------#
#visualize type 1 and type2 errors of both methods
#------------------------------#

err1_df <- window_df %>% 
  merge(master_pheno, by = c('species', 'flowering_type', 'location')) %>% 
  mutate(failure = pred < lower | pred > upper) %>% 
  group_by(species, cultivar, location, method) %>% 
  summarise(failure_rate = sum(failure) / n())

err1_plot_methods <- err1_df %>% 
  mutate(failure_rate = failure_rate * 100,
         species_label = recode(species, `european plum` = "europ. plum", `japanese plum` = "jap. plum"),
         species_label = str_to_title(species_label)) %>% 
  ggplot(aes(x = species_label, y = failure_rate, fill = method)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,112))+
  facet_grid(~location, scales = 'free_x', space = 'free_x') +
  ylab('Type 1 Error (%)') +
  xlab('') +
  scale_color_manual(values = okabe) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'none')



err2_df <- window_df %>% 
  merge(pheno_current, by = c('species', 'flowering_type', 'location')) %>% 
  mutate(failure = pred < lower | pred > upper) %>% 
  group_by(species, cultivar, location, method) %>% 
  summarise(failure_rate = sum(failure) / n()) %>% 
  filter((species %in% c('apricot', 'apple', 'european plum', 'japanese plum', 'pear', 'sweet cherry') & location == 'Sfax') |
           (species %in% c('almond', 'pistachio') & location %in% c('Klein-Altendorf', 'Zaragoza'))) 

err2_plot <- err2_df %>% 
  mutate(failure_rate = failure_rate * 100,
         species_label = recode(species, `european plum` = "europ. plum", `japanese plum` = "jap. plum"),
         species_label = str_to_title(species_label)) %>% 
  ggplot(aes(x = species_label, y = failure_rate, fill = method)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,112))+
  facet_grid(~location, scales = 'free_x', space = 'free_x') +
  ylab('Type 1 Error (%)') +
  xlab('') +
  scale_color_manual(values = okabe) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'none')

#the plots still need fine tuning
#colors need to be different
#labelling
#also look up how to change the paddings of the plot to make the patchowrk more tight




type2 <- fail_su_current %>% 
  rbind(fail_ther_current) %>% 
  mutate(species = stringr::str_to_title(species),
         location_label = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
                                 Zaragoza = 'Zarag.')) %>% 
  filter((species %in% c('Apricot', 'Apple', 'European Plum', 'Japanese Plum', 'Pear', 'Sweet Cherry') & location == 'Sfax') |
           (species == 'Almond' & location %in% c('Klein-Altendorf', 'Zaragoza'))) %>% 
  mutate(type_two_error = 1 - failure_rate,
         location_label = factor(location_label, levels = c('Kl.-Alt.', 'Zarag.', 'Sfax'))) %>% 
  ggplot(aes(y = type_two_error, x = species, fill = window_type)) +
  geom_boxplot() +
  facet_grid(~location_label, scales = 'free_x', space = 'free_x') +
  theme_bw(base_size = 15) +
  xlab('') +
  ylab('Type II error') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  scale_fill_manual(values = c("steelblue", "#E69F00"), breaks = c('thermal', 'sudoku'), labels = c('Thermal Risk', 'Transfer Ranges'))+
  #theme(legend.title = 'Method to construct\ntime window') +
  guides(fill=guide_legend(title="Flowering Window Method"))

type1 <- fail_su_current %>% 
  rbind(fail_ther_current) %>% 
  mutate(species = stringr::str_to_title(species),
         location_label = recode(location, `Klein-Altendorf` = 'Kl.-Alt.',
                                 Zaragoza = 'Zarag.'),
         location = factor(location, levels = c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Meknes', 'Sfax'))) %>% 
  filter((location == 'Klein-Altendorf' & species %in% c('Apple', 'European Plum', 'Japanese Plum', 'Pear', 'Sweet Cherry')) |
           (location == 'Cieza' & species %in% c('Apricot')) |
           (location == 'Zaragoza' & species %in% c('Apricot', 'Sweet Cherry', 'Pear')) |
           (location == 'Meknes' & species %in% c('Almond', 'Apple')) |
           (location == 'Sfax' & species %in% c('Almond', 'Pistachio'))) %>% 
  ggplot(aes(y = failure_rate, x = species, fill = window_type)) +
  geom_boxplot() +
  facet_grid(~location, scales = 'free_x', space = 'free_x') +
  theme_bw(base_size = 15) +
  xlab('') +
  ylab('Type I error') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
  scale_fill_manual(values = c("steelblue", "#E69F00"), breaks = c('thermal', 'sudoku'), labels = c('Thermal Risk', 'Transfer Ranges')) +
  #theme(legend.title = 'Method to construct\ntime window') +
  guides(fill=guide_legend(title="Flowering Window Method"))














#------------------------------------#
# Phenology for observed weather #####
#------------------------------------#

pheno_current <-  read.csv('data/projected_bloomdates_ensemble_observed_weather.csv') %>% 
  mutate(species = tolower(species),
         flowering_type = ifelse(species %in% c('apple', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50')) 


timewindow_df <- read.csv('data/bloom_timewindow_current_conditions.csv')


pheno_current %>% 
  merge.data.frame(timewindow_df, by = c('species', 'location', 'flowering_type')) %>% 
  filter(method == 'frost / heat risk') %>% 
  mutate(failure = pred < lower | pred > upper) %>% 
  group_by(location, species, cultivar) %>% 
  summarise(failure_rate = sum(failure) / n()) %>% 
  ggplot(aes(x = species, y = failure_rate, fill = location)) +
  geom_boxplot()

head(pheno_current)
#--> it can't be that so much of it is already now classified as failure

#--> plot thermal window with predicted flowering dates
pheno_current %>% 
  merge.data.frame(timewindow_df, by = c('species', 'location', 'flowering_type')) %>% 
  filter(method == 'frost / heat risk') %>% 
  ggplot(aes(x = pred, y = cultivar)) +
  geom_rect(aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf,
                fill = 'Thermal risks')) +
  geom_point(alpha = 0.2) +
  scale_fill_manual(values = 'lightsalmon') +
  facet_grid(species~location, scales = 'free_y', space = 'free_y') +
  theme_bw(base_size = 15) +
  guides(fill=guide_legend(title="Method to construct\ntime window")) +
  coord_cartesian(xlim = c(0, 365))
ggsave(filename = 'figures/timewindow_thermal_current.jpeg',
       height = 40, width = 30, units = 'cm', device = 'jpeg')


#now the sudoku time window
pheno_current %>% 
  merge.data.frame(timewindow_df, by = c('species', 'location', 'flowering_type')) %>% 
  filter(method == 'transfer empirical windows') %>% 
  ggplot(aes(x = pred, y = cultivar)) +
  geom_rect(aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf,
                fill = '"Sudoku"')) +
  geom_point(alpha = 0.2) +
  scale_fill_manual(values = 'lightsteelblue') +
  facet_grid(species~location, scales = 'free_y', space = 'free_y') +

  theme_bw(base_size = 15) +
  #theme(legend.title = 'Method to construct\ntime window') +
  guides(fill=guide_legend(title="Method to construct\ntime window")) +
  coord_cartesian(xlim = c(0, 365))
ggsave(filename = 'figures/timewindow_sudoku_current.jpeg',
       height = 40, width = 30, units = 'cm', device = 'jpeg')


pheno_current %>% 
  merge.data.frame(timewindow_df, by = c('species', 'location', 'flowering_type')) %>% 
  mutate(failure = pred < lower | pred > upper) %>% 
  group_by(location, species, cultivar, method) %>% 
  summarise(failure_rate = sum(failure) / n()) %>% 
  ggplot(aes(x = failure_rate, y = cultivar, fill = method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species~location, scales = 'free_y', space = 'free_y') +
  
  theme_bw(base_size = 15) +
  scale_fill_manual(values = c('salmon', 'steelblue'), breaks = c('frost / heat risk', 'transfer empirical windows'), labels = c('Thermal risks', '"Sudoku"'))+
  #theme(legend.title = 'Method to construct\ntime window') +
  guides(fill=guide_legend(title="Method to construct\ntime window"))

ggsave(filename = 'figures/timewindow_failurerate_current.jpeg',
       height = 40, width = 30, units = 'cm', device = 'jpeg')


#--------------------------------------#
# Phenolgy for Historic simulations ####
#--------------------------------------#

pheon_hist <- read.csv('data/projected_bloomdates_ensemble_historic_scenarios.csv')  %>% 
  mutate(species = tolower(species)) 

#read historic simulated weather data

hist_weather <- chillR::load_temperature_scenarios('data/hist-sim-weather/', prefix = 'hist_gen') %>% 
  purrr::map(function(x){
    set_colnames(x,c('DATE', 'Year', 'Month', 'Day', 'nodata', 'Tmin', 'Tmax'))
  }) 


names(hist_weather) <- str_split(names(hist_weather), pattern = '_') %>% 
  purrr::map_chr(2)


thermal_window_hist <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
                                                                                                                weather_list_pred = hist_weather, 
                                                                                                                observation_df = observation_df, 
                                                                                                                frost_threshold = 0, 
                                                                                                                heat_threshold = 32, 
                                                                                                                target_col_obs = x)) %>% 
  bind_rows() %>% 
  mutate(species = tolower(species))

timewindow_df



pheon_hist %>% 
  merge.data.frame(thermal_window_hist, by = c('species', 'location', 'flowering_type')) %>% 
  ggplot(aes(x = pheno_predicted, y = cultivar)) +
  geom_rect(aes(xmin = min_doy, xmax = max_doy, ymin = -Inf, ymax = Inf,
                fill = 'Thermal risks')) +
  geom_point(alpha = 0.2) +
  scale_fill_manual(values = 'lightsalmon') +
  facet_grid(species~location, scales = 'free_y', space = 'free_y') +
  theme_bw(base_size = 15) +
  guides(fill=guide_legend(title="Method to construct\ntime window")) +
  coord_cartesian(xlim = c(0, 365))
ggsave(filename = 'figures/timewindow_thermal_hist.jpeg',
       height = 40, width = 30, units = 'cm', device = 'jpeg')



pheon_hist %>% 
  merge.data.frame(sudoku_timewindow, by = c('species', 'location', 'flowering_type')) %>% 
  ggplot(aes(x = pheno_predicted, y = cultivar)) +
  geom_rect(aes(xmin = min_doy, xmax = max_doy, ymin = -Inf, ymax = Inf,
                fill = 'Sudoku')) +
  geom_point(alpha = 0.2) +
  scale_fill_manual(values = 'lightsteelblue') +
  facet_grid(species~location, scales = 'free_y', space = 'free_y') +
  theme_bw(base_size = 15) +
  guides(fill=guide_legend(title="Method to construct\ntime window")) +
  coord_cartesian(xlim = c(0, 365))
ggsave(filename = 'figures/timewindow_sudoku_hist.jpeg',
       height = 40, width = 30, units = 'cm', device = 'jpeg')



#calculate failure rates for two approaches

fail_su_hist <- pheon_hist %>% 
  merge.data.frame(sudoku_timewindow, by = c('species', 'location', 'flowering_type')) %>% 
  group_by(location, species, cultivar) %>% 
  summarise(failure_rate = sum(pheno_predicted < min_doy | pheno_predicted > max_doy) / n(),
            dist_window = min(c(abs(pheno_predicted - min_doy), abs(pheno_predicted - max_doy))),
            too_early = sum(pheno_predicted < min_doy) / n(),
            too_late = sum(pheno_predicted > max_doy) / n(),
            window_type = 'sudoku')

fail_ther_hist <- pheon_hist %>% 
  merge.data.frame(thermal_window_hist, by = c('species', 'location', 'flowering_type')) %>% 
  group_by(location, species, cultivar) %>% 
  summarise(failure_rate = sum(pheno_predicted < min_doy | pheno_predicted > max_doy) / n(),
            dist_window = min(c(abs(pheno_predicted - min_doy), abs(pheno_predicted - max_doy))),
            too_early = sum(pheno_predicted < min_doy) / n(),
            too_late = sum(pheno_predicted > max_doy) / n(),
            window_type = 'thermal')

fail_su_hist %>% 
  rbind(fail_ther_hist) %>% 
  ggplot(aes(x = failure_rate, y = cultivar, fill = window_type)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(species~location, scales = 'free_y', space = 'free_y') +
  
  theme_bw(base_size = 15) +
  scale_fill_manual(values = c('salmon', 'steelblue'), breaks = c('thermal', 'sudoku'), labels = c('Thermal risks', '"Sudoku"'))+
  #theme(legend.title = 'Method to construct\ntime window') +
  guides(fill=guide_legend(title="Method to construct\ntime window"))
ggsave(filename = 'figures/timewindow_failurerate_hist.jpeg',
       height = 40, width = 30, units = 'cm', device = 'jpeg')



#--------------------#
#Future phenology ####
#--------------------#

pheno_future <- read.csv('data/projected_bloomdates_ensemble.csv')

future_weather <- chillR::load_temperature_scenarios('data/future_weather/', prefix = '') 

#organize the future weather by time and ssp and have a final list with the three locations


loc <- str_split(names(future_weather), '_') %>% 
  purrr::map_chr(1)

ssp <- str_split(names(future_weather), '_') %>% 
  purrr::map_chr(2) %>% 
  str_split('\\.') %>% 
  purrr::map_chr(1)

model <- str_split(names(future_weather), '_') %>% 
  purrr::map_chr(2) %>% 
  str_split('\\.') %>% 
  purrr::map_chr(2)

time <- str_split(names(future_weather), '_') %>% 
  purrr::map_chr(2) %>% 
  str_split('\\.') %>% 
  purrr::map_chr(3)


fut_weather_list <- list()

for(scen in unique(ssp)){
  
  fut_weather_list[[scen]] <- list()
  
  #orginize it by year
  for(yr in unique(time)){
    
    fut_weather_list[[scen]][[yr]] <- list()
    
    for(lc in unique(loc)){
      
      k <- which(scen == ssp & yr == time & lc == loc)
      
      fut_weather_list[[scen]][[yr]][[lc]] <- future_weather[k] %>% 
        bind_rows(.id = 'id')
      
    }
    
  }
}



pheon_hist

for(scen in names(fut_weather_list)){
  
  for(yr in names(fut_weather_list[[scen]])){
    
    
    thermal_window_fut <- purrr::map(c('begin_flowering_f5', 'flowering_f50'), function(x) get_thermal_window_phenology(weather_list_obs = weather_list_obs, 
                                                                                                                         weather_list_pred = fut_weather_list[[scen]][[yr]], 
                                                                                                                         observation_df = observation_df, 
                                                                                                                         frost_threshold = 0, 
                                                                                                                         heat_threshold = 32, 
                                                                                                                         target_col_obs = x)) %>% 
      bind_rows() %>% 
      mutate(species = tolower(species))
    
    head(pheno_future)
    
    base::split
    
    pheno_future %>% 
      filter(scenario_year == yr, ssp == scen) %>% 
      tidyr::separate(species_cultivar, into = c('species', 'cultivar'), sep = '_') %>% 
      mutate(species = tolower(species)) %>% 
      mutate(flowering_type = ifelse(species %in% c('almond', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50')) %>% 
      merge.data.frame(thermal_window_fut, by = c('species', 'location', 'flowering_type')) %>% 
      ggplot(aes(x = pheno_predicted, y = cultivar)) +
      geom_rect(aes(xmin = min_doy, xmax = max_doy, ymin = -Inf, ymax = Inf,
                    fill = 'Thermal risks')) +
      geom_point(alpha = 0.2) +
      scale_fill_manual(values = 'lightsalmon') +
      facet_grid(species~location, scales = 'free_y', space = 'free_y') +
      theme_bw(base_size = 15) +
      guides(fill=guide_legend(title=paste0("Method to construct\ntime window\n\n", scen, ' ', yr))) +
      coord_cartesian(xlim = c(0, 365))
    ggsave(filename = paste0('figures/timewindow_thermal_future',scen,'_',yr,'.jpeg'),
           height = 40, width = 30, units = 'cm', device = 'jpeg')
    
    
    
    pheno_future %>% 
      filter(scenario_year == yr, ssp == scen) %>% 
      tidyr::separate(species_cultivar, into = c('species', 'cultivar'), sep = '_') %>% 
      mutate(species = tolower(species)) %>% 
      mutate(flowering_type = ifelse(species %in% c('almond', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50')) %>% 
      merge.data.frame(sudoku_timewindow, by = c('species', 'location', 'flowering_type')) %>% 
      ggplot(aes(x = pheno_predicted, y = cultivar)) +
      geom_rect(aes(xmin = min_doy, xmax = max_doy, ymin = -Inf, ymax = Inf,
                    fill = 'Sudoku')) +
      geom_point(alpha = 0.2) +
      scale_fill_manual(values = 'lightsteelblue') +
      facet_grid(species~location, scales = 'free_y', space = 'free_y') +
      theme_bw(base_size = 15) +
      guides(fill=guide_legend(title=paste0("Method to construct\ntime window\n\n", scen, ' ', yr))) +
      coord_cartesian(xlim = c(0, 365))
    ggsave(filename = paste0('figures/timewindow_sudoku_future',scen,'_',yr,'.jpeg'),
           height = 40, width = 30, units = 'cm', device = 'jpeg')
    
    
    
    fail_su_hist <-     pheno_future %>% 
      filter(scenario_year == yr, ssp == scen) %>% 
      tidyr::separate(species_cultivar, into = c('species', 'cultivar'), sep = '_') %>% 
      mutate(species = tolower(species)) %>% 
      mutate(flowering_type = ifelse(species %in% c('almond', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50')) %>% 
      merge.data.frame(sudoku_timewindow, by = c('species', 'location', 'flowering_type')) %>% 
      group_by(location, species, cultivar) %>% 
      summarise(failure_rate = sum(pheno_predicted < min_doy | pheno_predicted > max_doy) / n(),
                dist_window = min(c(abs(pheno_predicted - min_doy), abs(pheno_predicted - max_doy))),
                too_early = sum(pheno_predicted < min_doy) / n(),
                too_late = sum(pheno_predicted > max_doy) / n(),
                window_type = 'sudoku')
    
    fail_ther_hist <-     pheno_future %>% 
      filter(scenario_year == yr, ssp == scen) %>% 
      tidyr::separate(species_cultivar, into = c('species', 'cultivar'), sep = '_') %>% 
      mutate(species = tolower(species)) %>% 
      mutate(flowering_type = ifelse(species %in% c('almond', 'european plum', 'japanese plum'), yes = 'begin_flowering_f5', no = 'flowering_f50')) %>% 
      merge.data.frame(thermal_window_fut, by = c('species', 'location', 'flowering_type')) %>% 
      group_by(location, species, cultivar) %>% 
      summarise(failure_rate = sum(pheno_predicted < min_doy | pheno_predicted > max_doy) / n(),
                dist_window = min(c(abs(pheno_predicted - min_doy), abs(pheno_predicted - max_doy))),
                too_early = sum(pheno_predicted < min_doy) / n(),
                too_late = sum(pheno_predicted > max_doy) / n(),
                window_type = 'thermal')
    
    fail_su_hist %>% 
      rbind(fail_ther_hist) %>% 
      ggplot(aes(x = failure_rate, y = cultivar, fill = window_type)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      facet_grid(species~location, scales = 'free_y', space = 'free_y') +
      
      theme_bw(base_size = 15) +
      scale_fill_manual(values = c('salmon', 'steelblue'), breaks = c('thermal', 'sudoku'), labels = c('Thermal risks', '"Sudoku"'))+
      #theme(legend.title = 'Method to construct\ntime window') +
      guides(fill=guide_legend(title=paste0("Method to construct\ntime window\n\n", scen, ' ', yr)))
    ggsave(filename = paste0('figures/timewindow_failure_future',scen,'_',yr,'.jpeg'),
           height = 40, width = 30, units = 'cm', device = 'jpeg')
    
    
    
    
  }
}


# 
# names(hist_weather) <- str_split(names(hist_weather), pattern = '_') %>% 
#   purrr::map_chr(2)
# 
# 
# pheno_current
# 
# 
# 
# 
# plot(runn_mean(cka_season[[3]]$Temp, runn_mean = 1), type = 'line')
# plot(runn_mean(cka_season[[3]]$Temp, runn_mean = 24 * 7), type = 'line')
# plot(runn_mean(sfax_season[[3]]$Temp, runn_mean = 24 * 7 ), type = 'line')
# plot(runn_mean(meknes_season[[3]]$Temp, runn_mean = 24 * 7 ))
# plot(runn_mean(zaragoza_season[[3]]$Temp, runn_mean = 24 * 7 ))
# plot(runn_mean(cieza_season[[3]]$Temp, runn_mean = 24 * 7 ))
# 
# 
# #get temperature around flowering (+- one week)
# 
# adamedor %>% 
#   filter(species == 'Japanese plum')
# 
# days_before <- 60
# 
# cka_season[["2021"]]
# 
# #match by Jday 12:00 noon
# 
# day_flowering <- as.Date('2010-04-18')
# day_flowering <- as.Date('1973-05-18')
# 
# year <- as.character(lubridate::year(day_flowering))
# 
# #find position of flowering in the seasonList
# i <- which(cka_season[[year]]$JDay == lubridate::yday(day_flowering))[12]
# i_start <- i - (days_before * 24)
# 
# chillR::pheno
# 
# plot(cka_season[[year]]$Temp[i_start:i])
# #check all seasons in which a frost was reported for klein-altendorf
# adamedor %>% 
#   filter(comment)
# 
# frost_row <- grep(pattern = 'frost',  tolower(adamedor$comment))
# 
# adamedor$location[frost_row]
# #--> only in cka
# 
# unique(adamedor$species[frost_row])
# #--> only in apple and pear
# 
# unique(adamedor$cultivar[frost_row])
# #happened in several cultivars
# 
# sort(unique(adamedor$year[frost_row]))
# #years with frost risk
# 
# #in 1969 only roter boskop and berlepsch had frost damage
# 
# frost_date <- adamedor %>% 
#   filter(location == 'Klein-Altendorf',
#          species == 'Apple',
#          cultivar %in% c('Roter Boskoop', 'Cox Orange'),
#          year == 1968) %>% 
#   pull(flowering_f50) %>% 
#   as.Date()
# 
# no_frost_date <- adamedor %>% 
#   filter(location == 'Klein-Altendorf',
#          species == 'Apple',
#          !(cultivar %in% c('Roter Boskoop', 'Cox Orange')),
#          year == 1968) %>% 
#   pull(flowering_f50) %>% 
#   as.Date()
# 
# 
# #find position of flowering in the seasonList
# i_f <- purrr::map_int(no_frost_date, function(x) which(cka_season[['1968']]$JDay ==  lubridate::yday(x))[12])
# i_nf <- purrr::map_int(frost_date, function(x) which(cka_season[['1968']]$JDay ==  lubridate::yday(x))[12])
# 
# s_plot <- min(c(i_f, i_nf), na.rm = TRUE) - (60 * 24)
# e_plot <- max(c(i_f, i_nf), na.rm = TRUE) + (7 * 24)
# plot(y =  cka_season[['1968']]$Temp[s_plot:e_plot], x = s_plot:e_plot, type = 'line') 
# abline(v = i_f, col = 'blue', lwd = 3, lty = 2)
# abline(v = i_nf, col = 'red', lwd = 3, lty = 2)
# 
# 
# 
# 
# no_frost_date <- adamedor %>% 
#   filter(location == 'Klein-Altendorf',
#          species == 'Apple',
#          year == 1969) %>% 
#   pull(flowering_f50) %>% 
#   as.Date()
# 
# 
# #find position of flowering in the seasonList
# i_nf <- purrr::map_int(no_frost_date, function(x) which(cka_season[['1969']]$JDay ==  lubridate::yday(x))[12])
# i_f <- NA 
# 
# s_plot <- min(c(i_f, i_nf), na.rm = TRUE) - (60 * 24)
# e_plot <- max(c(i_f, i_nf), na.rm = TRUE) + (7 * 24)
# plot(y =  cka_season[['1969']]$Temp[s_plot:e_plot], x = s_plot:e_plot, type = 'line') 
# abline(v = i_nf, col = 'red', lwd = 3, lty = 2)
# 
# i_last_frost <- which.max(cka_season[['1969']]$Temp[s_plot:e_plot] <= 0) + s_plot
# #distance to last frost
# (i_nf - i_last_frost) / 24 
# #last frost happened around 2 months ago
# 
# 
# #get distance of flowering to last frost day in days
# 
# adamedor_sub <- adamedor %>% 
#   filter(location %in% c('Cieza', 'Zaragoza', 'Meknes', 'Klein-Altendorf', 'Sfax'),
#          species %in% c('Almond', 'Apricot', 'Japanese plum', 'European plum', 'Pistachio', 'Sweet Cherry', 'Pear', 'Apple'))
# 
# #for each phenology f5 or f50 calculate the distance to last frost event
# adamedor_sub$d_frost_f5 <- NA
# 
# for(i in 1:nrow(adamedor_sub)){
#   
#   d_flower <- adamedor_sub$begin_flowering_f5[i] %>% 
#     as.Date()
#   
#   if(is.na(d_flower)){
#     next()
#   }
#   
#   loc <- adamedor_sub$location[i]
#   
#   yr <- adamedor_sub$year[i] %>% 
#     as.character()
#   
#   
#   #get 2 month period before flowering
#   
#   i_flower <- which(SeasonList[[loc]][[yr]]$JDay == lubridate::yday(d_flower))[12]
#   i_s <- (i_flower - (24 * 60))
#   i_last_frost <- which.max(SeasonList[[loc]][[yr]]$Temp[i_s:i_flower] <= -0.1) + i_s
#   adamedor_sub$d_frost_f5[i] <- (i_flower - i_last_frost) / 24
# }
# 
# 
# adamedor_sub$d_frost_f50 <- NA
# 
# for(i in 1:nrow(adamedor_sub)){
#   
#   d_flower <- adamedor_sub$flowering_f50[i] %>% 
#     as.Date()
#   
#   if(is.na(d_flower)){
#     next()
#   }
#   
#   loc <- adamedor_sub$location[i]
#   
#   yr <- adamedor_sub$year[i] %>% 
#     as.character()
#   
#   if(!(yr %in% names(SeasonList[[loc]]))){
#     next()
#   }
#   
#   
#   #get 2 month period before flowering
#   
#   i_flower <- which(SeasonList[[loc]][[yr]]$JDay == lubridate::yday(d_flower))[12]
#   i_s <- (i_flower - (24 * 60))
#   i_last_frost <- which.max(SeasonList[[loc]][[yr]]$Temp[i_s:i_flower] <= -0.1) + i_s
#   adamedor_sub$d_frost_f50[i] <- (i_flower - i_last_frost) / 24
# }
# 
# 
# #mark columns with frost event
# adamedor_sub$frost_damage_reported <- FALSE
# adamedor_sub$frost_damage_reported[grep('frost', tolower(adamedor_sub$comment))] <- TRUE
# 
# adamedor_sub %>% 
#   ggplot(aes(x = location, y = d_frost_f5, fill = frost_damage_reported)) +
#   geom_boxplot() +
#   facet_grid(~species)
# 
# adamedor_sub %>% 
#   ggplot(aes(x = location, y = d_frost_f50, fill = frost_damage_reported)) +
#   geom_boxplot() +
#   facet_grid(~species)
# 
# #distance to frost event is not a good indicator
# #maybe calculate the number of frost hours 
# 
# adamedor_sub$h_frost_f5 <- NA
# 
# for(i in 1:nrow(adamedor_sub)){
#   
#   d_flower <- adamedor_sub$begin_flowering_f5[i] %>% 
#     as.Date()
#   
#   if(is.na(d_flower)){
#     next()
#   }
#   
#   loc <- adamedor_sub$location[i]
#   
#   yr <- adamedor_sub$year[i] %>% 
#     as.character()
#   
#   if(!(yr %in% names(SeasonList[[loc]]))){
#     next()
#   }
#   
#   
#   #get 2 month period before flowering
#   
#   i_flower <- which(SeasonList[[loc]][[yr]]$JDay == lubridate::yday(d_flower))[12]
#   i_s <- (i_flower - (24 * 60))
#   adamedor_sub$h_frost_f5[i] <- sum(SeasonList[[loc]][[yr]]$Temp[i_s:i_flower] <= -0.1) 
# }
# 
# adamedor_sub$h_frost_f50 <- NA
# 
# for(i in 1:nrow(adamedor_sub)){
#   
#   d_flower <- adamedor_sub$flowering_f50[i] %>% 
#     as.Date()
#   
#   if(is.na(d_flower)){
#     next()
#   }
#   
#   loc <- adamedor_sub$location[i]
#   
#   yr <- adamedor_sub$year[i] %>% 
#     as.character()
#   
#   if(!(yr %in% names(SeasonList[[loc]]))){
#     next()
#   }
#   
#   
#   #get 2 month period before flowering
#   
#   i_flower <- which(SeasonList[[loc]][[yr]]$JDay == lubridate::yday(d_flower))[12]
#   i_s <- (i_flower - (24 * 60))
#   adamedor_sub$h_frost_f50[i] <- sum(SeasonList[[loc]][[yr]]$Temp[i_s:i_flower] <= -0.1) 
# }
# 
# adamedor_sub %>% 
#   ggplot(aes(x = location, y = h_frost_f5, fill = frost_damage_reported)) +
#   geom_boxplot() +
#   facet_grid(~species)
# 
# adamedor_sub %>% 
#   ggplot(aes(x = location, y = h_frost_f50, fill = frost_damage_reported)) +
#   geom_boxplot() +
#   facet_grid(~species)
# 
# 
# 
# #do the same for apples and mark the ones which were indicated to have frost damage
# 
# 
# 
# #	1983-01-25
# #1980-02-21
# 
# d <- as.Date('1980-02-21')
# yr <- lubridate::year(d) %>% 
#   as.character()
# 
# plot(SeasonList[['Sfax']][[yr]]$Temp)
# i_f <-  which(SeasonList[['Sfax']][[yr]]$JDay == lubridate::yday(d))[12]
# abline(v = i_f)
# 
# #extract the n days before the flowering
# #distance to 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# target_df <- flower_summarized
# 
# 
# 
# f5 <- flower_summarized %>% 
#   dplyr::filter(flowering_type == 'begin_flowering_f5',
#                 location %in% c('Sfax', 'Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf'),
#                 !(species %in% c('Peach', 'Olive'))) %>% 
#   dplyr::select(species, location, mean) %>% 
#   reshape2::dcast(species ~ location, value.var = 'mean')
# 
# f50 <- flower_summarized %>% 
#   dplyr::filter(flowering_type == 'flowering_f50',
#                 location %in% c('Sfax', 'Meknes', 'Cieza', 'Zaragoza', 'Klein-Altendorf'),
#                 !(species %in% c('Peach', 'Olive'))) %>% 
#   dplyr::select(species, location, mean) %>% 
#   reshape2::dcast(species ~ location, value.var = 'mean')
# 
# comb <- 2:ncol(f5) %>% 
#   expand.grid(., .)
# 
# # comb <- colnames(f5)[-1] %>% 
# #   combn(2) %>% 
# #   t()
# 
# f5_first_degree_relationship <- apply(f5, MARGIN = 1, FUN = function(x) as.numeric(x[comb[,2]]) - as.numeric(x[comb[,1]])) %>% 
#   cbind(comb, .) %>% 
#   magrittr::set_colnames(c('from', 'to', f5$species))
# 
# f50_first_degree_relationship <- apply(f50, MARGIN = 1, FUN = function(x) as.numeric(x[comb[,2]]) - as.numeric(x[comb[,1]])) %>% 
#   cbind(comb, .) %>% 
#   magrittr::set_colnames(c('from', 'to', f50$species))
# 
# #if f5 contains data for one location but not for the other and there is a relationsip established
# #--> make a prediction
# 
# f50_first_degree_relationship <- apply(f50_first_degree_relationship, MARGIN = c(1,2), function(x) is.na(x) == FALSE) %>% 
#   rowSums() %>% 
#   cbind.data.frame(f50_first_degree_relationship) %>% 
#   filter(. >= 3,
#          from != to)
# 
# f5_first_degree_relationship <- apply(f5_first_degree_relationship, MARGIN = c(1,2), function(x) is.na(x) == FALSE) %>% 
#   rowSums() %>% 
#   cbind.data.frame(f5_first_degree_relationship) %>% 
#   filter(. >= 3,
#          from != to) 
# 
# first_degree_relationships <- f50_first_degree_relationship %>% 
#   rbind.data.frame(f5_first_degree_relationship)
# 
# 
# f50_obs <- f50 %>% 
#   reshape2::melt(id.vars = 'species', variable.name = 'location') %>% 
#   mutate(source = 'observed',
#          flowering_type = 'f50',
#          donor = 'none') %>% 
#   na.omit()
# 
# f5_obs <- f5 %>% 
#   reshape2::melt(id.vars = 'species', variable.name = 'location') %>% 
#   mutate(source = 'observed',
#          flowering_type = 'f5',
#          donor = 'none') %>% 
#   na.omit()
# 
# 
# 
# 
# f5_miss  <- f5 %>% 
#   reshape2::melt(id.vars = 'species', variable.name = 'location') %>% 
#   mutate(source = 'observed',
#          flowering_type = 'f5') %>% 
#   filter(!complete.cases(value))
# 
# f50_miss  <- f50 %>% 
#   reshape2::melt(id.vars = 'species', variable.name = 'location') %>% 
#   mutate(source = 'observed',
#          flowering_type = 'f50') %>% 
#   filter(!complete.cases(value))
# 
# #transform first degree relationships to an easier format
# first_degree_relationships <- first_degree_relationships %>% 
#   dplyr::select(-.) %>% 
#   reshape2::melt(id.vars = c('from', 'to')) %>% 
#   na.omit() %>% 
#   mutate(from = colnames(f5)[from],
#          to = colnames(f5)[to])
# 
# 
# #go through missing data, if there is something, make estimation
# for(i in 1:nrow(f5_miss)){
#   
#   to_fix <- f5_miss[i,]
#   
#   sub <- first_degree_relationships %>% 
#     filter(to == to_fix$location)
#   
#   if(nrow(sub) == 0){
#     next()
#   }
#   
#   for(j in 1:nrow(sub)){
#     
#     val_og <-   f5_obs %>% 
#       filter(source == 'observed',
#              species == f5_miss$species[i],
#              location == sub$from[j]) %>% 
#       pull(value)
#     
#     if(is_empty(val_og)){
#       next()
#     }
#     
#     
#     
#     to_fix$value <- val_og + sub$value[j]
#     to_fix$source <- 'deg_1'
#     to_fix$donor <- sub$from[j]
#     
#     f5_obs <- rbind.data.frame(f5_obs,
#                                to_fix)
#   }
#   
# }
# 
# 
# for(i in 1:nrow(f50_miss)){
#   
#   to_fix <- f50_miss[i,]
#   
#   sub <- first_degree_relationships %>% 
#     filter(to == to_fix$location)
#   
#   if(nrow(sub) == 0){
#     next()
#   }
#   
#   for(j in 1:nrow(sub)){
#     
#     val_og <-   f50_obs %>% 
#       filter(source == 'observed',
#              species == f50_miss$species[i],
#              location == sub$from[j]) %>% 
#       pull(value)
#     
#     if(is_empty(val_og)){
#       next()
#     }
#     
#     to_fix$value <- val_og + sub$value[j]
#     to_fix$source <- 'deg_1'
#     to_fix$donor <- sub$from[j]
#     
#     f50_obs <- rbind.data.frame(f50_obs,
#                                 to_fix)
#   }
#   
# }
# 
# #kick out the newly introduced values from the miss_list
# #also need to do the same thing for sd
# #definetely need a function later on
# i_f5 <- rep(TRUE, nrow(f5_miss))
# i_f50 <- rep(TRUE, nrow(f50_miss))
# for(i in 1:nrow(f5_miss)){
#   
#   if(paste0(f5_miss$species[i], f5_miss$location[i]) %in% paste0(f5_obs$species, f5_obs$location)){
#     i_f5[i] <- FALSE
#   }
#   
#   if(paste0(f50_miss$species[i], f50_miss$location[i]) %in% paste0(f50_obs$species, f50_obs$location)){
#     i_f50[i] <- FALSE
#   }
# }
# f5_miss <- f5_miss[i_f5,]
# f50_miss <- f50_miss[i_f50,]
# 
# 
# #make combinations of observed and deg1
# k <- max(which(f50_obs$source == 'observed'))
# 
# ind_row <- rbind(expand.grid(1:k, (k+1):nrow(f50_obs)),
#                  expand.grid((k+1):nrow(f50_obs), 1:k))
# 
# 
# #remove pairs for which the species is not equal
# ind_row_filter <- ind_row[f50_obs[ind_row$Var1,'species'] == f50_obs[ind_row$Var2,'species'],]
# 
# #remove pairs for which the source is equal to one donor
# ind_row_filter <- ind_row_filter[f50_obs[ind_row_filter$Var1,'donor'] != f50_obs[ind_row_filter$Var2,'location'],]
# ind_row_filter <- ind_row_filter[f50_obs[ind_row_filter$Var1,'location'] != f50_obs[ind_row_filter$Var2,'donor'],]
# 
# #remove selve comparisons
# ind_row_filter <- ind_row_filter[f50_obs[ind_row_filter$Var1,'location'] != f50_obs[ind_row_filter$Var2,'location'],]
# 
# #establish second degree relationship
# second_degree_relationship_f50 <- data.frame(
#   'from' = f50_obs[ind_row_filter$Var1, 'location'],
#   'to' = f50_obs[ind_row_filter$Var2, 'location'],
#   'species' = f50_obs[ind_row_filter$Var2, 'species'],
#   'shift' =   f50_obs[ind_row_filter$Var2, 'value'] - f50_obs[ind_row_filter$Var1, 'value']
# )
# 
# k <- max(which(f5_obs$source == 'observed'))
# ind_row <- rbind(expand.grid(1:k, (k+1):nrow(f5_obs)),
#                  expand.grid((k+1):nrow(f5_obs), 1:k))
# 
# 
# #remove pairs for which the species is not equal
# ind_row_filter <- ind_row[f5_obs[ind_row$Var1,'species'] == f5_obs[ind_row$Var2,'species'],]
# 
# #remove pairs for which the source is equal to one donor
# ind_row_filter <- ind_row_filter[f5_obs[ind_row_filter$Var1,'donor'] != f5_obs[ind_row_filter$Var2,'location'],]
# ind_row_filter <- ind_row_filter[f5_obs[ind_row_filter$Var1,'location'] != f5_obs[ind_row_filter$Var2,'donor'],]
# 
# #remove selve comparisons
# ind_row_filter <- ind_row_filter[f5_obs[ind_row_filter$Var1,'location'] != f5_obs[ind_row_filter$Var2,'location'],]
# 
# 
# #establish second degree relationship
# second_degree_relationship_f5 <- data.frame('from' = f5_obs[ind_row_filter$Var1, 'location'],
#                                             'to' = f5_obs[ind_row_filter$Var2, 'location'],
#                                             'species' = f5_obs[ind_row_filter$Var2, 'species'],
#                                             'shift' =   f5_obs[ind_row_filter$Var2, 'value'] - f5_obs[ind_row_filter$Var1, 'value']
# )
# 
# 
# second_degree_relationship <- rbind.data.frame(second_degree_relationship_f5, 
#                                                second_degree_relationship_f50)
# 
# #remove second degree relationships, for which a first degree was already established
# second_degree_relationship <- second_degree_relationship[!(paste0(second_degree_relationship$from, second_degree_relationship$to) %in%
#                                                              paste0(first_degree_relationships$from, first_degree_relationships$to)),]
# 
# #there are duplicates in the second degree relationship
# second_degree_relationship <- second_degree_relationship[!duplicated(second_degree_relationship),]
# 
# #remove repeated shifs,they come from the same source and do not offer new information
# second_degree_relationship <- second_degree_relationship %>% 
#   group_by(from, to) %>% 
#   summarise(shift = unique(round(shift, 2))) #need to round, otherwise comparison fails
# 
# #now go through the missing values, make estimates
# 
# for(i in 1:nrow(f50_miss)){
#   
#   to_fix <- f50_miss[i,]
#   
#   # loci <- which(to_fix$location == colnames(f50))
#   
#   sub <- second_degree_relationship %>% 
#     filter(to == to_fix$location)
#   
#   for(j in 1:nrow(sub)){
#     
#     val_og <-   f50_obs %>% 
#       filter(source == 'observed',
#              species == f50_miss$species[i],
#              location == sub$from[j]) %>% 
#       pull(value)
#     
#     if(is_empty(val_og)){
#       next()
#     }
#     
#     
#     
#     to_fix$value <- val_og + sub$shift[j]
#     to_fix$source <- 'deg_2'
#     to_fix$donor <- sub$from[j]
#     
#     f50_obs <- rbind.data.frame(f50_obs,
#                                 to_fix)
#   }
#   
# }
# 
# 
# for(i in 1:nrow(f5_miss)){
#   
#   to_fix <- f5_miss[i,]
#   
#   # loci <- which(to_fix$location == colnames(f50))
#   
#   sub <- second_degree_relationship %>% 
#     filter(to == to_fix$location)
#   
#   for(j in 1:nrow(sub)){
#     
#     val_og <-   f5_obs %>% 
#       filter(source == 'observed',
#              species == f5_miss$species[i],
#              location == sub$from[j]) %>% 
#       pull(value)
#     
#     if(is_empty(val_og)){
#       next()
#     }
#     
#     
#     
#     to_fix$value <- val_og + sub$shift[j]
#     to_fix$source <- 'deg_2'
#     to_fix$donor <- sub$from[j]
#     
#     f5_obs <- rbind.data.frame(f5_obs,
#                                to_fix)
#   }
#   
# }
# 
# #check how many are still missing
# #kick out the newly introduced values from the miss_list
# #also need to do the same thing for sd
# #definetely need a function later on
# i_f5 <- rep(TRUE, nrow(f5_miss))
# i_f50 <- rep(TRUE, nrow(f50_miss))
# for(i in 1:nrow(f5_miss)){
#   
#   if(paste0(f5_miss$species[i], f5_miss$location[i]) %in% paste0(f5_obs$species, f5_obs$location)){
#     i_f5[i] <- FALSE
#   }
#   
#   if(paste0(f50_miss$species[i], f50_miss$location[i]) %in% paste0(f50_obs$species, f50_obs$location)){
#     i_f50[i] <- FALSE
#   }
# }
# f5_miss <- f5_miss[i_f5,]
# f50_miss <- f50_miss[i_f50,]
# 
# 
# 
# #third degree relationship
# #between first degree relationship obtained information
# 
# #make combinations of observed and deg1
# l <- min(which(f50_obs$source == 'deg_1'))
# k <- max(which(f50_obs$source == 'deg_1'))
# 
# ind_row <- rbind(expand.grid(l:k, l:k))
# 
# #remove pairs for which the species is not equal
# ind_row_filter <- ind_row[f50_obs[ind_row$Var1,'species'] == f50_obs[ind_row$Var2,'species'],]
# 
# #remove pairs for which the source is equal to one donor
# ind_row_filter <- ind_row_filter[f50_obs[ind_row_filter$Var1,'donor'] != f50_obs[ind_row_filter$Var2,'location'],]
# ind_row_filter <- ind_row_filter[f50_obs[ind_row_filter$Var1,'location'] != f50_obs[ind_row_filter$Var2,'donor'],]
# 
# #remove selve comparisons
# ind_row_filter <- ind_row_filter[f50_obs[ind_row_filter$Var1,'location'] != f50_obs[ind_row_filter$Var2,'location'],]
# 
# #establish second degree relationship
# third_degree_relationship_f50 <- data.frame(
#   'from' = f50_obs[ind_row_filter$Var1, 'location'],
#   'to' = f50_obs[ind_row_filter$Var2, 'location'],
#   'species' = f50_obs[ind_row_filter$Var2, 'species'],
#   'shift' =   f50_obs[ind_row_filter$Var2, 'value'] - f50_obs[ind_row_filter$Var1, 'value']
# )
# 
# l <- min(which(f5_obs$source == 'deg_1'))
# k <- max(which(f5_obs$source == 'deg_1'))
# 
# ind_row <- rbind(expand.grid(l:k, l:k))
# 
# 
# #remove pairs for which the species is not equal
# ind_row_filter <- ind_row[f5_obs[ind_row$Var1,'species'] == f5_obs[ind_row$Var2,'species'],]
# 
# #remove pairs for which the source is equal to one donor
# ind_row_filter <- ind_row_filter[f5_obs[ind_row_filter$Var1,'donor'] != f5_obs[ind_row_filter$Var2,'location'],]
# ind_row_filter <- ind_row_filter[f5_obs[ind_row_filter$Var1,'location'] != f5_obs[ind_row_filter$Var2,'donor'],]
# 
# #remove selve comparisons
# ind_row_filter <- ind_row_filter[f5_obs[ind_row_filter$Var1,'location'] != f5_obs[ind_row_filter$Var2,'location'],]
# 
# 
# #establish second degree relationship
# third_degree_relationship_f5 <- data.frame('from' = f5_obs[ind_row_filter$Var1, 'location'],
#                                            'to' = f5_obs[ind_row_filter$Var2, 'location'],
#                                            'species' = f5_obs[ind_row_filter$Var2, 'species'],
#                                            'shift' =   f5_obs[ind_row_filter$Var2, 'value'] - f5_obs[ind_row_filter$Var1, 'value']
# )
# 
# 
# third_degree_relationship <- rbind.data.frame(third_degree_relationship_f5, 
#                                               third_degree_relationship_f50)
# 
# #remove second degree relationships, for which a first degree was already established
# third_degree_relationship <- third_degree_relationship[!(paste0(third_degree_relationship$from, third_degree_relationship$to) %in%
#                                                            c(paste0(first_degree_relationships$from, first_degree_relationships$to),
#                                                              paste0(second_degree_relationship$from, second_degree_relationship$to))),]
# 
# #there are duplicates in the second degree relationship
# third_degree_relationship <- third_degree_relationship[!duplicated(third_degree_relationship),]
# 
# #remove repeated shifs,they come from the same source and do not offer new information
# third_degree_relationship <- third_degree_relationship %>% 
#   group_by(from, to) %>% 
#   summarise(shift = unique(round(shift, 2))) #need to round, otherwise comparison fails
# 
# 
# 
# # f50_obs %>% 
# #   group_by(species, location, flowering_type) %>% 
# 
# #fill missing values with third degree relationship
# for(i in 1:nrow(f50_miss)){
#   
#   to_fix <- f50_miss[i,]
#   
#   # loci <- which(to_fix$location == colnames(f50))
#   
#   sub <- third_degree_relationship %>% 
#     filter(to == to_fix$location)
#   
#   if(nrow(sub) == 0){
#     next()
#   }
#   
#   for(j in 1:nrow(sub)){
#     
#     val_og <-   f50_obs %>% 
#       filter(species == f50_miss$species[i],
#              location == sub$from[j]) %>% 
#       pull(value)
#     
#     if(is_empty(val_og)){
#       next()
#     }
#     
#     
#     
#     to_fix$value <- val_og + sub$shift[j]
#     to_fix$source <- 'deg_3'
#     to_fix$donor <- sub$from[j]
#     
#     f50_obs <- rbind.data.frame(f50_obs,
#                                 to_fix)
#   }
#   
# }
# 
# 
# for(i in 1:nrow(f5_miss)){
#   
#   to_fix <- f5_miss[i,]
#   
#   # loci <- which(to_fix$location == colnames(f50))
#   
#   sub <- third_degree_relationship %>% 
#     filter(to == to_fix$location)
#   
#   if(nrow(sub) == 0){
#     next()
#   }
#   
#   for(j in 1:nrow(sub)){
#     
#     val_og <-   f5_obs %>% 
#       filter(species == f5_miss$species[i],
#              location == sub$from[j]) %>% 
#       pull(value)
#     
#     if(is_empty(val_og)){
#       next()
#     }
#     
#     
#     
#     to_fix$value <- val_og + sub$shift[j]
#     to_fix$source <- 'deg_3'
#     to_fix$donor <- sub$from[j]
#     
#     f5_obs <- rbind.data.frame(f5_obs,
#                                to_fix)
#   }
#   
# }
# 
# #check how many are still missing
# #kick out the newly introduced values from the miss_list
# #also need to do the same thing for sd
# #definetely need a function later on
# i_f5 <- rep(TRUE, nrow(f5_miss))
# i_f50 <- rep(TRUE, nrow(f50_miss))
# for(i in 1:nrow(f5_miss)){
#   
#   if(paste0(f5_miss$species[i], f5_miss$location[i]) %in% paste0(f5_obs$species, f5_obs$location)){
#     i_f5[i] <- FALSE
#   }
#   
#   if(paste0(f50_miss$species[i], f50_miss$location[i]) %in% paste0(f50_obs$species, f50_obs$location)){
#     i_f50[i] <- FALSE
#   }
# }
# f5_miss <- f5_miss[i_f5,]
# f50_miss <- f50_miss[i_f50,]
# 
# #no more missing values (except apricot f5, had no observations for it)
# 
# f50_obs %>% 
#   ggplot(aes(x = location, y = value)) +
#   geom_point() +
#   facet_grid(~species)
# 
# 
# 
# #reomove second degree relationship where from and to are equal
# second_degree_relationship <- second_degree_relationship %>% 
#   filter(from != to)
# 
# #make predictions for values of second degree relationship for missing values
# f50_miss
# 
# #check which combination of species and location is in second degree relationship present
# paste0(f50_miss$species, f50_miss$location) %in% paste0(second_degree_relationship$to,
#                                                         second_degree_relationship$species)
# 
# for(i in 1:nrow(f50_miss)){
#   
#   to_fix <- f50_miss[i,]
#   
#   # loci <- which(to_fix$location == colnames(f50))
#   
#   sub <- second_degree_relationship %>% 
#     filter(to == to_fix$location)
#   
#   
#   
#   #need to check if one of the donors has an original observation for one of the relationships
#   f50_obs %>% 
#     filter(source == 'observed')
#   
#   for(j in 1:nrow(sub)){
#     
#     val_og <-   f50_obs %>% 
#       filter(source == 'observed',
#              species == f50_miss$species[i],
#              location == sub$from[j]) %>% 
#       pull(value)
#     
#     
#     
#     
#     to_fix$value <- val_og + sub$shift[j]
#     to_fix$source <- 'deg_2'
#     to_fix$donor <- sub$from[j]
#     
#     f50_obs <- rbind.data.frame(f50_obs,
#                                 to_fix)
#   }
#   
# }
# 
# 
# 
# 

#now comes the second layer, which is more ambiogous
#deg1 information should not be used to project back things, its a one way (hopefully)





















#---------------------------#
#old code ####
#---------------------------#



# cka_frost_risk <- cka %>% 
#   mutate(doy = lubridate::yday(Date)) %>% 
#   group_by(doy) %>% 
#   summarise(chance_frost = sum(Tmin <= 0) / n()) %>% 
#   mutate(run_mean_frost = chillR::runn_mean(chance_frost, runn_mean = 10),
#          location = 'Klein-Altendorf')
# 
# plot(cka_frost_risk$chance_frost ~ cka_frost_risk$doy, type = 'line')
# lines(cka_frost_risk$run_mean_frost ~ cka_frost_risk$doy, col = 'red')
# 
# cieza_frost_risk <- cieza %>% 
#   mutate(doy = lubridate::yday(Date)) %>% 
#   group_by(doy) %>% 
#   summarise(chance_frost = sum(Tmin <= 0) / n()) %>% 
#   mutate(run_mean_frost = chillR::runn_mean(chance_frost, runn_mean = 10),
#          location = 'Cieza')
# 
# plot(cieza_frost_risk$chance_frost ~ cieza_frost_risk$doy, type = 'line')
# lines(cieza_frost_risk$run_mean_frost ~ cieza_frost_risk$doy, col = 'red')
# 
# zaragoza_frost_risk <- zaragoza %>% 
#   mutate(doy = lubridate::yday(Date)) %>% 
#   group_by(doy) %>% 
#   summarise(chance_frost = sum(Tmin <= 0) / n()) %>% 
#   mutate(run_mean_frost = chillR::runn_mean(chance_frost, runn_mean = 10),
#          location = 'Zaragoza')
# 
# sfax_frost_risk <- sfax %>% 
#   mutate(doy = lubridate::yday(Date)) %>% 
#   group_by(doy) %>% 
#   summarise(chance_frost = sum(Tmin <= 0) / n()) %>% 
#   mutate(run_mean_frost = chillR::runn_mean(chance_frost, runn_mean = 10),
#          location = 'Sfax')
# 
# meknes_frost_risk <- meknes %>% 
#   mutate(doy = lubridate::yday(Date)) %>% 
#   group_by(doy) %>% 
#   summarise(chance_frost = sum(Tmin <= 0) / n()) %>% 
#   mutate(run_mean_frost = chillR::runn_mean(chance_frost, runn_mean = 10),
#          location = 'Meknes')
# 
# 
# #bind the frost risks and then add it to the adamedor table
# frost_risk <- list(cka_frost_risk,
#                    cieza_frost_risk,
#                    zaragoza_frost_risk,
#                    sfax_frost_risk,
#                    meknes_frost_risk) %>% 
#   bind_rows() %>% 
#   expand_grid(species = c('Apple', 'Sweet Cherry', 'Almond', 'European plum', 'Japanese plum', 'Pistachio', 'Apricot', 'Pear'))
# 
# frost_risk %>% 
#   ggplot(aes(x = doy, y = run_mean_frost, col = location)) +
#   geom_line()
# 
# adamedor %>% 
#   mutate(doy_f5 = lubridate::yday(begin_flowering_f5),
#          doy_f50 = lubridate::yday(flowering_f50)) %>% 
#   merge.data.frame(frost_risk, by.x = c('location', 'doy_f5'), by.y = c('location', 'doy'), all.x = TRUE) %>% 
#   filter(species == 'Apple') %>% 
#   ggplot(aes(x= run_mean_frost)) +
#   geom_histogram() +
#   facet_grid(cultivar~location)
# 
# adamedor %>% 
#   mutate(doy_f5 = lubridate::yday(begin_flowering_f5),
#          doy_f50 = lubridate::yday(flowering_f50)) %>% 
#   merge.data.frame(frost_risk, by.x = c('location', 'doy_f50'), by.y = c('location', 'doy'), all.x = TRUE) %>% 
#   filter(species == 'Sweet Cherry') %>% 
#   ggplot(aes(x= run_mean_frost)) +
#   geom_histogram() +
#   facet_grid(cultivar~location)
# 
# 
# flower_sum_f5 <- adamedor %>% 
#   mutate(doy_f5 = lubridate::yday(begin_flowering_f5),
#          doy_f50 = lubridate::yday(flowering_f50)) %>% 
#   group_by(species, location, doy_f5) %>% 
#   summarise(n_flower = n())
# 
# flower_sum_f50 <- adamedor %>% 
#   mutate(doy_f5 = lubridate::yday(begin_flowering_f5),
#          doy_f50 = lubridate::yday(flowering_f50)) %>% 
#   group_by(species, location, doy_f50) %>% 
#   summarise(n_flower = n())
# 
# 
# coef <- 40
# 
# #have full frost risk values for all spcies
# 
# frost_risk
# 
# frost_risk %>% 
#   filter(location == 'Klein-Altendorf') %>% 
#   merge.data.frame(flower_sum_f5, by.y = c('location', 'doy_f5', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
#   ggplot(aes(x = doy)) +
#   geom_bar(aes(y = n_flower / coef), stat = 'identity')+
#   geom_line(aes(y = run_mean_frost, col = location)) +
#   scale_y_continuous("Chance Frost", sec.axis = sec_axis(~ . * coef, name = "Flowering Count"))+
#   facet_wrap(~species)
# 
# frost_risk %>% 
#   filter(species == 'Apple') %>% 
#   merge.data.frame(flower_sum_f50, by.y = c('location', 'doy_f50', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
#   ggplot(aes(x = doy)) +
#   geom_bar(aes(y = n_flower / coef), stat = 'identity')+
#   geom_line(aes(y = run_mean_frost, col = location)) +
#   scale_y_continuous("Chance Frost", sec.axis = sec_axis(~ . * coef, name = "Flowering Count"))+
#   facet_wrap(location~species)
# 
# coef <- 60
# 
# frost_risk %>% 
#   filter(species == 'Apricot') %>% 
#   merge.data.frame(flower_sum_f50, by.y = c('location', 'doy_f50', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
#   ggplot(aes(x = doy)) +
#   geom_bar(aes(y = n_flower / coef), stat = 'identity')+
#   geom_line(aes(y = run_mean_frost, col = location)) +
#   scale_y_continuous("Chance Frost", sec.axis = sec_axis(~ . * coef, name = "Flowering Count"))+
#   facet_wrap(location~species)
# 
# 
# #get maximum frost risk accepted per species
# frost_limit_f5 <- frost_risk %>% 
#   group_by(species) %>% 
#   merge.data.frame(flower_sum_f5, by.y = c('location', 'doy_f5', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
#   na.omit() %>% 
#   group_by(species) %>% 
#   summarise(max_frost_risk = max(run_mean_frost)) %>% 
#   mutate(flowering_stage = 'f5')
# 
# frost_limit_f50 <- frost_risk %>% 
#   group_by(species) %>% 
#   merge.data.frame(flower_sum_f50, by.y = c('location', 'doy_f50', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
#   na.omit() %>% 
#   group_by(species) %>% 
#   summarise(max_frost_risk = max(run_mean_frost)) %>% 
#   mutate(flowering_stage = 'f50')
# ##--> lower boundary of the time window
# 
# #get chance for 
# 
# 
# # Tomita, A. ;  Hagihara, E. ;  Dobashi-Yamashita, M. ;  Shinya, K.
# # Effects of high temperature during flowering on ovule degeneration of sweet cherry (Prunus avium L.).
# #--> temperatures between 22-31 showed ovule degeneration in sweet cherry, but severity differed by cultivars
# 
# #maybe use 25°C?
# heat_threshold <- 30
# 
# cka_heat_risk <- cka %>% 
#   mutate(doy = lubridate::yday(Date)) %>% 
#   group_by(doy) %>% 
#   summarise(chance_heat = sum(Tmax >= heat_threshold) / n()) %>% 
#   mutate(run_mean_heat = chillR::runn_mean(chance_heat, runn_mean = 10),
#          location = 'Klein-Altendorf')
# 
# cieza_heat_risk <- cieza %>% 
#   mutate(doy = lubridate::yday(Date)) %>% 
#   group_by(doy) %>% 
#   summarise(chance_heat = sum(Tmax >=  heat_threshold) / n()) %>% 
#   mutate(run_mean_heat = chillR::runn_mean(chance_heat, runn_mean = 10),
#          location = 'Cieza')
# 
# zaragoza_heat_risk <- zaragoza %>% 
#   mutate(doy = lubridate::yday(Date)) %>% 
#   group_by(doy) %>% 
#   summarise(chance_heat = sum(Tmax >= heat_threshold) / n()) %>% 
#   mutate(run_mean_heat = chillR::runn_mean(chance_heat, runn_mean = 10),
#          location = 'Zaragoza')
# 
# sfax_heat_risk <- sfax %>% 
#   mutate(doy = lubridate::yday(Date)) %>% 
#   group_by(doy) %>% 
#   summarise(chance_heat = sum(Tmax >= heat_threshold) / n()) %>% 
#   mutate(run_mean_heat = chillR::runn_mean(chance_heat, runn_mean = 10),
#          location = 'Sfax')
# 
# meknes_heat_risk <- meknes %>% 
#   mutate(doy = lubridate::yday(Date)) %>% 
#   group_by(doy) %>% 
#   summarise(chance_heat = sum(Tmax >= heat_threshold) / n()) %>% 
#   mutate(run_mean_heat = chillR::runn_mean(chance_heat, runn_mean = 10),
#          location = 'Meknes')
# 
# heat_risk <- list(cka_heat_risk,
#                   cieza_heat_risk,
#                   zaragoza_heat_risk,
#                   sfax_heat_risk,
#                   meknes_heat_risk) %>% 
#   bind_rows() %>% 
#   expand_grid(species = c('Apple', 'Sweet Cherry', 'Almond', 'European plum', 'Japanese plum', 'Pistachio', 'Apricot', 'Pear'))
# 
# 
# heat_risk %>% 
#   ggplot(aes(x = doy, y = run_mean_heat, col = location)) +
#   geom_line()
# 
# heat_risk %>% 
#   filter(location == 'Zaragoza') %>% 
#   merge.data.frame(flower_sum_f5, by.y = c('location', 'doy_f5', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
#   ggplot(aes(x = doy)) +
#   geom_bar(aes(y = n_flower / coef), stat = 'identity')+
#   geom_line(aes(y = run_mean_heat, col = location)) +
#   scale_y_continuous("Chance Heat", sec.axis = sec_axis(~ . * coef, name = "Flowering Count"))+
#   facet_wrap(~species)
# 
# 
# 
# heat_limit_f5 <- heat_risk %>% 
#   group_by(species) %>% 
#   merge.data.frame(flower_sum_f5, by.y = c('location', 'doy_f5', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
#   na.omit() %>% 
#   group_by(species) %>% 
#   summarise(max_heat_risk = max(run_mean_heat)) %>% 
#   mutate(flowering_stage = 'f5')
# 
# heat_limit_f50 <- heat_risk %>% 
#   group_by(species) %>% 
#   merge.data.frame(flower_sum_f50, by.y = c('location', 'doy_f50', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
#   na.omit() %>% 
#   group_by(species) %>% 
#   summarise(max_heat_risk = max(run_mean_heat)) %>% 
#   mutate(flowering_stage = 'f50')
# 
# #what time windows whould those two criteria mean??
# 
# df1 <-  merge.data.frame(frost_limit_f5, heat_limit_f5, by = c('species', 'flowering_stage'))
# df2 <- merge.data.frame(frost_limit_f50, heat_limit_f50, by = c('species', 'flowering_stage'))
# 
# thermal_window_threshold <- rbind(df1, df2)
# 
# thermal_window_threshold
# 
# time_window_df <- data.frame()
# 
# 
# 
# 
# lower <- frost_risk %>% 
#   merge.data.frame(thermal_window_threshold, by = c('species')) %>% 
#   filter(run_mean_frost <=  max_frost_risk) %>% 
#   group_by(location, species, flowering_stage) %>% 
#   summarise(min_doy = min(doy))
# 
# 
# #only take values left of the maximum heat risk
# max_heat_risk <- heat_risk %>% 
#   group_by(location, .drop = FALSE) %>% 
#   summarise(max_risk = max(run_mean_heat))
# 
# doy_max_risk_df <- merge.data.frame(heat_risk, max_heat_risk, by = 'location') %>% 
#   group_by(location) %>% 
#   filter(run_mean_heat == max_risk) %>% 
#   summarise(doy_max_risk = min(doy))
# 
# upper <- heat_risk %>% 
#   merge.data.frame(doy_max_risk_df, by = 'location') %>% 
#   filter(doy <= doy_max_risk) %>% 
#   merge.data.frame(thermal_window_threshold, by = c('species')) %>% 
#   filter(run_mean_heat <= max_heat_risk) %>% 
#   group_by(location, species, flowering_stage) %>% 
#   summarise(max_doy = max(doy))
# 
# 
# 
# thermal_time_window_final <- merge.data.frame(lower, upper, by = c('location', 'species', 'flowering_stage'))

library(tidyverse)

hist_bloom <- read.csv('data/projected_bloomdates_ensemble_historic_scenarios.csv') %>% 
  group_by(species, cultivar, location) %>% 
  summarize(median_bloom = median(pheno_predicted, na.rm = TRUE),
            sd_bloom = sd(pheno_predicted, na.rm = TRUE))

future_bloom <- data.table::fread('data/projected_bloomdates_future.csv') %>% 
  group_by(spec_cult, location, ssp, scenario_year) %>% 
  summarize(median_futurebloom = median(weighted_pred, na.rm = TRUE),
            sd_futurebloom = sd(weighted_pred, na.rm = TRUE),
            med_fail = sum(is.na(weighted_pred)) / n())
  
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

rm(adamedor, f50_cult, f5_cult)

master_fitting_data <- read.csv('data/master_phenology_repeated_splits.csv') %>% 
  filter(repetition == 1) %>% 
  mutate(species = gsub('Japanese Plum', 'European Plum', species)) %>% 
  group_by(species, cultivar) %>% 
  summarize(loc = unique(location)) %>% 
  mutate(spec_cult_loc = paste(species, cultivar, loc, sep = '_'),
         species_location = paste(species, loc, sep = '_')) %>% 
  filter(cultivar %in% cultivar_n)

future_sub <- future_bloom %>% 
  mutate(spec_cult_loc = paste(spec_cult, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) 

hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(location == 'Zaragoza') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = species)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)

hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(location == 'Klein-Altendorf') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = species)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)

hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(location == 'Meknes') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = species)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)


hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(location == 'Sfax') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = species)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)

hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(location == 'Cieza') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = species)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)

hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(location == 'Santomera') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = species)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)


hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(species == 'Sweet Cherry') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = location)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)

hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(species == 'Almond') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = location)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)

hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(species == 'Pear') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = location)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)

hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(species == 'Apple') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = location)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)

hist_bloom %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(future_sub, by = c('spec_cult_loc', 'location')) %>%
  filter(species == 'Apricot') %>% 
  #filter(ssp == 'ssp585') %>% 
  ggplot(aes(x = median_bloom, y = median_futurebloom)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(col = location)) +
  #geom_errorbar(aes(ymin = median_futurebloom - sd_futurebloom, 
  #                  ymax = median_futurebloom + sd_futurebloom,
  #                  col = species)) +
  #geom_errorbarh(aes(xmin = median_bloom - sd_bloom, xmax = median_bloom + sd_bloom, col = species))+
  facet_grid(scenario_year~ssp)




#######
future_bloom <- data.table::fread('data/projected_bloomdates_future.csv') %>% 
  group_by(spec_cult, location, ssp, scenario_year, gcm) %>% 
  summarize(med_fail = sum(is.na(weighted_pred)) / n()) %>% 
  ungroup() %>% 
  group_by(spec_cult, location, ssp, scenario_year) %>% 
  summarize(sd_fail = sd(med_fail),
            med_fail = median(med_fail))

future_sub_fail <- future_bloom %>% 
  tidyr::separate(spec_cult, into = c('species', 'cultivar'), sep = '_') %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum')) %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  mutate(ssp = factor(ssp, levels = c('ssp126', 'ssp245', 'ssp370', 'ssp585'),
                      labels = c('SSP1', 'SSP2', 'SSP3', 'SSP5')))

par_df <- read.csv('data/parameter_cultivars.csv') %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum'))

performance <- read.csv('data/performance_fitted_models.csv') %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum')) %>% 
  filter(split == 'Validation',
         cultivar %in% cultivar_n)

peformance_rpiq_sum <- performance %>% 
  group_by(species, cultivar) %>% 
  summarize(rpiq_sum = sum(rpiq_adj))

performance <- performance %>%  
  merge(peformance_rpiq_sum, by = c('species', 'cultivar')) %>% 
  mutate(rpiq_share = rpiq_adj /rpiq_sum)

new_order <- par_df %>% 
  merge(performance, by = c('species', 'cultivar')) %>% 
  mutate(yc_weighted = yc * rpiq_share) %>% 
  group_by(species, cultivar) %>% 
  summarise(yc = median(yc),
            yc_weighted = sum(yc_weighted)) %>% 
  do(tibble(al=levels(reorder(interaction(.$species, .$cultivar, drop=TRUE, sep = '+'), .$yc_weighted)))) %>% 
  pull(al)

test <- par_df %>% 
  group_by(species, cultivar) %>% 
  summarize(yc = median(yc))

#order species by their mean estimated chill demand in the parameters

library(colorRamps)

fail_plot_list <- list()

for(loc in c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Sfax', 'Meknes')){
  fail_plot_list[[loc]] <- future_sub_fail %>% 
    mutate(al=factor(interaction(species, cultivar,  sep = '+'), levels=new_order)) %>% 
    filter(location == loc) %>% 
    ggplot(aes(y=al, x = ssp)) +
    geom_tile(aes(fill = med_fail)) +
    scale_fill_gradientn(colours = matlab.like(15),
                         labels = scales::percent,
                         limits = c(0, 1), 
                         name = paste0('Median\nFailure Rate'))+
    theme_bw(base_size = 15) +
    scale_y_discrete(breaks= new_order, labels=gsub("^.*\\+", "", new_order)) +
    ylab('Cultivar (ordered by decreasing estimated chill requirement)') +
    xlab('Shared Socioeconomic Pathway (SSP)') 
  
  #ggsave(filename = paste0('figures/paper/failure-per-cultivar_', loc, '.jpeg'),plot = p, height = 20, width = 25, units = 'cm', device = 'jpeg')
}

library(patchwork)
fail_plot_list$`Klein-Altendorf` + fail_plot_list$Zaragoza + fail_plot_list$Cieza +
  fail_plot_list$Santomera + fail_plot_list$Sfax + fail_plot_list$Meknes + plot_layout(guides = 'collect')



new_order <- future_sub_fail %>% 
  filter(scenario_year == 2085, ssp == 'SSP5') %>% 
  group_by(species, cultivar, location) %>% 
  summarise(fail_2085 = median(med_fail)) %>% 
  do(tibble(al=levels(reorder(interaction(.$species, .$location, .$cultivar,  drop=TRUE, sep = '+'), .$fail_2085)))) %>% 
  pull(al)


#maybe order it by species, instead
fail_plot_list <- list()

for(spec in c('Almond', 'Pistachio', 'Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'European Plum')){
  
  sub <- future_sub_fail %>% 
    mutate(al=factor(interaction(species, location, cultivar,  sep = '+'), levels=new_order),
           ssp = recode(ssp, SSP1 = "1", SSP2 = "2", SSP3 = "3", SSP5 = "5"),
           location = recode(location, `Klein-Altendorf` = 'K-A')) %>% 
    filter(species == spec)
  
  ylab_text <- ''
  if(spec == 'Sweet Cherry') ylab_text <- 'Cultivar (ordered by decreasing estimated chill requirement)'
  xlab_text <- ''
  if(spec == 'Pistachio') xlab_text <- 'Shared Socioeconomic Pathway (SSP)'
  if(spec == 'Apple'){
    sub <- sub %>% 
      mutate(location = recode(location, Meknes = 'M'))
    
  }
  if(spec == 'Almond'){
    sub <- sub %>% 
      mutate(location = recode(location, Meknes = 'Mekn.'))
  }
  
  
  
  fail_plot_list[[spec]] <- sub %>% 
    ggplot(aes(y=al, x = ssp)) +
    geom_tile(aes(fill = med_fail)) +
    scale_fill_gradientn(colours = matlab.like(15),
                         labels = scales::percent,
                         limits = c(0, 1),
                         name = paste0('Median Failure Rate'))+
    # scale_fill_gradient(low = "white", high = "firebrick2", 
    #                     labels = scales::percent,
    #                     limits = c(0, 1), 
    #                     name = paste0('Median Failure Rate'))+
    facet_grid(species~scenario_year, scales = 'free_y', space = 'free_y') +
    facet_grid(location~scenario_year, scales = 'free_y', space = 'free_y') +
    theme_bw(base_size = 15) +
    theme(plot.margin = unit(c(0,0,0,0), "cm")) +
    scale_y_discrete(breaks= new_order, labels=gsub("^.*\\+", "", new_order), limits = rev) +
    ylab(ylab_text) +
    xlab(xlab_text) 
  
  #ggsave(filename = paste0('figures/paper/failure-per-cultivar_', loc, '.jpeg'),plot = p, height = 20, width = 25, units = 'cm', device = 'jpeg')
}

layout <- "
ACG
ACG
ACG
ACG
ACG
ADG
ADG
AEG
AEG
AEG
AEG
AEG
BFG
"

#size of plot in cm
plot_height <- 30
plot_width <- 27
cm_to_inch <- 1/2.54

#order the plot by failure rate in 2085 for ssp5

#remove the ylab
ylab <- fail_plot_list$`Sweet Cherry`$labels$y
fail_plot_list$`Sweet Cherry`$labels$y <- fail_plot_list$`European Plum`$labels$y <- fail_plot_list$Apricot$labels$y <-
  fail_plot_list$Apple$labels$y <- fail_plot_list$Pear$labels$y <- fail_plot_list$Pistachio$labels$y <-
  fail_plot_list$Almond$labels$y <- " "

#remove the xlab
xlab <- fail_plot_list$Pistachio$labels$x
fail_plot_list$`Sweet Cherry`$labels$x <- fail_plot_list$`European Plum`$labels$x <- fail_plot_list$Apricot$labels$x <-
  fail_plot_list$Apple$labels$x <- fail_plot_list$Pear$labels$x <- fail_plot_list$Pistachio$labels$x <-
  fail_plot_list$Almond$labels$x <- " "

jpeg(file="figures/paper/failure_cultivar-combined-v2.jpeg",
     height = plot_height, width = plot_width, units = 'cm', res = 300)

fail_plot_list$`Sweet Cherry` + fail_plot_list$`European Plum` + fail_plot_list$Apricot +
  fail_plot_list$Apple + fail_plot_list$Pear + fail_plot_list$Pistachio + 
  fail_plot_list$Almond +plot_layout(guides = 'collect', design = layout) & theme(legend.position = 'bottom')
#draw the ylab manually
grid::grid.draw(grid::textGrob(ylab, x = 0.01, rot = 90))
grid::grid.draw(grid::textGrob(xlab, y = 0.1))
dev.off()



svg(file="figures/paper/failure_cultivar-combined-v2.svg",
     height = plot_height * cm_to_inch,
    width = plot_width * cm_to_inch)

fail_plot_list$`Sweet Cherry` + fail_plot_list$`European Plum` + fail_plot_list$Apricot +
  fail_plot_list$Apple + fail_plot_list$Pear + fail_plot_list$Pistachio + 
  fail_plot_list$Almond +plot_layout(guides = 'collect', design = layout) & theme(legend.position = 'bottom')
#draw the ylab manually
grid::grid.draw(grid::textGrob(ylab, x = 0.01, rot = 90))
grid::grid.draw(grid::textGrob(xlab, y = 0.1))
dev.off()



#ggsave(filename = 'figures/paper/failure_cultivar-combined.jpeg', height = 20, width = 25, units = 'cm', device = 'jpeg')



future_bloom <- data.table::fread('data/projected_bloomdates_future.csv') %>% 
  group_by(spec_cult, location, ssp, scenario_year, gcm) %>% 
  summarize(median_futurebloom = median(weighted_pred, na.rm = TRUE),
            sd_futurebloom = sd(weighted_pred, na.rm = TRUE))

unique(future_bloom$spec_cult)

str(future_bloom)

hist_bloom <- hist_bloom %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum'))


shift_df <- future_bloom %>% 
  tidyr::separate(spec_cult, into = c('species', 'cultivar'), sep = '_') %>% 
  mutate(species = recode(species, `Japanese Plum` = 'European Plum')) %>% 
  mutate(spec_cult_loc = paste(species, cultivar, location, sep = '_')) %>% 
  filter(spec_cult_loc %in% master_fitting_data$spec_cult_loc) %>% 
  merge(hist_bloom, by = c('species', 'cultivar', 'location')) %>% 
  mutate(shift = median_futurebloom - median_bloom) %>% 
  group_by(species, cultivar, location, ssp, scenario_year) %>% 
  summarise(med_shift = median(shift, na.rm = TRUE)) %>% 
  mutate(al=factor(interaction(species, cultivar,  sep = '+'), levels=new_order)) %>% 
  mutate(ssp = factor(ssp, levels = c('ssp126', 'ssp245', 'ssp370', 'ssp585'),
                      labels = c('SSP1', 'SSP2', 'SSP3', 'SSP5')))


rng <- range(shift_df$med_shift)

loc <- 'Sfax'

for(loc in c('Klein-Altendorf', 'Zaragoza', 'Cieza', 'Santomera', 'Sfax', 'Meknes')){
  p <- shift_df %>% 
    filter(location == loc) %>% 
    ggplot(aes(y=al, x = ssp)) +
    geom_tile(aes(fill = med_shift)) +
    scale_fill_gradient2(low = 'blue',mid = 'white', high = 'red', 
                         limits = rng,
                         name = paste0('Median\nShift Bloom\nin ', loc))+
    facet_grid(species~scenario_year, scales = 'free_y', space = 'free_y') +
    theme_bw(base_size = 15) +
    scale_y_discrete(breaks= new_order, labels=gsub("^.*\\+", "", new_order)) +
    ylab('Cultivar (ordered by decreasing estimated chill requirement)') +
    xlab('Shared Socioeconomic Pathway (SSP)') 
  
  ggsave(filename = paste0('figures/paper/shift-per-cultivar_', loc, '.jpeg'),plot = p, height = 20, width = 25, units = 'cm', device = 'jpeg')
}


#shift


shift_plot_list <- list()


for(spec in c('Almond', 'Pistachio', 'Apple', 'Pear', 'Apricot', 'Sweet Cherry', 'European Plum')){
  
  sub <- shift_df %>% 
    merge(future_sub_fail, by = c('species', 'cultivar', 'location', 'ssp', 'scenario_year')) %>% 
    mutate(al=factor(interaction(species, location, cultivar,  sep = '+'), levels=new_order),
           ssp = recode(ssp, SSP1 = "1", SSP2 = "2", SSP3 = "3", SSP5 = "5"),
           location = recode(location, `Klein-Altendorf` = 'K-A'),
           med_shift = ifelse(med_shift > 30, yes = 30, no = med_shift)) %>% 
    filter(species == spec) 

  
  ylab_text <- ''
  if(spec == 'Sweet Cherry') ylab_text <- 'Cultivar (ordered by decreasing estimated chill requirement)'
  xlab_text <- ''
  if(spec == 'Pistachio') xlab_text <- 'Shared Socioeconomic Pathway (SSP)'
  if(spec == 'Apple'){
    sub <- sub %>% 
      mutate(location = recode(location, Meknes = 'M'))
    
  }
  if(spec == 'Almond'){
    sub <- sub %>% 
      mutate(location = recode(location, Meknes = 'Mekn.'))
  }
  
  

  shift_plot_list[[spec]] <- sub %>% 
    ggplot(aes(y=al, x = ssp)) +
    geom_tile(aes(fill = med_shift)) +
    # scale_fill_gradient2(low = 'blue',mid = 'white', high = 'red', 
    #                      limits = rng,
    #                      name = paste0('Median Shift Flowering (days)\ncompared to 2015'))+
    scale_fill_gradientn(colours = matlab.like(15),
                         limits = c(-30, 30),
                         name = 'Median Shift Flowering (days)\ncompared to 2015')+
    facet_grid(location~scenario_year, scales = 'free_y', space = 'free_y') +
    geom_point(data = sub[sub$med_fail >= 0.5,], aes(x = ssp, y = al, shape=''), size = 1.4) +
    theme_bw(base_size = 15) +
    theme(plot.margin = unit(c(0,0,0,0), "cm")) +
    scale_shape_manual(values = 4, name = 'Median Failure Rate > 50%') +
    scale_y_discrete(breaks= new_order, labels=gsub("^.*\\+", "", new_order), limits = rev) +
    ylab('Cultivar (ordered by decreasing estimated chill requirement)') +
    xlab('Shared Socioeconomic Pathway (SSP)') 
  
  #ggsave(filename = paste0('figures/paper/failure-per-cultivar_', loc, '.jpeg'),plot = p, height = 20, width = 25, units = 'cm', device = 'jpeg')
}

layout <- "
ACG
ACG
ACG
ACG
ACG
ADG
ADG
AEG
AEG
AEG
AEG
AEG
BFG
"

#order the plot by failure rate in 2085 for ssp5

#remove the ylab
ylab <- shift_plot_list$`Sweet Cherry`$labels$y
shift_plot_list$`Sweet Cherry`$labels$y <- shift_plot_list$`European Plum`$labels$y <- shift_plot_list$Apricot$labels$y <-
  shift_plot_list$Apple$labels$y <- shift_plot_list$Pear$labels$y <- shift_plot_list$Pistachio$labels$y <-
  shift_plot_list$Almond$labels$y <- " "

#remove the xlab
xlab <- shift_plot_list$Pistachio$labels$x
shift_plot_list$`Sweet Cherry`$labels$x <- shift_plot_list$`European Plum`$labels$x <- shift_plot_list$Apricot$labels$x <-
  shift_plot_list$Apple$labels$x <- shift_plot_list$Pear$labels$x <- shift_plot_list$Pistachio$labels$x <-
  shift_plot_list$Almond$labels$x <- " "

jpeg(file="figures/paper/shift_cultivar-combined-v2.jpeg",
     height = plot_height, width = plot_width, units = 'cm', res = 300)

shift_plot_list$`Sweet Cherry` + shift_plot_list$`European Plum` + shift_plot_list$Apricot +
  shift_plot_list$Apple + shift_plot_list$Pear + shift_plot_list$Pistachio + 
  shift_plot_list$Almond +plot_layout(guides = 'collect', design = layout) & theme(legend.position = 'bottom')
#draw the ylab manually
grid::grid.draw(grid::textGrob(ylab, x = 0.01, rot = 90))
grid::grid.draw(grid::textGrob(xlab, y = 0.1))
dev.off()


svg(file="figures/paper/shift_cultivar-combined-v2.svg",
    height = plot_height * cm_to_inch,
    width = plot_width * cm_to_inch)

shift_plot_list$`Sweet Cherry` + shift_plot_list$`European Plum` + shift_plot_list$Apricot +
  shift_plot_list$Apple + shift_plot_list$Pear + shift_plot_list$Pistachio + 
  shift_plot_list$Almond +plot_layout(guides = 'collect', design = layout) & theme(legend.position = 'bottom')
#draw the ylab manually
grid::grid.draw(grid::textGrob(ylab, x = 0.01, rot = 90))
grid::grid.draw(grid::textGrob(xlab, y = 0.1))
dev.off()

cross_out_df <- future_sub_fail %>% 
  filter(med_fail >= 0.5)
  

shift_ka <- shift_df %>% 
  filter(location == 'Klein-Altendorf') %>% 
  group_by(species, scenario_year, ssp) %>% 
  summarize(median = median(med_shift))

shift_delay <- shift_df %>% 
  filter(location %in% c('Sfax', 'Meknes', 'Santomera', 'Cieza')) %>% 
  group_by(species, scenario_year, ssp, location) %>% 
  summarize(median = median(med_shift))

shift_stagnaation <- shift_df %>% 
  filter(location %in% c('Meknes', 'Zaragoza')) %>% 
  group_by(species, scenario_year, ssp, location) %>% 
  summarize(median = median(med_shift))


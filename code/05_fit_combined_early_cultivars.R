#fitting flowering groups

library(chillR)
library(tidyverse)
library(MEIGOR)
library(LarsChill)

sfax <- read.csv('data/sfax-org_1973-2021.csv')
meknes <- read.csv('data/meknes-org_1972-2014.csv')

#make hourly
coord_meknes <- c(33.88, -5.54)
coord_sfax <-c(34.75, 10.75)

sfax_hourly <- stack_hourly_temps(sfax, latitude = coord_sfax[1])
meknes_hourly <- stack_hourly_temps(meknes, latitude = coord_meknes[1])

sfax_hourly <- sfax_hourly$hourtemps
meknes_hourly <- meknes_hourly$hourtemps

#make seasonal list
sfax_season <- genSeasonList(sfax_hourly, years = 1974:2021)
meknes_season <- genSeasonList(meknes_hourly, years = 1973:2014)

names(sfax_season) <- 1974:2021
names(meknes_season) <- 1973:2014

SeasonList <- list('Sfax' = sfax_season, 
                   'Meknes' = meknes_season)








adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

adamedor_sum <- adamedor %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(),
            locations = length(unique(location)),
            countries = length(unique(country)))

almond_cult <- adamedor_sum %>% 
  filter(species == 'Almond', 
         n >= 20) %>% 
  dplyr::pull(cultivar)



#take all almond data

almond_sub <- adamedor %>% 
  filter(species == 'Almond',
         cultivar %in% almond_cult) %>% 
  drop_na(begin_flowering_f5) %>% 
  mutate(begin_flowering_f5 = lubridate::ymd(begin_flowering_f5)) %>% 
  mutate(doy_begin = lubridate::yday(begin_flowering_f5))

#only keep observations that have data for begin of flowering


cultivars <- unique(almond_sub$cultivar)
p <- 0.75
seed <- 1234567890


pheno_cal_list <- season_cal_list <- pheno_val_list <- season_val_list <- list()

#take calibration and validation data from each cultivar
for(cult in cultivars){
  
  pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()
  season_cal_list[[cult]] <-  season_val_list[[cult]] <- list()
  
  #check which and how many locations
  overview_df <- almond_sub %>% 
    dplyr::filter(cultivar == cult) %>% 
    group_by(location) %>% 
    summarise(n = n()) %>% 
    mutate(n_cal = floor(n * p),
           n_val = ceiling(n * (1 - p)))
  
  #for each location decide how much we take for training and calibration
  for(loc in overview_df$location){
    #extract years with observations
    pheno_years <- almond_sub %>% 
      dplyr::filter(cultivar == cult, location == loc) %>% 
      summarise(pheno_years = unique(year)) %>% 
      dplyr::pull(pheno_years)
    
    
    #decide which years belong to calibration and validation
    cal_years <- sort(sample(x = pheno_years, 
                             size = overview_df$n_cal[overview_df$location == loc], 
                             replace = FALSE))
    
    val_years <- pheno_years[!pheno_years %in% cal_years]
    
    
    #extract corresponding phenology data
    pheno_cal <- almond_sub %>% 
      dplyr::filter(location == loc, cultivar == cult, year %in% cal_years) %>% 
      dplyr::pull(doy_begin)
    
    pheno_val <- almond_sub %>% 
      dplyr::filter(location == loc, cultivar == cult, year %in% val_years) %>% 
      dplyr::pull(doy_begin)
    
    
    
    #add phenology information to list
    pheno_cal_list[[cult]] <- rbind(pheno_cal_list[[cult]],
                                    data.frame(location = loc,
                                               year = cal_years,
                                               pheno = pheno_cal))
    
    pheno_val_list[[cult]] <- rbind(pheno_val_list[[cult]],
                                    data.frame(location = loc,
                                               year = val_years,
                                               pheno = pheno_val))
    
    
    #add season data to season list
    season_cal_list[[cult]] <- c(season_cal_list[[cult]],
                                 SeasonList[[loc]][as.character(cal_years)]) 
    
    season_val_list[[cult]] <- c(season_val_list[[cult]],
                                 SeasonList[[loc]][as.character(val_years)]) 
    
    
  }
  
}



#create groups of flowering
quan_df <- almond_sub %>% 
  group_by(cultivar) %>% 
  summarise(up = quantile(doy_begin, probs = 0.25),
            mid = median(doy_begin),
            low = quantile(doy_begin, probs = 0.75))



summed_dif <- abs(rep(quan_df$up, nrow(quan_df)) - rep(quan_df$up, each = nrow(quan_df))) + 
  abs(rep(quan_df$mid, nrow(quan_df)) - rep(quan_df$mid, each = nrow(quan_df))) +  
  abs(rep(quan_df$low, nrow(quan_df)) - rep(quan_df$low, each = nrow(quan_df)))

distance_matrix <- matrix(summed_dif, nrow = nrow(quan_df), ncol = nrow(quan_df), byrow = TRUE)


distance_matrix_v2 <- stats::dist(quan_df[,2:4], diag = TRUE, upper = TRUE)
names(distance_matrix_v2) <- make.names(quan_df$cultivar)

error.freq.hclust <- hclust(distance_matrix_v2, method = "ward.D2")
plot(error.freq.hclust, hang = -1)
rect.hclust(error.freq.hclust, 3)

quan_df$cluster <- cutree(error.freq.hclust, 3)

merge.data.frame(almond_sub, quan_df, by = 'cultivar') %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  mutate(cluster = factor(cluster, labels = c('early', 'mid', 'late'), levels = c('1', '3', '2'))) %>% 
  ggplot(aes(x = cultivar, y = doy_begin)) + 
  geom_boxplot() +
  facet_grid(~cluster, scales = 'free_x') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

almond_sub <- merge.data.frame(almond_sub, quan_df, by = 'cultivar') %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  mutate(cluster = factor(cluster, labels = c('early', 'mid', 'late'), levels = c('1', '3', '2')))


#do the fitting

early_cult <- unique(almond_sub[almond_sub$cluster == 'early', 'cultivar'])
mid_cult <- unique(almond_sub[almond_sub$cluster == 'mid', 'cultivar'])
late_cult <- unique(almond_sub[almond_sub$cluster == 'late', 'cultivar'])



ncult <- length(early_cult)

x_0 <- c(rep(40,ncult),   rep(190,ncult),   rep(0.5,ncult),  25,   279,    286.1,     47.7,             28,        4,   36,     4,    1.60)
x_U <- c(rep(80,ncult),   rep(500,ncult),   rep(1.0,ncult),  30,   281,      287,       48,             50,       10,   40,    10,    5.00)
x_L <- c(rep(20,ncult),   rep(100,ncult),   rep(0.1,ncult),  15,   279,      286,       16,             24,        2,   20,     0,    1.2)

#limits for the inequality constraints
#         #gdh parameters   #q10 for E0 and E1
c_L <- c(  0,   0,   0,     1.5, 1.5)
c_U <- c(Inf, Inf, Inf,     3.5, 3.5)



problem<-list(f="evaluation_function_meigo_nonlinear_combined",
              x_0 = x_0,
              x_L = x_L,
              x_U = x_U,
              c_L = c_L, 
              c_U = c_U)


#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 30, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0)

#check if the function works
evaluation_function_meigo_nonlinear_combined(x = x_0, 
                                             modelfn = custom_PhenoFlex_GDHwrapper, 
                                             bloomJDays = pheno_cal_list[early_cult],
                                             SeasonList = season_cal_list[early_cult],
                                             n_cult = ncult)

set.seed(123456789)

res_list <- MEIGO(problem = problem,
                  opts,
                  algorithm="ESS", 
                  modelfn = custom_PhenoFlex_GDHwrapper,
                  bloomJDays = pheno_cal_list[early_cult],
                  SeasonList = season_cal_list[early_cult],
                  n_cult = ncult)



x_0 <- res_list$xbest

problem<-list(f="evaluation_function_meigo_nonlinear_combined",
              x_0 = x_0,
              x_L = x_L,
              x_U = x_U,
              c_L = c_L, 
              c_U = c_U)

#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 60, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0)

res_list2 <- MEIGO(problem = problem,
                   opts,
                   algorithm="ESS", 
                   modelfn = custom_PhenoFlex_GDHwrapper,
                   bloomJDays = pheno_cal_list[early_cult],
                   SeasonList = season_cal_list[early_cult],
                   n_cult = ncult)

prediction_df2 <- data.frame()


par_all <- res_list2$xbest
for(i in 1:length(early_cult)){
  
  cult <- early_cult[i]
  
  par_cult <- par_all[c(i, (ncult*1)+(i), (ncult * 2) + (i), (length(par_all)-8):length(par_all))]
  
  
  pheno_cal_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_cal_list[[cult]])
  pheno_val_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_val_list[[cult]])
  
  prediction_df2 <- rbind.data.frame(prediction_df2,
                                     rbind.data.frame(cbind.data.frame(cultivar = cult,
                                                                       data = 'calibration',
                                                                       pheno_cal_list[[cult]],
                                                                       pheno_pred = pheno_cal_pred),
                                                      
                                                      cbind.data.frame(cultivar = cult,
                                                                       data = 'validation',
                                                                       pheno_val_list[[cult]],
                                                                       pheno_pred = pheno_val_pred)
                                     ))
  
}

prediction_df2 %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0)

RMSEP(predicted = prediction_df2$pheno_pred, prediction_df2$pheno)
RPIQ(predicted = prediction_df2$pheno_pred, prediction_df2$pheno)

prediction_df2$error <- prediction_df2$pheno_pred - prediction_df2$pheno

prediction_df2 %>% 
  ggplot(aes(x = error, fill = data)) +
  geom_histogram() +
  facet_wrap(~cultivar)







x_0 <- res_list2$xbest

problem<-list(f="evaluation_function_meigo_nonlinear_combined",
              x_0 = x_0,
              x_L = x_L,
              x_U = x_U,
              c_L = c_L, 
              c_U = c_U)

#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 60, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0)

set.seed(123456789)

res_list3 <- MEIGO(problem = problem,
                   opts,
                   algorithm="ESS", 
                   modelfn = custom_PhenoFlex_GDHwrapper,
                   bloomJDays = pheno_cal_list[early_cult],
                   SeasonList = season_cal_list[early_cult],
                   n_cult = ncult)


prediction_df3 <- data.frame()


par_all <- res_list3$xbest
for(i in 1:length(early_cult)){
  
  cult <- early_cult[i]
  
  par_cult <- par_all[c(i, (ncult*1)+(i), (ncult * 2) + (i), (length(par_all)-8):length(par_all))]
  
  
  pheno_cal_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_cal_list[[cult]])
  pheno_val_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_val_list[[cult]])
  
  prediction_df3 <- rbind.data.frame(prediction_df3,
                                     rbind.data.frame(cbind.data.frame(cultivar = cult,
                                                                       data = 'calibration',
                                                                       pheno_cal_list[[cult]],
                                                                       pheno_pred = pheno_cal_pred),
                                                      
                                                      cbind.data.frame(cultivar = cult,
                                                                       data = 'validation',
                                                                       pheno_val_list[[cult]],
                                                                       pheno_pred = pheno_val_pred)
                                     ))
  
}

prediction_df3 %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0)

RMSEP(predicted = prediction_df3$pheno_pred, prediction_df3$pheno)
RPIQ(predicted = prediction_df3$pheno_pred, prediction_df3$pheno)

prediction_df3$error <- prediction_df3$pheno_pred - prediction_df3$pheno

prediction_df3 %>% 
  ggplot(aes(x = error, fill = data)) +
  geom_histogram() +
  facet_wrap(~cultivar)



#round 4
x_0 <- res_list3$xbest
x_U <- c(rep(80,ncult),   rep(500,ncult),   rep(1.0,ncult),  30,   281,      287,       48,             50,       10,   40,    10,    5.00)
x_L <- c(rep(5,ncult),   rep(50,ncult),   rep(0.1,ncult),  15,   279,      286,       16,             24,        2,   20,     0,    1.2)

problem<-list(f="evaluation_function_meigo_nonlinear_combined",
              x_0 = x_0,
              x_L = x_L,
              x_U = x_U,
              c_L = c_L, 
              c_U = c_U)

#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 60 * 2, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0)

set.seed(123456789)

res_list4 <- MEIGO(problem = problem,
                   opts,
                   algorithm="ESS", 
                   modelfn = custom_PhenoFlex_GDHwrapper,
                   bloomJDays = pheno_cal_list[early_cult],
                   SeasonList = season_cal_list[early_cult],
                   n_cult = ncult)


prediction_df4 <- data.frame()


par_all <- res_list4$xbest
for(i in 1:length(early_cult)){
  
  cult <- early_cult[i]
  
  par_cult <- par_all[c(i, (ncult*1)+(i), (ncult * 2) + (i), (length(par_all)-8):length(par_all))]
  
  
  pheno_cal_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_cal_list[[cult]])
  pheno_val_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_val_list[[cult]])
  
  prediction_df4 <- rbind.data.frame(prediction_df4,
                                     rbind.data.frame(cbind.data.frame(cultivar = cult,
                                                                       data = 'calibration',
                                                                       pheno_cal_list[[cult]],
                                                                       pheno_pred = pheno_cal_pred),
                                                      
                                                      cbind.data.frame(cultivar = cult,
                                                                       data = 'validation',
                                                                       pheno_val_list[[cult]],
                                                                       pheno_pred = pheno_val_pred)
                                     ))
  
}

prediction_df4 %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0)

RMSEP(predicted = prediction_df4$pheno_pred, prediction_df4$pheno)
RPIQ(predicted = prediction_df4$pheno_pred, prediction_df4$pheno)

prediction_df4$error <- prediction_df4$pheno_pred - prediction_df4$pheno

prediction_df4 %>% 
  ggplot(aes(x = error, fill = data)) +
  geom_histogram() +
  facet_wrap(~cultivar)

saveRDS(res_list4, file = 'data/early_cult_res_list_4.RDS')

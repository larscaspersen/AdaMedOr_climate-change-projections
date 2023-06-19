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









#fixier Tc, slope, theta_star, tau
#fit for all cultivars together the parameters Tu, Tb, theta_c, Tf, pie_c
#fit for cultivars seperately the parameters yc, zc, s1

almond_sub %>% 
  group_by(location, cultivar) %>% 
  summarise(n = n())


almond_sub %>% 
  mutate(cult_no = as.factor(as.numeric(as.factor(cultivar)))) %>% 
  ggplot(aes(x = cult_no, y = doy_begin, fill = cultivar)) +
  geom_boxplot() + 
  theme_bw() + 
  theme(legend.position = 'none') +
  facet_grid(~location)


bloom_category <- data.frame(cult_no = c(c(1, 3, 7, 11, 16, 19, 32, 33, 34, 37, 38),
                                         c(29:27, 25, 21, 20, 18, 12, 10, 4, 6, 2, 22),
                                         c(8, 9, 13:15, 17, 23, 14, 24, 26, 30, 31, 35, 36, 5)),
                             bloom_cat = c(rep('early', 11), 
                                           rep('mid', 13),
                                           rep('late', 15)))


almond_sub %>% 
  mutate(cult_no = as.factor(as.numeric(as.factor(cultivar)))) %>% 
  merge.data.frame(bloom_category, by = 'cult_no', all.x = TRUE) %>% 
  ggplot(aes(x = cult_no, y = doy_begin, fill = cultivar)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(legend.position = 'none') +
  facet_grid(~bloom_cat, scales = 'free_x')
#maybe use k-means cluster method to get better estimates. can use my estimates as a starting point



#bloomJDays is now a dataframe with the column pheno
#seasonlist elements must be in the same order as bloomJDays elements (also within the elements)
evaluation_function_meigo_nonlinear_combined <- function(x, 
                                                         modelfn,
                                                         bloomJDays,
                                                         SeasonList,
                                                         n_cult = 3,
                                                         na_penalty = 365,
                                                         return_bloom_days = FALSE){
  
  #innput:
  #         x is the parameters in meigo
  #         modelfn is the function used to calculate the bloomdays
  #         SeasonList contains the weather data for the different years
  #         na_penalty is the value used if the model failed to predict any day with the current set of parameters for a particular year
  
  #output: inequality constraints g
  #        model performance value F
  
  
  
  #instead of A0, A1, E0 and E1 we have now theta*, theta_c, Tau(thetha*) and pie_c
  #--> we need to solve now a non-linear system to calculate A0, A1, E0 and E1 based on the parameters
  #    ('yc', 'zc', 's1', 'Tu', 'theta*', 'theta_c', 'Tau(thetha*)', 'pie_c', 'Tf', 'Tc', 'Tb',  'slope')
  #x<- c(40,   190,   0.5,  25,   279,      287,       28,             26,       4,   36,    4,    1.60)
  
  params<-numeric(4)
  
  params[1] <- x[5+((n_cult -1) * 3)]   #theta*
  params[2] <- x[6+((n_cult -1) * 3)]    #theta_c
  params[3] <- x[7+((n_cult -1) * 3)]    #Tau(thetha*)
  params[4] <- x[8+((n_cult -1) * 3)]     #pi_c
  
  
  output<-nleqslv::nleqslv(c(500, 15000), LarsChill:::solve_nle, jac=NULL, params, xscalm="auto", method="Newton",
                           control=list(trace=0,allowSingular=TRUE))
  
  
  #This is a numerical method which can produce non-convergence. Check this
  if (output$termcd >= 3){
    #if the nle algorithm has stalled just discard this solution
    E0<-NA; E1<-NA; A0<-NA; A1<-NA
    return(list(F=10^6, g=rep(10^6,5)))
    
    #You would add here a flag to let your optimization procedure know
    #That this solution should be ignored by lack of convergence
    
  } else {
    
    E0 <- output$x[1]
    E1 <- output$x[2]
    
    #A1 and A0 can be calculated through Equations 36 and 37
    
    q=1/params[1]-1/params[2]
    
    A1 <- -exp(E1/params[1])/params[3]*log(1-exp((E0-E1)*q))
    A0 <- A1*exp((E0-E1)/params[2])
  }
  
  
  #change the name of the parameters, dont know if necessary
  par <- x
  par[(5:8)+((n_cult -1) * 3)] <- c(E0, E1, A0, A1)
  
  #split the parameters and reconstruct them for the different cultivar
  
  
  
  #loop over the cultivars, calculate predicted days for each cultivar
  rss <- purrr::map_dbl(1:length(SeasonList), function(i){
    
    #extract the parameters
    par_cult <- par[c(i, 1+((n_cult -1))+i, 2+((n_cult -1)*2)+i, (length(par)-8):length(par))]
    #predict the bloom
    pred_bloom <- unlist(lapply(X = SeasonList[[i]], FUN = modelfn, par = par_cult))
    pred_bloom <- ifelse(is.na(pred_bloom), yes = na_penalty, no = pred_bloom)
    #calculate for each cultivar the rss
    rss <- sum((pred_bloom - bloomJDays[[i]]$pheno)^2)
    return(rss)
    
  })
  
  
  #calculate the model performance value
  F <- sum(rss)
  
  
  
  #####
  #inequality constraints
  #####
  
  #this is the vector containing the values for the inequality constraints
  #at first initialize the vector
  g <- rep(0,5)
  
  
  #equality constraints should be always stated before inequality constraints, according to meigo vignette
  #(but we dont have any in this case)
  
  
  #inequality constraints are reformulated as differences
  
  #Tu >= Tb
  g[1] <- par[length(par)-8] - par[length(par)-1]
  #Tx >= Tb
  g[2] <- par[length(par)-2] - par[length(par)-1]
  #Tc >= Tu
  g[3] <- par[length(par)-2] - par[length(par)-8]
  
  
  #the q10 value should be between 1.5 and 3.5
  #q10 formation = exp((10*E0)/(T2-T1))
  #q10 destruction = exp((10*E1)/(T2-T1))
  #T1 = 297
  #T2 = 279
  #parameters T1 and T2 chosen after Egea 2020
  #ranges for q10 are provided in c_L and c_U
  g[4] <- exp((10 * par[length(par)-7]) / (297 * 279))
  
  g[5] <- exp((10 * par[length(par)-6]) / (297 * 279))
  
  
  if(return_bloom_days == FALSE){
    #output
    return(list(F=F, g=g))
  } else{
    return(pred_bloom)
  }
}

ncult <- length(unique(almond_sub$cultivar))

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
  inter_save = 0,
  plot = 1)

set.seed(123456789)

evaluation_function_meigo_nonlinear_combined(x = x_0, modelfn = custom_PhenoFlex_GDHwrapper())

res_list <- MEIGO(problem = problem,
                  opts,
                  algorithm="ESS", 
                  modelfn = custom_PhenoFlex_GDHwrapper,
                  bloomJDays = pheno_cal_list,
                  SeasonList = season_cal_list,
                  n_cult = ncult)

#retrieve model performance and model prediction
performance_df <- prediction_df <- data.frame()


par_all <- res_list$xbest
for(i in 1:length(cultivars)){
  
  cult <- cultivars[i]
  
  par_cult <- par_all[c(i, (ncult*1)+(i), (ncult * 2) + (i), (length(par_all)-8):length(par_all))]
  
  
  pheno_cal_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_cal_list[[cult]])
  pheno_val_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_val_list[[cult]])
  
  prediction_df <- rbind.data.frame(prediction_df,
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

prediction_df %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0) 



#fitting round 2

x_0 <- res_list$xbest
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
  inter_save = 0,
  plot = 1)

set.seed(123456789)

res_list2 <- MEIGO(problem = problem,
                  opts,
                  algorithm="ESS", 
                  modelfn = custom_PhenoFlex_GDHwrapper,
                  bloomJDays = pheno_cal_list,
                  SeasonList = season_cal_list,
                  n_cult = ncult)



#retrieve model performance and model prediction
prediction_df2 <- data.frame()


par_all <- res_list2$xbest
for(i in 1:length(cultivars)){
  
  cult <- cultivars[i]
  
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


#--> maybe let it run for 2h?


#fitting round 3

x_0 <- res_list2$xbest

problem<-list(f="evaluation_function_meigo_nonlinear_combined",
              x_0 = x_0,
              x_L = x_L,
              x_U = x_U,
              c_L = c_L, 
              c_U = c_U)


#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 120, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0,
  plot = 1)

set.seed(123456789)

res_list3 <- MEIGO(problem = problem,
                   opts,
                   algorithm="ESS", 
                   modelfn = custom_PhenoFlex_GDHwrapper,
                   bloomJDays = pheno_cal_list,
                   SeasonList = season_cal_list,
                   n_cult = ncult)

prediction_df3 <- data.frame()


par_all <- res_list3$xbest
for(i in 1:length(cultivars)){
  
  cult <- cultivars[i]
  
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




#round 4
x_0 <- res_list3$xbest

problem<-list(f="evaluation_function_meigo_nonlinear_combined",
              x_0 = x_0,
              x_L = x_L,
              x_U = x_U,
              c_L = c_L, 
              c_U = c_U)


#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 60 * 4, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0,
  plot = 1)

set.seed(123456789)

res_list4 <- MEIGO(problem = problem,
                   opts,
                   algorithm="ESS", 
                   modelfn = custom_PhenoFlex_GDHwrapper,
                   bloomJDays = pheno_cal_list,
                   SeasonList = season_cal_list,
                   n_cult = ncult)

prediction_df4 <- data.frame()


par_all <- res_list4$xbest
for(i in 1:length(cultivars)){
  
  cult <- cultivars[i]
  
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

prediction_df4$error <- prediction_df4$pheno_pred - prediction_df4$pheno

prediction_df4 %>% 
  ggplot(aes(x = error)) +
  geom_histogram() +
  facet_wrap(~cultivar)

hist(prediction_df4$error)

#wir fixieren jetzt mal Tc, theta_star, pie_c, slope, Tb. 




evaluation_function_meigo_nonlinear_fixed <- function(x, 
                                                         modelfn,
                                                         bloomJDays,
                                                         SeasonList,
                                                         na_penalty = 365,
                                                         n_cult,
                                                         theta_star = NULL,
                                                         pie_c = NULL,
                                                         Tc = NULL,
                                                         Tb = NULL,
                                                         slope = NULL){
  

  #bring x to another variable
  x_supplied <- x
    
  x <- c(x_supplied[1:((n_cult*3)+1)], #repeated yv, zc, s1 and lastly single Tu
         theta_star, #fixed
         x_supplied[c(length(x_supplied)-2, length(x_supplied)-1)], #theta_c and tau
         pie_c, #fixed
         x_supplied[length(x_supplied)], #Tf
         Tc,
         Tb,
         slope) 


  params<-numeric(4)
  
  params[1] <- theta_star  #theta*
  params[2] <- x[6+((n_cult -1) * 3)]    #theta_c
  params[3] <- x[7+((n_cult -1) * 3)]    #Tau(thetha*)
  params[4] <- pie_c     #pi_c
  
  
  output<-nleqslv::nleqslv(c(500, 15000), solve_nle, jac=NULL, params, xscalm="auto", method="Newton",
                           control=list(trace=0,allowSingular=TRUE))
  
  
  #This is a numerical method which can produce non-convergence. Check this
  if (output$termcd >= 3){
    #if the nle algorithm has stalled just discard this solution
    E0<-NA; E1<-NA; A0<-NA; A1<-NA
    return(list(F=10^6, g=rep(10^6,5)))
    
    #You would add here a flag to let your optimization procedure know
    #That this solution should be ignored by lack of convergence
    
  } else {
    
    E0 <- output$x[1]
    E1 <- output$x[2]
    
    #A1 and A0 can be calculated through Equations 36 and 37
    
    q=1/params[1]-1/params[2]
    
    A1 <- -exp(E1/params[1])/params[3]*log(1-exp((E0-E1)*q))
    A0 <- A1*exp((E0-E1)/params[2])
  }
  
  
  #change the name of the parameters, dont know if necessary
  par <- x
  par[(5:8)+((n_cult -1) * 3)] <- c(E0, E1, A0, A1)
  
  
  
  #loop over the cultivars, calculate predicted days for each cultivar
  rss <- purrr::map_dbl(1:length(SeasonList), function(i){
    
    #extract the parameters
    par_cult <- par[c(i, 1+((n_cult -1))+i, 2+((n_cult -1)*2)+i, (length(par)-8):length(par))]
    #predict the bloom
    pred_bloom <- unlist(lapply(X = SeasonList[[i]], FUN = modelfn, par = par_cult))
    pred_bloom <- ifelse(is.na(pred_bloom), yes = na_penalty, no = pred_bloom)
    #calculate for each cultivar the rss
    rss <- sum((pred_bloom - bloomJDays[[i]]$pheno)^2)
    return(rss)
    
  })
  
  
  #calculate the model performance value
  F <- sum(rss)
  
  
  
  #####
  #inequality constraints
  #####
  
  #this is the vector containing the values for the inequality constraints
  #at first initialize the vector
  g <- rep(0,5)
  
  
  #equality constraints should be always stated before inequality constraints, according to meigo vignette
  #(but we dont have any in this case)
  
  
  #inequality constraints are reformulated as differences
  
  #Tu >= Tb
  g[1] <- par[length(par)-8] - par[length(par)-1]
  #Tx >= Tb
  g[2] <- par[length(par)-2] - par[length(par)-1]
  #Tc >= Tu
  g[3] <- par[length(par)-2] - par[length(par)-8]
  
  
  #the q10 value should be between 1.5 and 3.5
  #q10 formation = exp((10*E0)/(T2-T1))
  #q10 destruction = exp((10*E1)/(T2-T1))
  #T1 = 297
  #T2 = 279
  #parameters T1 and T2 chosen after Egea 2020
  #ranges for q10 are provided in c_L and c_U
  g[4] <- exp((10 * par[length(par)-7]) / (297 * 279))
  
  g[5] <- exp((10 * par[length(par)-6]) / (297 * 279))
  
  
  
  return(list(F=F, g=g))
  
}

evaluation_function_meigo_nonlinear_fixed(x = x_0, 
                                             modelfn = custom_PhenoFlex_GDHwrapper, 
                                             bloomJDays = pheno_cal_list,
                                             SeasonList = season_cal_list,
                                             n_cult = ncult,
                                             theta_star = res_list4$xbest[116],
                                             pie_c = res_list4$xbest[119],
                                             Tc = res_list4$xbest[121],
                                             Tb = res_list4$xbest[122],
                                             slope = res_list4$xbest[123])


#      ('yc', 'zc', 's1', 'Tu', 'theta_c', 'Tau(thetha*)', Tf')
x_0 <- res_list4$xbest[c(1:115, 117:118, 120)]
x_U <- c(rep(60,ncult),   rep(500,ncult),   rep(1.0,ncult),  30,  288,       48,             10)
x_L <- c(rep(05,ncult),   rep(50,ncult),   rep(0.1,ncult),  15,   285,       16,              2)
#some of eduardos parameters were outside of the range I originally expected

#limits for the inequality constraints
#         #gdh parameters   #q10 for E0 and E1
c_L <- c(  0,   0,   0,     1.5, 1.5)
c_U <- c(Inf, Inf, Inf,     3.5, 3.5)


problem<-list(f="evaluation_function_meigo_nonlinear_fixed",
              x_0 = x_0,
              x_L = x_L,
              x_U = x_U,
              c_L = c_L, 
              c_U = c_U)


#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 60 * 4, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0,
  plot = 1)

set.seed(123456789)

res_list5 <- MEIGO(problem = problem,
                   opts,
                   algorithm="ESS", 
                   modelfn = custom_PhenoFlex_GDHwrapper,
                   bloomJDays = pheno_cal_list,
                   SeasonList = season_cal_list,
                   n_cult = ncult,
                   theta_star = res_list4$xbest[116],
                   pie_c = res_list4$xbest[119],
                   Tc = res_list4$xbest[121],
                   Tb = res_list4$xbest[122],
                   slope = res_list4$xbest[123])


prediction_df5 <- data.frame()

theta_star = res_list4$xbest[116]
pie_c = res_list4$xbest[119]
Tc = res_list4$xbest[121]
Tb = res_list4$xbest[122]
slope = res_list4$xbest[123]

par_all <- res_list5$xbest
par_all <- c(par_all[1:((n_cult*3)+1)], #repeated yv, zc, s1 and lastly single Tu
             theta_star, #fixed
             par_all[c(length(x_supplied)-2, length(x_supplied)-1)], #theta_c and tau
             pie_c, #fixed
             par_all[length(x_supplied)], #Tf
             Tc,
             Tb,
             slope) 


for(i in 1:length(cultivars)){
  
  cult <- cultivars[i]
  
  par_cult <- par_all[c(i, (ncult*1)+(i), (ncult * 2) + (i), (length(par_all)-8):length(par_all))]
  
  
  pheno_cal_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_cal_list[[cult]])
  pheno_val_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_val_list[[cult]])
  
  prediction_df5 <- rbind.data.frame(prediction_df5,
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

prediction_df5 %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0) 

prediction_df5$error <- prediction_df5$pheno_pred - prediction_df5$pheno

prediction_df5 %>% 
  ggplot(aes(x = error)) +
  geom_histogram() +
  facet_wrap(~cultivar)

RMSEP(prediction_df5$pheno_pred, prediction_df5$pheno)
#error of 9 days ....

RPIQ(prediction_df5$pheno_pred, prediction_df5$pheno)
#but rpiq of 2, so it is not complete garbage

#fitting everything seems to be not working great, lets try fitting the three different groups
#--> this looks better than the output for the grouped fitting
#maybe have a run for each 

par_all <- res_list5$xbest
par_all <- c(par_all[1:((n_cult*3)+1)], #repeated yv, zc, s1 and lastly single Tu
             theta_star, #fixed
             par_all[c(length(x_supplied)-2, length(x_supplied)-1)], #theta_c and tau
             pie_c, #fixed
             par_all[length(x_supplied)], #Tf
             Tc,
             Tb,
             slope) 

evaluation_function_meigo_nonlinear_fixed

evaluation_function_meigo_nonlinear_minimal <- function(x, 
                                                        modelfn,
                                                        bloomJDays,
                                                        SeasonList,
                                                        theta_c,
                                                        theta_star,
                                                        tau,
                                                        pie_c,
                                                        Tf,
                                                        slope,
                                                        Tb,
                                                        Tu,
                                                        Tc,
                                                        na_penalty = 365){
  
  #innput:
  #         x is the parameters in meigo
  #         modelfn is the function used to calculate the bloomdays
  #         SeasonList contains the weather data for the different years
  #         na_penalty is the value used if the model failed to predict any day with the current set of parameters for a particular year
  
  #output: inequality constraints g
  #        model performance value F
  
  
  
  #instead of A0, A1, E0 and E1 we have now theta*, theta_c, Tau(thetha*) and pie_c
  #--> we need to solve now a non-linear system to calculate A0, A1, E0 and E1 based on the parameters
  #    ('yc', 'zc', 's1', 'Tu', 'theta*', 'theta_c', 'Tau(thetha*)', 'pie_c', 'Tf', 'Tc', 'Tb',  'slope')
  #x<- c(40,   190,   0.5,  25,   279,      287,       28,             26,       4,   36,    4,    1.60)
  
  x <- c(x, Tu, theta_star, theta_c, tau, pie_c, Tf, Tc, Tb, slope)
  
  params<-numeric(4)
  
  params[1] <- x[5]   #theta*
  params[2] <- x[6]    #theta_c
  params[3] <- x[7]    #Tau(thetha*)
  params[4] <- x[8]     #pi_c
  
  
  output<-nleqslv::nleqslv(c(500, 15000), solve_nle, jac=NULL, params, xscalm="auto", method="Newton",
                           control=list(trace=0,allowSingular=TRUE))
  
  
  #This is a numerical method which can produce non-convergence. Check this
  if (output$termcd >= 3){
    #if the nle algorithm has stalled just discard this solution
    E0<-NA; E1<-NA; A0<-NA; A1<-NA
    return(list(F=10^6, g=rep(10^6,5)))
    
    #You would add here a flag to let your optimization procedure know
    #That this solution should be ignored by lack of convergence
    
  } else {
    
    E0 <- output$x[1]
    E1 <- output$x[2]
    
    #A1 and A0 can be calculated through Equations 36 and 37
    
    q=1/params[1]-1/params[2]
    
    A1 <- -exp(E1/params[1])/params[3]*log(1-exp((E0-E1)*q))
    A0 <- A1*exp((E0-E1)/params[2])
  }
  
  
  #change the name of the parameters, dont know if necessary
  par <- x
  par[5:8] <- c(E0, E1, A0, A1)
  
  #calculate the predicted flower dates
  pred_bloom <- unlist(lapply(X = SeasonList, FUN = modelfn, par = par))
  
  #if the model returns no bloom day, then give penalty term instead
  pred_bloom <- ifelse(is.na(pred_bloom), yes = na_penalty, no = pred_bloom)
  
  #calculate the model performance value
  F <- sum((pred_bloom - bloomJDays)^2)
  
  
  
  #####
  #inequality constraints
  #####
  
  #this is the vector containing the values for the inequality constraints
  #at first initialize the vector
  g <- rep(0,5)
  
  
  #equality constraints should be always stated before inequality constraints, according to meigo vignette
  #(but we dont have any in this case)
  
  
  #inequality constraints are reformulated as differences
  
  #Tu >= Tb
  g[1] <- par[4] - par[11]
  #Tx >= Tb
  g[2] <- par[10] - par[11]
  #Tc >= Tu
  g[3] <- par[10] - par[4]
  
  
  #the q10 value should be between 1.5 and 3.5
  #q10 formation = exp((10*E0)/(T2-T1))
  #q10 destruction = exp((10*E1)/(T2-T1))
  #T1 = 297
  #T2 = 279
  #parameters T1 and T2 chosen after Egea 2020
  #ranges for q10 are provided in c_L and c_U
  g[4] <- exp((10 * par[5]) / (297 * 279))
  
  g[5] <- exp((10 * par[6]) / (297 * 279))
  
  
  
  return(list(F=F, g=g))
  
}


#      ('yc', 'zc', 's1', 'Tu', 'theta_c', 'Tau(thetha*)', Tf')
x_U <- c(60,   1000,   1.0)
x_L <- c(05,   50,    0.1)
#some of eduardos parameters were outside of the range I originally expected

#limits for the inequality constraints
#         #gdh parameters   #q10 for E0 and E1
c_L <- c(  0,   0,   0,     1.5, 1.5)
c_U <- c(Inf, Inf, Inf,     3.5, 3.5)


#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 10, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0)

res_list6 <- list()

set.seed(123456789)

for(i in 1:ncult){
  
  par_cult <- par_all[c(i, (ncult*1)+(i), (ncult * 2) + (i), (length(par_all)-8):length(par_all))]
  x_0 <- par_cult[1:3]
  
  problem<-list(f="evaluation_function_meigo_nonlinear_minimal",
                x_0 = x_0,
                x_L = x_L,
                x_U = x_U,
                c_L = c_L, 
                c_U = c_U)

  
  res_list6[[i]] <- MEIGO(problem = problem,
                     opts,
                     algorithm="ESS", 
                     modelfn = custom_PhenoFlex_GDHwrapper,
                     bloomJDays = pheno_cal_list[[i]]$pheno,
                     SeasonList = season_cal_list[[i]],
                     Tu = par_cult[4],
                     theta_star = par_cult[5],
                     theta_c = par_cult[6],
                     tau = par_cult[7],
                     pie_c = par_cult[8],
                     Tf = par_cult[9],
                     Tc = par_cult[10],
                     Tb = par_cult[11],
                     slope = par_cult[12])
}






#copied from the par all
Tu <- 20.4961953 
theta_star <- 280.0351477 
theta_c <- 286.2632764  
tau <- 28.5460031  
pie_c <- 25.1985135   
Tf <- 3.4404005  
Tc <- 39.7256368   
Tb <- 0.7611169   
slope <- 1.8122088

prediction_df6 <- data.frame()

purrr::map(res_list6, 'xbest')


for(i in 1:length(cultivars)){
  
  cult <- cultivars[i]
  par_cult <- c(res_list6[[i]]$xbest, Tu, theta_star, theta_c, tau, pie_c, Tf, Tc, Tb, slope)
  

  pheno_cal_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_cal_list[[cult]])
  pheno_val_pred <- return_predicted_days(convert_parameters(par_cult), 
                                          modelfn = custom_PhenoFlex_GDHwrapper, 
                                          SeasonList = season_val_list[[cult]])
  
  prediction_df6 <- rbind.data.frame(prediction_df6,
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

prediction_df6 %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0) 


prediction_df6 %>% 
  ggplot(aes(x = pheno, y = pheno_pred, col = data)) +
  geom_point(aes(shape = location)) +
  geom_abline(linetype = 'dashed', slope = 1, intercept = 0) +
  facet_wrap(~cultivar)

prediction_df6$error <- prediction_df6$pheno_pred - prediction_df6$pheno

prediction_df6 %>% 
  ggplot(aes(x = error)) +
  geom_histogram() +
  facet_wrap(~cultivar)

RMSEP(prediction_df6$pheno_pred, prediction_df6$pheno)
#error of 9 days ....

RPIQ(prediction_df6$pheno_pred, prediction_df6$pheno)

performance <- prediction_df6 %>% 
  group_by(cultivar, data) %>% 
  summarise(rmse = RMSEP(pheno_pred, pheno))


#maybe now do a run of joint fitting again?
#round 7



x_0 <- c(unlist(purrr::map(res_list6, function(x) x$xbest[1])),
         unlist(purrr::map(res_list6, function(x) x$xbest[2])),
         unlist(purrr::map(res_list6, function(x) x$xbest[3])),
         Tu, theta_star, theta_c, tau, pie_c, Tf, Tc, Tb, slope)
x_U <- c(rep(60,ncult),   rep(600,ncult),   rep(1.5,ncult),  30,   281,      287,       48,             50,       10,   40,    10,    5.00)
x_L <- c(rep(5,ncult),   rep(50,ncult),     rep(0.1,ncult),  15,   279,      286,       16,             24,        2,   20,     0,    1.2)

problem<-list(f="evaluation_function_meigo_nonlinear_combined",
              x_0 = x_0,
              x_L = x_L,
              x_U = x_U,
              c_L = c_L, 
              c_U = c_U)


#options for fitter
opts<-list(#maxeval = 1000,
  maxtime = 60 * 60 * 4, 
  local_solver = 'DHC', 
  local_bestx = 1,
  inter_save = 0)

set.seed(123456789)

res_list7 <- MEIGO(problem = problem,
                   opts,
                   algorithm="ESS", 
                   modelfn = custom_PhenoFlex_GDHwrapper,
                   bloomJDays = pheno_cal_list,
                   SeasonList = season_cal_list,
                   n_cult = ncult)

res_list7

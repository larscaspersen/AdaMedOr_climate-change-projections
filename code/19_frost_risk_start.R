+üpoiuztrewe321
321/-*+ of phenoflex

library(chillR)
library(tidyverse)
library(LarsChill)

#setwd('../fruittree_portfolio/')

source('code/utilities/load_save_fitting_results.R')
source('code/utilities/ensemble_prediction.R')

#read fitting data of cherries

cherry_fit <- list()

for(i in 1:10){
  cherry_fit[[i]] <- load_fitting_result(path = 'data/fitting/sweet_cherry/repeated_fitting/', prefix = paste0('repeat', i, '_'))
}

#load temperature data of cka and zaragoza
cka <- read.csv('data/weather_ready/cka_clean.csv')
zaragoza <- read.csv('data/weather_ready/zaragoza_clean.csv')

#make hourly
coord_zaragoza <- c(41.65, -0.88)
coord_cka <- c(50.61, 6.99)

cka_hourly <- stack_hourly_temps(cka, latitude = coord_cka[1])
zaragoza_hourly <- stack_hourly_temps(zaragoza, latitude = coord_zaragoza[1])

cka_hourly <- cka_hourly$hourtemps
zaragoza_hourly <- zaragoza_hourly$hourtemps

#make seasonal list
cka_season <- genSeasonList(cka_hourly, years = min(cka$Year):max(cka$Year))
zaragoza_season <- genSeasonList(zaragoza_hourly, years = min(zaragoza$Year):max(zaragoza$Year))

names(cka_season) <- min(cka$Year):max(cka$Year)
names(zaragoza_season) <- min(zaragoza$Year):max(zaragoza$Year)

# SeasonList <- list('Zaragoza' = zaragoza_season, 
#                    'Klein-Altendorf' = cka_season)


LarsChill::return_predicted_days()
pheno_ensemble_prediction

par <- cherry_fit[[1]]$Ambrunés$xbest
Tc <- 36
theta_star <- 279

par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])

custom_PhenoFlex_GDHwrapper(x = cka_season[[1]], par = par)


return_potential_frost_days_one_season <- function (x, par, constraints = FALSE){
  #x is one of the elements in season List
  #par are the parameters of the model
  
  #make explicit what which parameters is
  yc = par[1]
  zc = par[2]
  s1 = par[3]
  Tu = par[4]
  E0 = par[5]
  E1 = par[6]
  A0 = par[7]
  A1 = par[8]
  Tf = par[9]
  Tc = par[10] 
  Tb = par[11]
  slope = par[12]
  
  
  #in case the parameters do not make sense, return NA
  if(constraints){
    
    t1 <- Tu <= Tb
    t2 <- Tc <= Tb
    t3 <- exp((10 * E0)/(297 * 279)) < 1.5 | exp((10 * E0)/(297 * 279)) > 3.5
    t4 <- exp((10 * E1)/(297 * 279)) < 1.5 | exp((10 * E0)/(297 * 279)) > 3.5
    
    if(any(c(t1, t2, t3, t4))){
      return(NA)
    }
    
  }
  
  model_out <- chillR::PhenoFlex(temp = x$Temp, 
                    times = seq_along(x$Temp), 
                    yc = yc, 
                    zc = zc, 
                    s1 = s1, 
                    Tu = Tu, 
                    E0 = E0, 
                    E1 = E1, 
                    A0 = A0, 
                    A1 = A1, 
                    Tf = Tf, 
                    Tc = Tc, 
                    Tb = Tb, 
                    slope = slope, 
                    Imodel = 0L, 
                    basic_output = FALSE)
  
  heat_accum  <- model_out$z / zc
  
  #heat accumulation after flowering should be 1
  heat_accum[(test$bloomindex):length(heat_accum)] <- 1
  
  fun_Trcit <- function(h_acc, T1 = -25, T3 = -2.8){
    
    fun_T2 <- function(acc){
      return(-9.8109 * (acc^2) + 22.076 * acc - 15.013)
    }
    
    T_crit <- ifelse(h_acc < 0, 
                     yes = T1, 
                     no = ifelse(h_acc == 1, 
                                 yes = T3, 
                                 no = fun_T2(h_acc)))
    
    return(T_crit)
  }
  
  T_crit <- fun_Trcit(heat_accum)
  
  return(sum(T_crit >= x$Temp))
  
}

return_potential_frost_days_several_seasons <- function(par, SeasonList){
  
  frost_risk_days <- unlist(lapply(X = SeasonList, FUN = return_potential_frost_days_one_season, par = par))
  
  return(frost_risk_days)
}

plot(return_potential_frost_days_several_seasons(par = par, SeasonList = cka_season))
points(return_potential_frost_days_several_seasons(par = par, SeasonList = zaragoza_season), pch = 3)  


#but what if there is no predicted day? then we have a wider window of potential frost risk days. the end of the counting needs to be maybe a month after the flowering?  
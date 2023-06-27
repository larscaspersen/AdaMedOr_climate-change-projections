#save apricot cleanly

library(LarsChill)

source('code/utilities/load_save_fitting_results.R')

#load fitting results

#number of repititions
r <- 10

apricot_fit <- apricot_fit2 <- list()

for(i in 1:r){
  apricot_fit[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  apricot_fit2[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting_with_cieza/', prefix = paste0('repeat', i, '_'))
}


names(apricot_fit2[[1]])
#add the cultivars which are not in apricot fit2 from apricot fit
apricot_zaragoza_only  <- names(apricot_fit[[1]])[!names(apricot_fit[[1]]) %in% names(apricot_fit2[[1]])]

#add cultivars which I had not to refit
for(i in 1:r){
  for(cult in apricot_zaragoza_only){
    apricot_fit2[[i]][[cult]] <- apricot_fit[[i]][[cult]]
  }
  
}

dir.create('data/fitting/apricot/repeated_fitting_clean/')

for(i in 1:10){
  save_fitting_list(apricot_fit2[[i]], path = 'data/fitting/apricot/repeated_fitting_clean/', prefix = paste0('repeat', i, '_'))
}

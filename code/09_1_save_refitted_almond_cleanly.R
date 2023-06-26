
setwd('../fruittree_portfolio/')
#load almond data
almond_fit1 <- almond_fit2 <- almond_fit3 <- almond_fit <- list()
for(i in 1:10){
  
  almond_fit1[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting_different_bounds/',
                                           prefix = paste0('part_1_repeat', i, '_'))
  
  almond_fit2[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting_different_bounds/',
                                          prefix = paste0('part_2_repeat', i, '_'))
  
  
  almond_fit3[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting_different_bounds/',
                                          prefix = paste0('part_3_repeat', i, '_'))
  
  almond_fit[[i]] <- c(almond_fit1[[i]], almond_fit2[[i]], almond_fit3[[i]])
}


#save data
for(i in 1:10){
  save_fitting_list(almond_fit[[i]], path = 'data/fitting/almond/repeated_fitting_new_bounds/', prefix = paste0('repeat', i, '_'))
}

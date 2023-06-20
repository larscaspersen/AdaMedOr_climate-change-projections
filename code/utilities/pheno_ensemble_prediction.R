pheno_ensemble_prediction <- function(par_list, confidence, temp, return_se = TRUE){
  
  predicted <- purrr::map(par_list, function(x){
    par <- x$xbest
    par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
    
    return_predicted_days(convert_parameters(par), 
                          modelfn = custom_PhenoFlex_GDHwrapper, 
                          SeasonList =temp)
  }) %>% 
    do.call('cbind', .) %>% 
    as.matrix()
  weights <- confidence / sum(confidence)
  
  weighted_pred <- as.vector(predicted %*% weights)
  
  sd_pred <- predicted %>% 
    t() %>% 
    as.data.frame() %>% 
    set_colnames( 1:nrow(predicted)) %>% 
    reshape2::melt(id.vars = NULL) %>% 
    group_by(variable) %>% 
    summarise(sd = sd(value)) %>% 
    pull(sd)
  #scale rpiq_values
  
  
  if(return_se){
    return(list(predicted = weighted_pred, sd = sd_pred,
                individual_pred = predicted))
  } else{
    return(weighted_pred)
  }
  
}
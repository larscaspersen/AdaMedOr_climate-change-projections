setwd('../almond_pheno/')

library(LarsChill)
library(tidyverse)
library(chillR)

source('code/utilities/load_save_fitting_results.R')

#read fitted parameters
apricot_fit <- apricot_fit2 <- eplum_fit <- jplum_fit <- pistachio_fit <- cherry_fit <- almond_fit <- pear_fit <-  list()
r <- 10

for(i in 1:r){
  apricot_fit[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  apricot_fit2[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting_with_cieza/', prefix = paste0('repeat', i, '_'))
  eplum_fit[[i]] <- load_fitting_result('data/fitting/european_plum/', prefix = paste0('repeat', i, '_'))
  jplum_fit[[i]] <- load_fitting_result('data/fitting/japanese_plum/', prefix = paste0('repeat', i, '_'))
  pistachio_fit[[i]] <- load_fitting_result('data/fitting/pistachio/', prefix = paste0('repeat', i, '_'))
  cherry_fit[[i]] <- load_fitting_result('data/fitting/sweet_cherry/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  almond_fit[[i]] <- load_fitting_result('data/fitting/almond/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  pear_fit[[i]] <- load_fitting_result('data/fitting/pear/', prefix = paste0('repeat', i, '_'))
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


fit_list <- list('Apricot' = apricot_fit2,
                 'European Plum' = eplum_fit,
                 'Japanese Plum' = jplum_fit,
                 'Pistachio' = pistachio_fit,
                 'Sweet Cherry' = cherry_fit,
                 'Almond' = almond_fit,
                 'Pear' = pear_fit)


#need to load and recreate all the splits I did in the other scripts, because I forgot to save it


#need zaragoza and klein altendorf
cka <- read.csv('data/weather_ready/temp_cka_1958-2022.csv')
zaragoza <- read.csv('data/weather_raw/temp_zgz_1973-2022.csv')
sfax <- read.csv('data/weather_ready/sfax_1973-2021_fixed.csv')
cieza <- readxl::read_xlsx('data/weather_raw/Cieza(95_22)Tmax&Tmin.xlsx')
meknes <- read.csv('data/weather_ready/meknes-bassatine-fixed_1973-2022.csv')



#cieza needs to be modified a bit
cieza <- cieza %>% 
  dplyr::select(-Hours) %>% 
  mutate(Month = lubridate::month(Date),
         Day = lubridate::day(Date),
         DATE = as.character(Date),
         Year = as.integer(Year))

#make hourly
coord_zaragoza <- c(41.65, -0.88)
coord_cka <- c(50.61, 6.99)
coord_sfax <-c(34.75, 10.75)
coord_cieza <-c(38.24, -1.41)
coord_meknes <- c(33.88, -5.54)

cka_hourly <- stack_hourly_temps(cka, latitude = coord_cka[1])
zaragoza_hourly <- stack_hourly_temps(zaragoza, latitude = coord_zaragoza[1])
sfax_hourly <- stack_hourly_temps(sfax, latitude = coord_sfax[1])
cieza_hourly <- stack_hourly_temps(cieza, latitude = coord_cieza[1])
meknes_hourly <- stack_hourly_temps(meknes, latitude = coord_meknes[1])

#read splitted phenology data 
master_pheno_split <- read.csv('data/master_phenology_repeated_splits.csv')
unique(master_pheno_split$location)

hourly_weather_list <- list('Sfax' = sfax_hourly$hourtemps,
                            'Zaragoza' = zaragoza_hourly$hourtemps,
                            'Klein-Altendorf' = cka_hourly$hourtemps,
                            'Cieza' = cieza_hourly$hourtemps, 
                            'Meknes' = meknes_hourly$hourtemps)





par_df <- purrr::map(names(fit_list), function(spec){
  purrr::map(names(fit_list[[spec]][[1]]), function(cult){
    
    purrr::map(fit_list[[spec]], function(x) x[[cult]]$xbest) %>% 
      bind_cols() %>% 
      t() %>% 
      data.frame() %>% 
      mutate(species = spec,
             cultivar = cult, 
             run = 1:r
             )
    
  } ) %>% 
    bind_rows()
  
} ) %>% 
  bind_rows()


colnames(par_df)[1:10] <- c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope')
par_df <- par_df %>% 
  mutate(Tc = 36,
         theta_star = 279,
         cult = cultivar,
         cultivar = as.factor(run))

# for(cult in unique(par_df$cult)){
#   
#   loc <- master_pheno_split %>% 
#     filter(cultivar == cult) %>% 
#     pull(location) %>% 
#     unique()
#   
#   spec <- master_pheno_split %>% 
#     filter(cultivar == cult) %>% 
#     pull(species) %>% 
#     unique()
#   
#   h_weather <- hourly_weather_list[loc] %>% 
#     purrr::map(function(x) x[,c('DATE', 'Year', 'Temp', 'JDay')]) %>% 
#     bind_rows()
#   
#   p <- LarsChill::gen_combined_temp_response_plot(par_df[par_df$cult == cult,],
#                                              weather_list = hourly_weather_list[loc],
#                                              col_palette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
#                                                             "#CC79A7", "#999999", 'red', 'black'))
#   
#   fname <- paste0('figures/tempresponses/', spec, '_', cult, '_tempresponse.jpeg')
#   ggsave(plot = p, filename = fname, height = 15, width = 20, units = 'cm', device = 'jpeg')
# }


path1 <- matrix(c(0, 1, 2, 3, 0, 1, 2, 3), 4)
path2 <- matrix(c(0, 1, 2, 3, 4, 5, 6, 7), 4)
path3 <- matrix(c(0:3, 1, 4, 7, 2), 4)
path4 <- matrix(c(0:3, 1, 4, 7, 8), 4)

plot(path1, ylim = c(0, 10))
lines(x = path2)
lines(x = path3)
lines(x = path4)

SimilarityMeasures::DTW(path1, path2)
SimilarityMeasures::DTW(path1, path3)
SimilarityMeasures::DTW(path1, path4)
#could use the DTW function to calculate differences among the chill and heat response


#calculate dtw for each parameter combination within a cultivar
#compare differences between cultivars by calculating difference among each combination

temp_response_df <- data.frame()

par <- convert_parameters(unlist(par_df[1, c('yc', 'zc', 's1', 'Tu', 'theta_star', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tc', 'Tb', 'slope')]))


temp_df1 <- get_temp_response_df(par, temp_values = seq(-5, 50, by = 0.1))
par <- convert_parameters(unlist(par_df[2, c('yc', 'zc', 's1', 'Tu', 'theta_star', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tc', 'Tb', 'slope')]))

temp_df2 <- get_temp_response_df(par, temp_values = seq(-5, 50, by = 0.1))


par <- convert_parameters(unlist(par_df[11, c('yc', 'zc', 's1', 'Tu', 'theta_star', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tc', 'Tb', 'slope')]))

temp_df11 <- get_temp_response_df(par, temp_values = seq(-5, 50, by = 0.1))

SimilarityMeasures::DTW(as.matrix(temp_df1[c('Temperature', 'Chill_response')]),
                        as.matrix(temp_df2[c('Temperature', 'Chill_response')]))

plot(temp_df1[c('Temperature', 'Chill_response')])
points(temp_df2[c('Temperature', 'Chill_response')])
points(temp_df11[c('Temperature', 'Chill_response')])


SimilarityMeasures::DTW(as.matrix(temp_df1[c('Temperature', 'Chill_response')]),
                        as.matrix(temp_df11[c('Temperature', 'Chill_response')]))

test <- temp_df2
test$Chill_response  <- test$Chill_response * 2

SimilarityMeasures::DTW(as.matrix(temp_df1[c('Temperature', 'Chill_response')]),
                        as.matrix(temp_df2[c('Temperature', 'Chill_response')]), pointSpacing = -1)

SimilarityMeasures::DTW(as.matrix(temp_df2[c('Temperature', 'Chill_response')]),
                        as.matrix(test[c('Temperature', 'Chill_response')]), pointSpacing = -1)

#I can scale everything between 0 and 1 if I do not care too much about the height

##--> get calulcate the curves differences, all of them re-scaled between 0


#maybe restrict it for now to apricots
par_apricot <- par_df %>% 
  filter(species == 'Apricot')


start_temp_response <- Sys.time()
#calculate difference in heat and chill response for each of them
#calculate chill and heat responses first
temp_res_list <- purrr::map(1:nrow(par_apricot), function(i){
  get_temp_response_df(convert_parameters(unlist(par_df[i, c('yc', 'zc', 's1', 'Tu', 'theta_star', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tc', 'Tb', 'slope')])),
                       temp_values = seq(-5, 50, by = 0.1))
})
end_temp_response <- Sys.time()

#get all different combinations without duplicates or self comparison
comb <- t(combn(1:nrow(par_apricot), 2))


#furthermore, only keep 

start_similarity <- Sys.time()
simil_chill <- purrr::map2(comb[,1], comb[,2], function(x,y){
  m1 <- matrix(c(temp_res_list[[x]]$Temperature,
         temp_res_list[[x]]$Chill_response / max(temp_res_list[[x]]$Chill_response)), 
         ncol = 2, 
         nrow = nrow(temp_res_list[[x]]))
  
  m2 <- matrix(c(temp_res_list[[y]]$Temperature,
                 temp_res_list[[y]]$Chill_response / max(temp_res_list[[y]]$Chill_response)), 
               ncol = 2, 
               nrow = nrow(temp_res_list[[x]]))
  
  return(SimilarityMeasures::DTW(m1, m2))
})
end_similarity <- Sys.time()


simil_chill
#add the observations to the comb thing

#comb[,3] <- unlist(simil_chill)
comb <- data.frame(comb, similarity = unlist(simil_chill))
simil_df <- purrr::map2(comb$X1, comb$X2, function(x,y){
  
  return(data.frame(
    cult1 = par_df$cult[x],
    run1 = par_df$run[x],
    cult2 = par_df$cult[y],
    run2 = par_df$run[y]
  ))
  
}) %>% 
  bind_rows() %>% 
  mutate(similarity_chill = comb$similarity)

simil_df %>% 
  filter(cult1 == cult2) %>% 
  ggplot(aes( x= cult1, y = similarity_chill)) +
  geom_boxplot()


anova(lm(similarity_chill ~ cult1, data = simil_df[simil_df$cult1 == simil_df$cult2,]))
#similarity is affected by cultivars, they differ, which ones do?


model=lm(similarity_chill ~ cult1, data = simil_df[simil_df$cult1 == simil_df$cult2,])
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'cult1', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")


cld <- multcompView::multcompLetters4(ANOVA, TUKEY)

# table with factors and 3rd quantile
Tk <- simil_df %>% 
  filter(cult1 == cult2) %>% 
  group_by(cult1) %>%
  summarise(mean=mean(similarity_chill), quant = quantile(similarity_chill, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$cult1)
Tk$cld <- cld$Letters

simil_df %>% 
  filter(cult1 == cult2) %>% 
  ggplot(aes( x= cult1, y = similarity_chill)) +
  geom_boxplot() +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = Tk, aes(x = cult1, y = quant, label = cld), size = 3, vjust=-1, hjust =-1)
#--> within simularity


#but I also calculated the similarity of one cultivar to the next


hc <- hclust(dist(simil_df[simil_df$cult1 == simil_df$cult2, ]), "ave")
plot(hc)


# A panel of colors to draw each group with the same color :
box1

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( box1$stats[nrow(box1$stats),] )
box1$

#Add the labels
text( c(1:nlevels(data$treatment)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] 


plot(temp_df1[c('Temperature', 'Chill_response')], type = 'l')
plot(diff(temp_df1$Chill_response)/diff(temp_df1$Temperature), type = 'l')

plot(x = temp_df1$Temperature, y = smooth(temp_df1$Chill_response), type = 'l')
plot(diff(smooth(temp_df1$Chill_response))/diff(temp_df1$Temperature), type = 'l')

plot(temp_df2[c('Temperature', 'Chill_response')], type = 'l')
plot(diff(temp_df2$Chill_response)/diff(temp_df2$Temperature), type = 'l')


#--> the function is very sensitive to height differences
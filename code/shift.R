#calculate shift in bloom dates compared to current predictions

library(tidyverse)

pheno_current <-  read.csv('data/projected_bloomdates_ensemble_observed_weather.csv') %>% 
  mutate(species = tolower(species)) 

pheno_hist <- read.csv('data/projected_bloomdates_ensemble_historic_scenarios.csv')  %>% 
  mutate(species = tolower(species)) 

pheno_future <- read.csv('data/projected_bloomdates_ensemble.csv') %>% 
  tidyr::separate(species_cultivar, into = c('species', 'cultivar'), sep = '_' ) %>% 
  mutate(species = tolower(species)) 

#calculate for each species / cultivar / location the boxplot whiskers, the quantiles, median 
#--> calculate change in these

#do colg


# test <- pheno_hist %>% 
#   dplyr::select(-flowering_type) %>% 
#   rbind(pheno_future) %>% 
#   split(f = ~ cultivar + species + location + ssp + scenario_year) %>% 
#   purrr::discard(~ nrow(.x) == 0) %>% 
#   purrr::map_dbl(function(x) nortest::lillie.test(x$pheno_predicted)$p.value)

#data is definitely not normal distributed

#--> wilcox test because not normally distributed

hist_list <-  pheno_hist %>% 
  split(f = ~ species + cultivar + location) %>% 
  purrr::discard(~ nrow(.x) == 0)

future_list <- pheno_future %>% 
  split(f = ~ cultivar + species + location + ssp + scenario_year + gcm) %>% 
  purrr::discard(~ nrow(.x) == 0)






#compare elements
perform_wilcox_test <-  function(hist_name, hist_list, future_list){
  
  special_case <- FALSE
  
  name1 <- hist_name
  #name1 <- names(hist_list)[42]
  
  splitted <- stringr::str_split(string = name1, pattern = '\\.') %>% 
    purrr::map(tolower)
  
  #case that a cultivar name contains .
  if(length(splitted[[1]]) == 4){
    splitted[[1]][2] <- paste(splitted[[1]][2], splitted[[1]][3], sep = '.')
    splitted[[1]][3] <- splitted[[1]][4]
    
    splitted[[1]] <- splitted[[1]][1:3]
    
    special_case <- TRUE
  }
  
  #if length splitted is larger than 3 then I need to correct something
  
  
  #match the three search terms individually
  
  if(special_case){
    split_fut <- stringr::str_split(names(future_list), '\\.') %>% 
      purrr::discard(~length(.x) == 5)
    
    #repair names
    split_fut <- split_fut %>% 
      purrr::map(function(x){
        x[2] <- paste(x[1], x[2], sep = '.')
        x <- x[2:length(x)]
        return(x)
      })
    
  } else {
    split_fut <- stringr::str_split(names(future_list), '\\.')
  }
  
  spec_lgl <- purrr::map_lgl(split_fut, function(x) splitted[[1]][1] %in% tolower(x))
  cult_lgl <- purrr::map_lgl(split_fut, function(x) splitted[[1]][2] %in% tolower(x))
  loc_lgl <- purrr::map_lgl(split_fut, function(x) splitted[[1]][3] %in% tolower(x))
  
  match_i <- which(spec_lgl & cult_lgl & loc_lgl)
  
  out <- purrr::map_dbl(future_list[match_i], function(x) wilcox.test(x = hist_list[[1]]$pheno_predicted, y = x$pheno_predicted)$p.value) %>% 
    bind_rows(.id = 'id') %>% 
    reshape2::melt(id.vars = NULL)
  
  #adjust 
  out$p_adjusted <- p.adjust(out$value, method = 'BH')
  
  return(out)

}

out <- purrr::map(names(hist_list), function(x) perform_wilcox_test(x, hist_list, future_list), .progress = TRUE)

out_clean <- out %>% 
  bind_rows() 

#split by ., then do some repair

out_clean <- out_clean$variable %>% 
  stringr::str_split('\\.') %>% 
  purrr::map(function(x){
    
    if(length(x) == 6){
      return(data.frame(cultivar = x[1],
                        species = x[2],
                        location = x[3],
                        ssp = x[4],
                        year = x[5],
                        gcm = x[6]))
    } else if(length(x) == 7){
      return(data.frame(cultivar = paste(x[1],x[2], sep = '.'),
                        species = x[3],
                        location = x[4],
                        ssp = x[5],
                        year = x[6],
                        gcm = x[7]))
    } else {
      print('warning, wrong vector length')
    }
  }) %>% 
  bind_rows() %>% 
  mutate(p_adjusted = out_clean$p_adjusted)


out_clean <- out_clean %>% 
  mutate(sign_different = p_adjusted < 0.05)



#now add median shifts in the bloom date to the data.frame

pheno_hist_sum <- pheno_hist %>% 
  group_by(species, cultivar, location) %>% 
  summarise(med_current = median(pheno_predicted))

pheno_future_sum <- pheno_future %>% 
  group_by(species, cultivar, location, ssp, scenario_year, gcm) %>% 
  summarise(med_future = median(pheno_predicted)) %>% 
  merge.data.frame(pheno_hist_sum, by = c('species', 'cultivar', 'location')) %>% 
  mutate(shift = med_future - med_current,
         cultivar = tolower(cultivar),
         species = tolower(species),
         location = tolower(location),
         year = scenario_year) 


test <- out_clean %>% 
  mutate(cultivar = tolower(cultivar),
         species = tolower(species),
         location = tolower(location)) %>% 
  merge.data.frame(pheno_future_sum, by = c('species', 'cultivar', 'location', 'ssp', 'year', 'gcm')) 
  


#indicate if median shift is inside the target range
#example of sudoku approach

test %>% 
  mutate(species = stringr::str_to_title(species),
         location = stringr::str_to_title(location),
         species_label = recode(species, 
                                `European Plum` = 'a',
                                `Japanese Plum`  = 'b',
                                Pistachio = 'c'),
         species_label = factor(species_label, levels = c('Almond', 'Apple', 'Apricot', 'a', 'b', 'Pear', 'c', 'Sweet Cherry')),
         cultivar = stringr::str_to_title(cultivar)) %>% 
  filter(year == '2050') %>% 
  ggplot(aes(x = shift, y = cultivar, col = ssp)) +
  geom_point(aes(alpha = sign_different)) +
  facet_grid(species_label~location, scales = 'free_y', space = 'free_y')+
  scale_y_discrete(limits=rev) +
  theme_bw()
ggsave('figures/shift_bloomdate_2050.jpeg', height = 20, width = 25, unit = 'cm', device = 'jpeg')

test %>% 
  filter(year == '2085') %>% 
  ggplot(aes(x = shift, y = cultivar, col = ssp)) +
  geom_point(aes(alpha = sign_different)) +
  facet_grid(species~location, scales = 'free_y', space = 'free_y')


#now calculate the median shift
unique(pheno_future$gcm)

#some cultivars have . in their name and that makes everything messy

chillR::gens


which(grepl('Stark', names(hist_list)))
names(hist_list)






#idea was to plot changes in more than median, but idea failed because the changes cannot
#be put together as one boxplot anymore

# pheno_hist_sum <- pheno_hist %>% 
#   group_by(species, cultivar, location) %>% 
#   summarise(iqr = IQR(pheno_predicted),
#             q_25 = quantile(pheno_predicted, probs = 0.25),
#             median = median(pheno_predicted),
#             q_75 = quantile(pheno_predicted, probs = 0.75),
#             lower_whisker = max(q_25 - (1.5*iqr), min(pheno_predicted)),
#             upper_whisker = min(q_75 + (1.5*iqr), max(pheno_predicted)))
# 
# 
# 
# pheno_future_sum <- pheno_future %>% 
#   tidyr::separate(species_cultivar, into = c('species', 'cultivar'), sep = '_' ) %>% 
#   mutate(species = tolower(species)) %>% 
#   group_by(species, cultivar, location, ssp, scenario_year) %>% 
#   summarise(iqr = IQR(pheno_predicted),
#             q_25 = quantile(pheno_predicted, probs = 0.25),
#             median = median(pheno_predicted),
#             q_75 = quantile(pheno_predicted, probs = 0.75),
#             lower_whisker = max(q_25 - (1.5*iqr), min(pheno_predicted)),
#             upper_whisker = min(q_75 + (1.5*iqr), max(pheno_predicted)))
# 
# pheno_future_sum %>% 
#   merge.data.frame(pheno_hist_sum, by = c('species', 'cultivar', 'location')) %>% 
#   mutate(q_25 = q_25.y - q_25.x,
#          median = median.y - median.x,
#          q_75 = q_75.y - q_75.x,
#          lower_whisker = lower_whisker.y - lower_whisker.x,
#          upper_whisker = upper_whisker.y - upper_whisker.y)
# 
# 
# ggplot(pheno_hist_sum, aes(y = cultivar)) +
#   geom_boxplot(stat = 'identity', 
#                aes(xlower  = q_25,
#                    xupper  = q_75,
#                    xmiddle = median,
#                    xmin   = lower_whisker,
#                    xmax   = upper_whisker)) +
#   facet_grid(species ~ location, scales = 'free_y',space = 'free_y')






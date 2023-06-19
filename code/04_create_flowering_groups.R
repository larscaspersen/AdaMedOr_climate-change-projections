library(tidyverse)

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



quan_df <- almond_sub %>% 
  group_by(cultivar) %>% 
  summarise(up = quantile(doy_begin, probs = 0.25),
            mid = median(doy_begin),
            low = quantile(doy_begin, probs = 0.75))



summed_dif <- abs(rep(quan_df$up, nrow(quan_df)) - rep(quan_df$up, each = nrow(quan_df))) + 
abs(rep(quan_df$mid, nrow(quan_df)) - rep(quan_df$mid, each = nrow(quan_df))) +  
abs(rep(quan_df$low, nrow(quan_df)) - rep(quan_df$low, each = nrow(quan_df)))

distance_matrix <- matrix(summed_dif, nrow = nrow(quan_df), ncol = nrow(quan_df), byrow = TRUE)

dist(USArrests)
distance_matrix_v2 <- dist(quan_df[,2:4], diag = TRUE, upper = TRUE)
names(distance_matrix_v2) <- make.names(quan_df$cultivar)

error.freq.hclust <- hclust(distance_matrix_v2, method = "ward.D2")
plot(error.freq.hclust, hang = -1)
rect.hclust(error.freq.hclust, 3)

quan_df$cluster <- cutree(error.freq.hclust, 3)

merge.data.frame(almond_sub, quan_df, by = 'cultivar') %>% 
  ggplot(aes(x = cultivar, y = doy_begin)) + 
  geom_boxplot() +
  facet_grid(~cluster, scales = 'free_x') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(tidyverse)
library(chillR)
library(LarsChill)

master <- read.csv('data/master_phenology_repeated_splits.csv') %>% 
  filter(repetition == 1) %>% 
  group_by(species, cultivar) %>% 
  summarise(location = unique(location))

more_loc <- master %>% 
  group_by(species, cultivar) %>% 
  summarise(nloc = n()) %>% 
  filter(nloc > 1)

#for those cultivars, have a second (or third) column with location name
master$location2 <- NA
master$location3 <- NA

for(i in 1:nrow(more_loc)){
  
  #i <- 1
  row_i <- which(master$species == more_loc$species[i] & master$cultivar == more_loc$cultivar[[i]])
  master$location2[row_i[1]] <- master$location[row_i[2]]
  
  if(length(row_i) == 3){
    master$location3[row_i[1]] <- master$location[row_i[3]]
    
    master <- master[-row_i[2:3],]
  } else {
    master <- master[-row_i[2], ]
  }
}


apricot_fit <- apple_fit <- eplum_fit <- jplum_fit <- pistachio_fit <- cherry_fit <- almond_fit <- pear_fit <-  list()

for(i in 1:10){
  apricot_fit[[i]] <- load_fitting_result('data/fitting/apricot/repeated_fitting_clean/', prefix = paste0('repeat', i, '_'))
  apple_fit[[i]]  <- load_fitting_result('data/fitting/apple/', prefix = paste0('repeat', i, '_'))
  eplum_fit[[i]] <- load_fitting_result('data/fitting/european_plum/', prefix = paste0('repeat', i, '_'))
  jplum_fit[[i]] <- load_fitting_result('data/fitting/japanese_plum/', prefix = paste0('repeat', i, '_'))
  pistachio_fit[[i]] <- load_fitting_result('data/fitting/pistachio/', prefix = paste0('repeat', i, '_'))
  cherry_fit[[i]] <- load_fitting_result('data/fitting/sweet_cherry/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  #almond_fit[[i]]  <- load_fitting_result(path = 'data/fitting/almond/repeated_fitting/', prefix = paste0('repeat', i, '_'))
  #almond_fit[[i]] <- load_fitting_result(path = 'data/fitting/almond/repeated_fitting_new_bounds', prefix = paste0('repeat', i, '_'))
  almond_fit[[i]]  <- load_fitting_result(path = 'data/fitting/almond/repeated_fitting_santomera_v2_cleanly_saved/', prefix = paste0('repeat', i, '_'))
  pear_fit[[i]] <- load_fitting_result('data/fitting/pear/', prefix = paste0('repeat', i, '_'))
}



fit_list_combined <- list('Apricot' = apricot_fit,
                          'Apple' = apple_fit,
                          'European Plum' = eplum_fit,
                          'Japanese Plum' = jplum_fit,
                          'Pistachio' = pistachio_fit,
                          'Sweet Cherry' = cherry_fit,
                          'Almond' = almond_fit,
                          'Pear' = pear_fit)

spec <- fit_list_combined[[1]]
rep <- spec[[1]]
cult <- rep[[1]]
spec

par_out <- purrr::map(fit_list_combined, function(spec){
  
  purrr::map(spec, function(rep){
    
    purrr::map(rep, 'xbest') %>% 
      bind_cols() %>% 
      mutate(par = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope')) %>% 
      pivot_longer(cols = -par, names_to = 'cultivar')
    
  }) %>% 
    bind_rows(.id = 'repetition')
}) %>% 
  bind_rows(.id = 'species') %>% 
  pivot_wider(id_cols = c('species', 'repetition', 'cultivar'), names_from = par)


rownames(par_out) <- paste0(par_out$species, '_', par_out$cultivar, '_', par_out$repetition)

idcols <- par_out %>% 
  select(species, cultivar, repetition)

par_analysis <- par_out %>% 
  select(-c('species', 'cultivar', 'repetition')) %>% 
  data.frame() %>% 
  scale() #standardize



library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

distance <- get_dist(par_analysis)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


k2 <- kmeans(par_analysis, centers = 2, nstart = 25) 
k3 <- kmeans(par_analysis, centers = 3, nstart = 25)
k4 <- kmeans(par_analysis, centers = 4, nstart = 25)
k5 <- kmeans(par_analysis, centers = 5, nstart = 25)
k6 <- kmeans(par_analysis, centers = 6, nstart = 25)
k7 <- kmeans(par_analysis, centers = 7, nstart = 25)
k8 <- kmeans(par_analysis, centers = 8, nstart = 25)
k9 <- kmeans(par_analysis, centers = 9, nstart = 25)
k10 <- kmeans(par_analysis, centers = 10, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = par_analysis) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = par_analysis) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = par_analysis) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = par_analysis) + ggtitle("k = 5")


wss <- function(k) {
  kmeans(par_analysis, k, nstart = 25 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:20

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)

fviz_nbclust(par_analysis, kmeans, method = "wss")
fviz_nbclust(par_analysis, kmeans, method = "silhouette")
#only two clusters.....

set.seed(123)
gap_stat <- clusGap(par_analysis, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

par_out$cluster <- k7$cluster


par_out %>% 
  group_by(cluster) %>% 
  summarise(nspec = length(unique(species)))





#pca

set.seed(111)
ind <- sample(2, nrow(par_out),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- par_out[ind==1,]
testing <- par_out[ind==2,]


pc <- prcomp(training %>% select(-cluster, -species, -cultivar, -repetition),
             center = TRUE,
             scale. = TRUE)
pc$sdev * pc$sdev
pc$rotation

coord <- factoextra::get_pca_var(pc)

library("corrplot")
corrplot(coord$cos2, is.corr=FALSE)


attributes(pc)

summary(pc)



library(ggfortify)
autoplot(pc)

autoplot(pc, data = training, colour = 'species', 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

autoplot(pc, data = training, colour = 'species', 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

df <- cbind(pc$x[,1:2], training[,'species']) %>% as.data.frame()
df$PC1 <- as.numeric(df$PC1) / (pc$sdev[1] * sqrt(nrow(training)))
df$PC2 <- as.numeric(df$PC2) / (pc$sdev[2] * sqrt(nrow(training)))
#df$PC3 <- as.numeric(df$PC3) / (pc$sdev[3] * sqrt(nrow(training)))
df$species <- as.factor(df$species)
df <- data.frame(df)

ggplot(df, aes(PC1, PC2, colour = species)) +
  geom_point() +
  # stat_ellipse(size = 1, data = df[df$species == "Almond" | 
  #                                    df$species == "Pear" | 
  #                                    df$species == "Pistachio",]) 
stat_ellipse(size = 1) 

pc$

#model only knows how to detect pear 



trg <- predict(pc, training)
trg <- data.frame(trg, training['species'])
tst <- predict(pc, testing)
tst <- data.frame(tst, testing['species'])

library(nnet)
#trg$species <- relevel(trg$species, ref = "Almond")
mymodel <- multinom(species~PC1+PC2+PC3, data = trg)
summary(mymodel)
multinom(formula = species ~ PC1 + PC2 + PC3, data = trg)

p <- predict(mymodel, trg)
tab <- table(p, trg$species)
tab

#--> it seems that the model can only differntiate between almond + pistachio and rest
#so rather the location than the species


p1 <- predict(mymodel, tst)
tab1 <- table(p1, tst$species)
tab1
1 - sum(diag(tab1))/sum(tab1)

###hierachical clustering

dist_mat <- dist(par_analysis, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 7)

plot(hclust_avg)
rect.hclust(hclust_avg , k = 7, border = 2:6)
abline(h = 7, col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj,k = 7)
plot(avg_col_dend)

par_outcl <- mutate(par_out, h_cluster = cut_avg)
count(par_outcl,h_cluster)
#alles landet in cluster 1 oder 2

par_outcl %>% 
  group_by(species, h_cluster) %>% 
  summarise(n = n())

#it is again, almond vs rest

#what about locations?????

count_loc <- par_outcl %>% 
  merge(master, by = c('species', 'cultivar')) %>% 
  group_by(location, h_cluster) %>% 
  summarise(n = n()) %>% 
  drop_na()

count_loc2 <- par_outcl %>% 
  merge(master, by = c('species', 'cultivar')) %>% 
  group_by(location2, h_cluster) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  rename(location = 'location2')

count_loc3 <- par_outcl %>% 
  merge(master, by = c('species', 'cultivar')) %>% 
  group_by(location3, h_cluster) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  rename(location = 'location3')

final_count_hclust <- count_loc %>% 
  rbind(count_loc2) %>% 
  rbind(count_loc3) %>% 
  group_by(h_cluster, location) %>% 
  summarise(n = sum(n))

#get number of observations per location
nobs <- final_count_hclust %>% 
  group_by(location) %>% 
  summarise(nobs = sum(n))



final_count_hclust %>% 
  merge(nobs, by = 'location') %>% 
  mutate(nrel = n / nobs) %>% 
  ggplot(aes(x = as.factor(h_cluster), y = location)) +
  geom_tile(aes(fill = nrel)) + 
  xlab('Cluster') + 
  ylab('Location') +
  scale_fill_gradient(name = 'Percent\nof cases',
                      low = 'white', 
                      high = 'firebrick',
                      na.value = 'white')
ggsave('figures/cluster_hierachical_location.jpeg', width = 15, height = 10, units = 'cm', device = 'jpeg')


nobs_spec <- par_outcl %>% 
  group_by(species) %>% 
  summarise(nobs = n())

par_outcl %>% 
  group_by(species, h_cluster) %>% 
  summarise(n = n()) %>% 
  merge(nobs_spec, by = c('species')) %>% 
  mutate(nrel = n /  nobs) %>% 
  ggplot(aes(x = h_cluster, y = species)) +
  geom_tile(aes(fill = nrel)) + 
  xlab('Cluster') + 
  ylab('Species') +
  scale_fill_gradient(name = 'Percent\nof cases',
                      low = 'white', 
                      high = 'firebrick', 
                      na.value = 'white')
ggsave('figures/cluster_hierachical_species.jpeg', width = 15, height = 10, units = 'cm', device = 'jpeg')



#results for pca cluster


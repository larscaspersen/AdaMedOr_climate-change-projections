library(tidyverse)
library(ggpubr)
library(multcomp)

#posthoc analysis

performance <- read.csv('data/performance_fitted_models.csv')


performance %>% 
  filter(species == 'Almond') %>% 
  ggline(x = "cultivar", y = "rmse", 
         add = c("mean_se", "jitter"), 
         ylab = "Weight", xlab = "Treatment")

mod <- lm(rpiq_adj ~ cultivar, data = performance[performance$species == 'Almond',])

# Compute the analysis of variance
res.aov <- aov(rmse ~ cultivar, data = performance[performance$species == 'Almond',])
# Summary of the analysis
summary(res.aov)

mod_means_contr <- emmeans::emmeans(object = mod,
                                    pairwise ~ "cultivar",
                                    adjust = "tukey")

mod_means <- multcomp::cld(object = mod_means_contr$emmeans,
                           Letters = letters)
mod_means

#maybe compare first on a species level
mod <- lm(rpiq_adj ~ species, data = performance)

mod_means_contr <- emmeans::emmeans(object = mod,
                                    pairwise ~ "species",
                                    adjust = "tukey")

mod_means <- multcomp::cld(object = mod_means_contr$emmeans,
                           Letters = letters)
mod_means

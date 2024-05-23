# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

adamedor <- read.csv('data/combined_phenological_data_adamedor_clean.csv')

library(tidyverse)
adamedor %>% 
  group_by(species, cultivar) %>% 
  summarise(n = n(), 
            location = paste0(unique(location))) %>% 
  filter(n >= 20) %>% 
  group_by(location) %>% 
  summarise(species = unique(species))

stations <- read.csv('data/weather_ready/weather_station_phenological_observations.csv')

stations$species <- c(paste0(c('Apricot', 'Pear', 'Sweet Cherry'), collapse = ', '),
                      paste0(c('Apple', 'European Plum', '\nPear', 'Sweet Cherry'), collapse = ', '),
                      paste0(c('Almond', 'Pistachio'), collapse = ', '),
                      'Apricot',
                      paste0(c('Almond', 'Apple'), collapse = ', '),
                      'Almond')


stations$label <- c(paste0('"', '~bold(Zaragoza)~\n', paste0(c('Apricot', 'Pear', 'Sweet*Cherry'), collapse = '*'), '"',  collapse = ''),
                    paste0('"', '~bold(Klein-Altendorf)~\n', paste0(c('Apple', 'European*Plum', '\nPear', 'Sweet*Cherry'), collapse = '*'),'"', collapse = ''),
                    paste0('"', '~bold(Sfax)~\n', paste0(c('Almond', 'Pistachio'), collapse = '*'), '"', collapse = ''),
                    paste0('"', '~bold(Cieza)~\n', paste0(c('Apricot')), '"', collapse = ''),
                    paste0('"', '~bold(Meknes)~\n', paste0(c('Almond', 'Apple'), collapse = '*'), '"', collapse = ''),
                    paste0('"', '~bold(Santomera)~\n', paste0(c('Almond')), '"', collapse = ''))



# stations$label <- c("~bold('Zaragoza\n')~'Apricot, Pear, Sweet Cherry'",
#                     "~bold('Klein-Altenforf')~'\nApple,'~'European'~'Plum,'~'Pear'~'Sweet'~'Cherry'",
#                     "~bold('Sfax')~'\nAlmond,'~'Pistachio'",
#                     "~bold('Cieza')~'\nApricot'",
#                     "~bold('Meknes')~'\nAlmond,'~'Apple'")

library("ggrepel")
library("ggspatial")

world <- ne_countries(scale = 'medium', returnclass = 'sf')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = stations, aes(x = longitude, y = latitude), size = 3, 
             shape = 23, fill = "darkred") +
  geom_label_repel(data = stations, aes(x = longitude, y = latitude, label = station_name),
                  fontface = "bold",
                  nudge_x = c(-2, -3, -1.5, -1.5, -1.8, 1.5),
                  nudge_y = c(0, 0, 0, 0, 0, 0.05),
                  min.segment.length = 20,
                  fill = "#FFFFFFAA", label.size = NA) +
  # geom_text_repel(data = stations, aes(x = longitude, y = latitude, label = station_name), 
  #                 fontface = "bold", position = position_dodge()) +
  geom_label_repel(data = stations, aes(x = longitude, y = latitude, label = species),
                  nudge_x = c(-2, -3, -1.5, -1.5, -1.8, 1.5),
                  nudge_y = c(-0.70, -1.1, -0.70, -0.70, -0.70, -0.7), min.segment.length = 20, fill = "#FFFFFFAA", label.size = NA) +
  coord_sf(xlim = c(-12, 12), ylim = c(28, 54), expand = FALSE) +
  xlab('Longitude') +
  ylab('Latitude')+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(height = unit(1, 'cm'),
                         width = unit(1, 'cm'),
    location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave('figures/paper/overview_map.jpeg', height = 15, width = 10, units = 'cm', device = 'jpeg')


cm_to_inch <- 1/2.54
svg(file="figures/paper/map_blank.svg",
    height = 15 * cm_to_inch,
    width = 10 * cm_to_inch)


ggplot(data = world) +
  geom_sf() +
  geom_point(data = stations, aes(x = longitude, y = latitude), size = 3, 
             shape = 23, fill = "darkred") +
  # geom_label_repel(data = stations, aes(x = longitude, y = latitude, label = station_name),
  #                 fontface = "bold",
  #                 nudge_x = c(-2, -3, -1.5, -1.5, -1.8, 1.5), 
  #                 nudge_y = c(0, 0, 0, 0, 0, 0.05),
  #                 min.segment.length = 20,
  #                 fill = "#FFFFFFAA", label.size = NA) +
  # geom_text_repel(data = stations, aes(x = longitude, y = latitude, label = station_name), 
  #                 fontface = "bold", position = position_dodge()) +
  # geom_label_repel(data = stations, aes(x = longitude, y = latitude, label = species), 
  #                 nudge_x = c(-2, -3, -1.5, -1.5, -1.8, 1.5), 
  #                 nudge_y = c(-0.70, -1.1, -0.70, -0.70, -0.70, -0.7), min.segment.length = 20, fill = "#FFFFFFAA", label.size = NA) +
coord_sf(xlim = c(-12, 12), ylim = c(28, 54), expand = FALSE) +
  xlab('Longitude') +
  ylab('Latitude')+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(height = unit(1, 'cm'),
                         width = unit(1, 'cm'),
                         location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering)

dev.off()












santomera <- read.csv('data/weather_ready/murcia_clean.csv')

santomera %>% 
  filter(Year >= 1990, Year <= 2020) %>% 
  summarise(mean_Tmin = mean(Tmin),
            mean_Tmax = mean(Tmax))


# ggplot(data = world) +
#   geom_sf() +
#   geom_point(data = stations, aes(x = longitude, y = latitude), size = 3, 
#              shape = 23, fill = "darkred") +
#   geom_label_repel(data = stations, aes(x = longitude, y = latitude, label = label), 
#                    nudge_x = c(-2, -3, -1.5, -1.5, -1.8), 
#                    min.segment.length = 20, fill = "#FFFFFFAA", parse = TRUE) +
#   coord_sf(xlim = c(-12, 12), ylim = c(28, 54), expand = FALSE) +
#   annotation_scale(location = "bl", width_hint = 0.4) +
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering)
# 
# ggsave('figures/overview_map.jpeg', height = 20, width = 15, units = 'cm', device = 'jpeg')
#   
#   
#   
# ggplot(data = world) +
#   geom_sf(fill = "antiquewhite1") +
#   geom_sf(data = counties, aes(fill = area)) +
#   geom_sf(data = states, fill = NA) + 
#   geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
#   geom_sf(data = flcities) +
#   geom_text_repel(data = flcities, aes(x = lng, y = lat, label = city), 
#                   fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
#                                                                                  -0.25, 0.5, 0.5, -0.5)) +
#   geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", 
#              nudge_y = states$nudge_y) +
#   scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
#   annotation_scale(location = "bl", width_hint = 0.4) +
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE) +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("Observation Sites", subtitle = "(2 sites in Palm Beach County, Florida)") +
#   theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
#                                         size = 0.5), panel.background = element_rect(fill = "aliceblue"))
#     
# 
# 
# 
# 
# st_as_sf(country_elevation_df)
# 
# 
# tm_shape(country_elevation_df) +
#   tm_raster()
# 
# 
# sp::SpatialPolygonsDataFrame(country_transformed)
#     
# # Crop the spatial object according to the limits set above
# mediterranean <- terra::crop(country_transformed, med_extent)
#     
#     
# # Replace point boundary extent with that of the Mediterranean to make sure the interpolation is done for the whole extent
#     # of the Mediterranean
#     stations_sp@bbox <- mediterranean@bbox
#     
#     # Define Med extent to get elevation
#     elevation_med_extent <- terra::crop(mediterranean, raster::extent(-10, 45, 21, 49))
#     
#     # Get the elevation model of the Mediterranean
#     elevation_med <- elevatr::get_elev_raster(elevation_med_extent, clip = "bbox", z = 6, neg_to_na = TRUE)
#     
#     # Create the map
#     stations_map <- tm_shape(elevation_med, bbox = raster::extent(-10, 45, 20, 49)) +
#       tm_raster(breaks = seq(0, 4500, 1500), style = "cont", title = "Elevation (m.a.s.l.)",
#                 legend.is.portrait = FALSE) +
#       tm_shape(mediterranean, bbox = raster::extent(-10, 45, 20, 49)) +
#       tm_borders(col = 'grey40', lwd = 0.8) +
#       tm_graticules(lines = FALSE, labels.size = 0.6, labels.col = "black") +
#       tm_shape(stations_sp) +
#       tm_symbols(size = 0.1, shape = 4, col = 'steelblue4') +
#       tm_compass(position = c(0.93, 0.05), text.size = 0.5) +
#       tm_scale_bar(position = c(0.57, 0.055), bg.color = 'transparent', text.size = 0.5, color.dark = "grey20") +
#       tm_add_legend(type = "symbol", labels = "Weather station",
#                     col = "steelblue4", shape = 4, size = 0.35, title = "") +
#       tm_layout(legend.outside = FALSE,
#                 legend.position = c(0.05, 0.015),
#                 legend.bg.color = "transparent",
#                 outer.margins = c(0.02, 0.02, 0.02, 0.02),
#                 legend.title.size = 0.9,
#                 legend.text.size = 0.6,
#                 legend.width = -0.5,
#                 legend.stack = "horizontal",
#                 bg.color = "white",
#                 
#                 attr.color = "black")
# 
#     
#     
#     
#     
#     #make map of europ with the sample locations
#     
#     library(tmap)
#     
#     data("World")
#     
#     tm_shape(World) +
#       tm_polygons()
#     
#     
#     # libraries we need
#     libs <- c("elevatr", "terra", "tidyverse", 
#               "sf", "giscoR", "marmap")
#     
#     # install missing libraries
#     installed_libs <- libs %in% rownames(installed.packages())
#     if (any(installed_libs == F)) {
#       install.packages(libs[!installed_libs])
#     }
#     
#     # load libraries
#     invisible(lapply(libs, library, character.only = T))
#     
#     
#     
#     # 1. GET COUNTRY MAP
#     #---------
#     
#     crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
#     
#     giscoR::gisco_get_countries(region = )
#     
#     ctry_code <- c('AUT', 'AND', 'BEL',
#                    'ESP', 'NLD', 'FRA', 
#                    'ITA', 'DEU', 'DZA',
#                    'LIE', 'IRL', 'MLT',
#                    'PRT', 'LUX', 'MAR',
#                    'MCO', 'TUN', 'GBR',
#                    'SMR', 'LBY', 'CHE',
#                    'CZE', 'SVN', 'HRV',
#                    'POL')
#     
#     
#     
#     region <- gisco_get_countries(country = ctry_code)
#     ggplot(region) +
#       geom_sf(fill = "#078930", col = "white") +
#       theme_minimal()
#     
#     
#     
#     country_transformed <- st_transform(region, crs = crsLONGLAT)
#     
#     
#     
#     
#     # 2. GET ELEVATION DATA
#     #---------
#     
#     get_elevation_data <- function(country_elevation, country_elevation_df) {
#       
#       country_elevation <- get_elev_raster(
#         locations = country_transformed, 
#         z = 4, 
#         clip = "locations") 
#       
#       country_elevation_df <- as.data.frame(country_elevation, xy = T) %>%
#         na.omit()
#       
#       colnames(country_elevation_df)[3] <- "elevation"
#       
#       return(country_elevation_df)
#     }
#     
#     country_elevation_df <- get_elevation_data()
#     
#     region_outline    <- fortify(country_transformed, region = "CNTR_ID")
#     
#     
#     
#     
#     # 3. MAP
#     #---------
#     
#     typeof(country_elevation_df)
#     
#     tm_shape(country_elevation_df) +
#       tm_polygons()
#     
#     
#     
#     
#     
#     
#     
#     ggplot() +
#       geom_tile(data = country_elevation_df, 
#                 aes(x = x, y = y, fill = elevation)) +
#       scale_fill_etopo() +
#       geom_polygon(data = region_outline, aes(x = x, y = y), fill = NA, color = 'black') +
#       coord_sf(crs = crsLONGLAT, xlim = c(-19, 12), ylim = c(24, 54)) +
#       
#       theme_bw() +
#       theme(text = element_text(family = "georg", color = "#22211d"),
#             axis.line = element_blank(),
#             axis.text.x = element_blank(),
#             axis.text.y = element_blank(),
#             axis.ticks = element_blank(),
#             axis.title.x = element_blank(),
#             axis.title.y = element_blank(),
#             legend.position = "none",
#             panel.grid.major = element_line(color = "white", size = 0.2),
#             panel.grid.minor = element_blank(),
#             plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
#             plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
#             plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
#             plot.background = element_rect(fill = "white", color = NA), 
#             panel.background = element_rect(fill = "white", color = NA),
#             panel.border = element_blank())
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     library(tmap)
#     
#     
#     
#     # Read the file containing the initial 387 weather stations. Remember that, for several reasons, the final number of stations
#     # reduced to 347
#     stations <- read.csv("data/weather_ready/weather_station_phenological_observations.csv")
#     
#     # Transform the data frame into a spatial object
#     stations_sp <- sp::SpatialPointsDataFrame(stations[, c("longitude", "latitude")],
#                                               proj4string = sp::CRS( "+proj=longlat +datum=WGS84 +no_defs"),
#                                               data = stations[, -(which(colnames(stations) %in% 
#                                                                           c("longitude", "latitude")))])
#     
#     # Define the limits for the region
#     med_extent <- raster::extent(-12.0, 12, 25.0, 50.0)
#     
#     
#     
#     
#     
#     country_elevation <- get_elev_raster(
#       locations = country_transformed, 
#       z = 7, 
#       clip = "locations", neg_to_na = TRUE) 
#     
#     # Crop the spatial object according to the limits set above
#     mediterranean <- terra::crop(country_elevation, med_extent)
#     
#     # tm_shape(country_elevation, bbox = raster::extent(-12, 12, 28, 54)) +
#     #   tm_raster(palette = rev(etopo.colors(11))[-c(1:2)], ) +
#     tm_shape(country_transformed, bbox = raster::extent(-12, 12, 28, 54)) +
#       tm_borders() +
#       tm_graticules(lines = FALSE, labels.size = 0.6, labels.col = "black") +
#       tm_shape(stations_sp) +
#       tm_symbols(size = 1, shape = 20, col = 'salmon') +
#       tm_shape(stations_sp) +
#       geom_text_repel(data = flcities, aes(x = lng, y = lat, label = city), 
#                       fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
#                                                                                      -0.25, 0.5, 0.5, -0.5))
#     tm_text("station_name") +
#       tm_compass(position = c(0.93, 0.05), text.size = 0.5) +
#       tm_scale_bar(position = c(0.57, 0.055), bg.color = 'transparent', text.size = 0.5, color.dark = "grey20") +
#       tm_add_legend(type = "symbol", labels = "Weather station",
#                     col = "salmon", shape = 20, size = 0.35, title = "")
    
    
    



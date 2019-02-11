# install.packages("mapedit")

#
# Interactive editing of positions (on a satellite map)
#

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Libraries + functions  ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

library(leaflet)
library(mapview)
library(sf)
library(raster)   # note: select() exists in both raster and dplyr
library(dplyr)

source("04fun_mapview_functions.R")
source("02fun_Get_areas_functions.R")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## 1. Data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

### CORINE classes lookup table
corine_classes <- readxl::read_excel("Data/02_CORINE_classes.xlsx")

### Site data
df_sites <- readRDS("Data/02_df_sites.rds")

# site20 <- readRDS("temp_site20.rds")
# df_sites[20,]$geometry <- site20

### Raster data
folder_tif <- "C:/Data/Star-Walk/g100_clc12_V18_5"
fn <- paste0(folder_tif, "/g100_clc12_V18_5.tif")
raster_corine <- raster(fn)

angle_satellite <- readRDS("Data/02_angle_satellite.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## 2. Mapview plots ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Plot CORINE pixels of one site
mapview_site(1)


# Plot pie of one site
mapview(get_trapeze_polygon(2, df_sites, angle_satellite), map.types = "Esri.WorldImagery")

# Plot pies of each lake (note: can click on station no in bottom right corner to zoom on each of them)
mapview_multi <- function(i){
  trapeze_list <- i %>% purrr::map(~get_trapeze_polygon(., df_sites, angle_satellite))
  names(trapeze_list) <- paste("Station", i)
  mapview(trapeze_list, map.types = "Esri.WorldImagery", col.regions = "red")
}

mapview_multi(1:6)
mapview_multi(1:6 + 6)
mapview_multi(1:6 + 12)
mapview_multi(1:6 + 18)
mapview_multi(1:6 + 24)
mapview_multi(1:6 + 30)


# test
# rasterobject <- get_raster_station(2, df_sites, raster_corine)
# cols <- corine_classes$Color[corine_classes$BinValues %in% rasterobject$binvalues] 
# mapview(rasterobject$raster, map.types = "Esri.WorldImagery", col.regions = cols, alpha.regions = 0.6)

# dir.create("Figures/07_www_maps")

mapview_lake <- function(lakename){
  i <- which(df_sites$lake %in% lakename)
  mapview_multi(i) %>% mapshot(sprintf("Figures/07_www_maps/Map_%s.html", lakename))
  }

unique(df_sites$lake) %>% purrr::walk(mapview_lake)


# Plot pie plus raster of one site
# NOTE: mapview isn't able to plot the rasters correctly, the pixels appear to be a little jumbled up
plot_pie_raster <- function(i){
  rast <- df_sites[i,] %>% st_coordinates() %>% get_raster_site(dm = 1000, raster = raster_corine)
  binvalues <- rast@data@values %>% table() %>% names() %>% as.numeric()
  cols <- corine_classes$Color[corine_classes$BinValues %in% binvalues] 
  mapview(rast, alpha.regions = 0.6, map.types = "Esri.WorldImagery", col.regions = cols) +
    mapview(get_trapeze_polygon(i, df_sites, angle_satellite), col.regions = "grey", alpha.regions = 0.6)
} 

# dir.create("Figures/07_sitemaps_examples")

plot_pie_raster(6) %>% mapshot("Figures/07_sitemaps_examples/Map06_Ohrid_6.html")
plot_pie_raster(8) %>% mapshot("Figures/07_sitemaps_examples/Map08_Prespa_2.html")
plot_pie_raster(14) %>% mapshot("Figures/07_sitemaps_examples/Map14_Lura_2.html")



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# Tried to plot a list of rasters but that doesn't work....
#   (I guess one then has to make them into polygons)
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Function: Get raster of each lake (note: can click on station no in bottom right corner to zoom on each of them)
# get_raster_station <- function(i, data, raster){
#   rast <- data[i,] %>% st_coordinates() %>% get_raster_site(dm = 1000, raster = raster)
#   binvalues <- rast@data@values %>% table() %>% names() %>% as.numeric()
#   list(raster = rast, binvalues = binvalues)
# }

# rasterobj_list <- 1:6 %>% 
#   purrr::map(~get_raster_station(., df_sites, raster_corine)) %>%
#   purrr::transpose()
# cols <- rasterobj_list$binvalues %>% purrr::flatten() %>% unlist() %>% unique()
# mapview(rasterobj_list$raster, map.types = "Esri.WorldImagery", col.regions = cols, alpha.regions = 0.6)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Make "_a" plots ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Just data, without geography
data_df <- as.data.frame(df_sites)

# Make "_a" plots
for (i in seq_len(nrow(df_sites))){
  # for (i in 13:18){
  png(sprintf("Figures/07_Siteplots/Site%02.f_a.png", i), width = 20, height = 20, units = "cm", res = 200)
  plot_site_trapeze(i, angle_vector = angle_satellite)
  dev.off()
} 

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Make "_b" plots and calculate all areas ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Just data, without geography
data_df <- as.data.frame(df_sites)

# Make all trapeze polygons
trapezes_landuse <- seq_len(nrow(df_sites)) %>% purrr::map(get_trapezeoverlap_from_site, angle_vector = angle_satellite)
# plot(trapezes_landuse[[i]][,"Land_use"])

# Make "_b" plots
for (i in seq_len(nrow(df_sites))){
  title <- paste0("Site ", i, ": ", data_df[i,"lake"], ", ", data_df[i,"site"], " (", data_df[i,"name"], ")")
  png(sprintf("Figures/07_Siteplots/Site%02.f_b.png", i), width = 20, height = 20, units = "cm", res = 200)
  plot(trapezes_landuse[[i]][,"Land_use"], main = "")
  mtext(title, cex = 1.2, line = 0.5)
  dev.off()
} 



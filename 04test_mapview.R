### Libraries + functions
library(foreign)

library(raster)
library(mapview)
library(dplyr)
library(ggplot2)

source("01fun_plot_raster_functions.R")
source("02fun_Get_areas_functions.R")

## Data ----
### Raster data
folder_tif <- "C:/Data/Star-Walk/g100_clc12_V18_5"
fn <- paste0(folder_tif, "/g100_clc12_V18_5.tif")
raster_corine <- raster(fn)

### Raster for Ohrid
ext <- extent(5212458, 5235640, 2036437, 2076818)
raster_ohrid <- crop(raster_corine, ext)

### CORINE classes lookup table
corine_classes <- readxl::read_excel("Data/02_CORINE_classes.xlsx")

### Site data
df_sites <- readRDS("Data/02_df_sites.rds")

## Plots

### Lura - Satellite map and sampling points
df_sites_pick <- subset(df_sites, lake %in% "Lura")
mapview(df_sites_pick, map.types = "Esri.WorldImagery")

### Lura - Satellite map and sampling points, plus CORINE map

# Get CORINE raster for lake pluss 1000 m to each side
ext <- extent(df_sites_pick)
ext@xmin <- ext@xmin - 1000
ext@xmax <- ext@xmax + 1000
ext@ymin <- ext@ymin - 1000
ext@ymax <- ext@ymax + 1000
raster_pick <- crop(raster_corine, ext)
# Get correct colors
vals <- unique(raster_pick@data@values) %>% sort()
cols <- corine_classes %>%
  filter(BinValues %in% vals) %>%
  pull(Color)
Area_type2 <- corine_classes %>%
  filter(BinValues %in% vals) %>%
  pull(Area_type2)
pie(rep(1, length(vals)), labels = Area_type2, col = cols)
# Plot
mapview(df_sites_pick, map.types = "Esri.WorldImagery") + 
  mapview(raster_pick, alpha.regions = 0.4, col.regions = cols)

# Trying to show actual class names of CORINE raster in the legend
# Two approaches:
# 1. Supply the same raster, but try to set the labels of the legend. Seems not to be possible 
# 2. Convert data to data frame and plot using ggplot (https://stackoverflow.com/a/48975387/1734247)
# 3. Trying to make a new raster where the values of the raster is 
#    a factor (with 'Area_type2' values):
rastermatrix <- as.matrix(raster_pick)
rastervector <- rastermatrix %>% as.numeric()
raster_df <- tibble(BinValues = rastervector) %>%
  left_join(corine_classes %>% select(BinValues, CLC_CODE, Area_type2))
raster_pick2 <- raster_pick
raster_matrix_new <- raster_df %>% pull(Area_type2) %>% as.factor() %>% matrix(ncol = ncol(rastermatrix)) 
raster_matrix_new[1:6, 1:3]
# raster_pick2[] <- raster_matrix_new  # Error: cannot replace values on this raster (it is too large)
raster_pick2[] <- raster_df %>% pull(Area_type2) %>% as.factor()

# Neither of these worked:
plainview(raster_pick2, col.regions = cols)
mapview(raster_pick2, col.regions = cols)
mapview(raster_pick2, col.regions = cols, legend = list(labels = letters[1:5]))

# This static plot worked however:
rasterVis::levelplot(raster_pick2, col.regions = cols)

# Half workaround...: use the CLC_CODE
raster_pick2[] <- raster_df %>% pull(CLC_CODE) %>% as.numeric()
mapview(df_sites_pick, map.types = "Esri.WorldImagery") + 
  mapview(raster_pick2, alpha.regions = 0.4, col.regions = cols)



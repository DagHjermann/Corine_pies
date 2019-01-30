### Libraries + functions
library(raster)
library(mapview)
library(dplyr)

source("04fun_mapview_functions.R")
source("02fun_Get_areas_functions.R")

## 1. Data ----
### Raster data
folder_tif <- "C:/Data/Star-Walk/g100_clc12_V18_5"
fn <- paste0(folder_tif, "/g100_clc12_V18_5.tif")
raster_corine <- raster(fn)

### CORINE classes lookup table
corine_classes <- readxl::read_excel("Data/02_CORINE_classes.xlsx")

### Site data
df_sites <- readRDS("Data/02_df_sites.rds")

## 2. Plots ----

### Lura lake sampling points 
df_sites_pick <- subset(df_sites, lake %in% "Lura")



### 2a. Lura - Satellite map and sampling points ----
mapview(df_sites_pick, map.types = "Esri.WorldImagery")


### 2b. Lura - Satellite map and sampling points, plus CORINE map ----

# Get raster for selected sample points
raster_pick <- get_raster_pick(df_sites_pick)

# Get raster data enhanced with CORINE codes and colors
raster_df <- raster_pick %>% get_raster_dataframe()

# Get colors and CORINE codes in the selected raster
cols <- raster_pick %>% get_corine_values("Color")
area_type2 <- raster_pick %>% get_corine_values("Area_type2")

# Dynamic map with CLC codes
raster_pick_clccode <- raster_pick
raster_pick_clccode[] <- raster_df %>% pull(CLC_CODE) %>% as.numeric() %>% matrix(ncol = ncol(raster_pick))  # fill raster with numeric values
m <- mapview(df_sites_pick, map.types = "Esri.WorldImagery") + 
  mapview(raster_pick_clccode, alpha.regions = 0.4, col.regions = cols)
m

if (FALSE){
  mapshot(m, "Figures/04_mapview/Lura.html")
}

# Static map with CLC codes + area types (but not satellite)

# Following https://oscarperpinan.github.io/rastervis/#factor
raster_pick_areatype <- ratify(raster_pick)
rat <- levels(raster_pick_areatype)[[1]]
rat <- merge(rat, as.data.frame(corine_classes)[,c("BinValues", "CLC_CODE", "Area_type2")], 
             all.x = TRUE, all.y = FALSE, by.x = "ID", by.y = "BinValues")
levels(raster_pick_areatype) <- rat
vis <- rasterVis::levelplot(raster_pick_areatype, col.regions = cols, att = "Area_type2") +
  latticeExtra::layer(sp.points(df_sites_pick_sp))
vis

# raster_pick_areatype[] <- raster_df %>% pull(Area_type2) %>% as.factor() %>% matrix(ncol = ncol(raster_pick))  # fill raster with factor values

if (FALSE){
  # tiff("Figures/04_Areatypes_Lura.tiff")
  png("Figures/04_Areatypes_Lura.png")
  vis
  dev.off()
  }




### Ohrid lake sampling points ----
df_sites_pick <- subset(df_sites, lake %in% "Ohrid")


### 2a. Ohrid - Satellite map and sampling points ----
mapview(df_sites_pick, map.types = "Esri.WorldImagery")


### 2b. Ohrid - Satellite map and sampling points, plus CORINE map ----

# Get raster for selected sample points
raster_pick <- get_raster_pick(df_sites_pick)

mapview(df_sites_pick, map.types = "Esri.WorldImagery") + 
  mapview(raster_pick, alpha.regions = 0.4, col.regions = cols)

# Get raster data enhanced with CORINE codes and colors
raster_df <- raster_pick %>% get_raster_dataframe()

# Test
raster_pick_test <- raster_pick
raster_pick_test[] <- raster_df %>% pull(BinValues) %>% as.numeric() %>% matrix(ncol = ncol(raster_pick)) # fill raster with numeric values
mapview(df_sites_pick, map.types = "Esri.WorldImagery") + 
  mapview(raster_pick_test, alpha.regions = 0.4, col.regions = cols)

# Get colors and CORINE codes in the selected raster
cols <- raster_pick %>% get_corine_values("Color")
area_type2 <- raster_pick %>% get_corine_values("Area_type2")

# Dynamic map with CLC codes
CLC_code <- raster_pick
CLC_code[] <- raster_df %>% pull(CLC_CODE) %>% as.numeric() %>% matrix(ncol = ncol(raster_pick))  # fill raster with numeric values
m <- mapview(df_sites_pick, map.types = "Esri.WorldImagery") + 
  mapview(CLC_code, alpha.regions = 0.4, col.regions = cols)
m

if (FALSE){
  mapshot(m, "Figures/04_mapview/Ohrid.html")
}

# Static map with CLC codes + area types (but not satellite)

# Following https://oscarperpinan.github.io/rastervis/#factor
raster_pick_areatype <- ratify(raster_pick)
rat <- levels(raster_pick_areatype)[[1]]
rat <- merge(rat, as.data.frame(corine_classes)[,c("BinValues", "CLC_CODE", "Area_type2")], 
             all.x = TRUE, all.y = FALSE, by.x = "ID", by.y = "BinValues")
levels(raster_pick_areatype) <- rat
vis <- rasterVis::levelplot(raster_pick_areatype, col.regions = cols, att = "Area_type2") +
  latticeExtra::layer(sp.points(df_sites_pick_sp))
vis   # Not able to show points, for some reason. See script 01 instead

# raster_pick_areatype[] <- raster_df %>% pull(Area_type2) %>% as.factor() %>% matrix(ncol = ncol(raster_pick))  # fill raster with factor values

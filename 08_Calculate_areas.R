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
library(ggplot2)

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


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. Calculate areas ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Make all trapeze polygons
trapezes_landuse <- seq_len(nrow(df_sites)) %>% purrr::map(get_trapezeoverlap_from_site, angle_vector = angle_satellite)

# check
# trapezes_landuse[[8]]

# Getting area for a single site
get_area_rawdata <- function(i, trapeze_list, data){
  data <- as.data.frame(data)
  trapeze = trapeze_list[[i]]
  data.frame(i = i, 
             data[i,c("lake","site","name")], 
             Area = st_area(trapeze), 
             Land_use = trapeze$Land_use, 
             stringsAsFactors = FALSE)
  
}

# get_area_rawdata(8, trapeze_list = trapezes_landuse, data = df_sites)

# Getting areas for all sites
areas_landuse_raw <- seq_len(nrow(df_sites)) %>% 
  purrr::map_df(get_area_rawdata, trapeze_list = trapezes_landuse, data = df_sites)

# Getting areas for all sites, summarised
areas_landuse <- areas_landuse_raw %>%
  group_by(i, lake, site, name, Land_use) %>%
  summarise(Area = sum(Area))

# check
# areas_landuse %>% filter(i == 8)

areas_landuse <- left_join(
  areas_landuse %>% mutate(Land_use = as.numeric(Land_use)), 
  corine_classes %>% select(BinValues, Color, CLC_CODE, Area_type, Area_type2),
  by = c("Land_use" = "BinValues") 
)

areas_landuse_broad <- areas_landuse %>%
  select(i, lake, site, name, Area_type2, Area) %>%
  tidyr::spread(Area_type2, Area)

# openxlsx::write.xlsx(areas_landuse, "Data/08_LandUse_10ha_trapeze.xlsx")
# openxlsx::write.xlsx(areas_landuse_broad, "Data/08_LandUse_10ha_trapeze_bred.xlsx")

# file.copy("Data/02_LandUse_10ha_trapeze.xlsx", "Data/02_LandUse_10ha_trapeze_OLD.xlsx")
# file.copy("Data/02_LandUse_10ha_trapeze_bred.xlsx", "Data/02_LandUse_10ha_trapeze_bred_OLD.xlsx")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Plot area per site
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Get colors (in the same order as the unique Area_type2 values)
cols <- areas_landuse %>%
  group_by(Area_type2) %>%
  summarise(Color = first(Color)) %>%
  pull(Color)

levels <- with(df_sites, paste(lake, site))

areas_landuse %>%
  as.data.frame() %>%
  mutate(Lake_site = factor(paste(lake,site), levels = rev(levels))) %>%                             # These two lines are required 
  ggplot(aes(Lake_site, Area, fill = Area_type2)) + 
  geom_col() +
  scale_fill_manual(values = cols) +
  coord_flip() 

# file.copy("Figures/02_Land_use_10ha_trapeze.png", "Figures/02_Land_use_10ha_trapeze_OLD.png")
ggsave("Figures/08_Land_use_10ha_trapeze.png", width = 9, dpi = 500)





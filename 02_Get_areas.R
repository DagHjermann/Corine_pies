### Libraries + functions
library(raster)     # raster(), extent()
library(foreign)    # read.dbf()
library(sf)         # st_*()
library(dplyr)
library(ggplot2)

source("01fun_plot_raster_functions.R")
source("02fun_Get_areas_functions.R")

### Raster data
folder_tif <- "C:/Data/Star-Walk/g100_clc12_V18_5"
fn <- paste0(folder_tif, "/g100_clc12_V18_5.tif")
raster_corine <- raster(fn)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Create CORINE classes lookup table
# From "\\niva-of5\osl-userdata$\DHJ\Documents\seksjon 318\Star-Walk\Corine\read raster 2012.R"
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

corine_classes <- raster_corine@data@attributes[[1]]
corine_classes$Color <- raster_corine@legend@colortable[1:nrow(corine_classes)]
df_colors <- read.dbf(paste0(folder_tif, "/g100_clc12_V18_5.tif.vat.dbf"))
corine_classes <- base::merge(corine_classes, df_colors, by.x = "BinValues", by.y = "Value")
# corine_classes$LABEL3

# Area class for print (shortened the "Mostly agriculture..." class)
corine_classes$Area_type <- corine_classes$LABEL3
levels(corine_classes$Area_type)[22] <- "Agriculture with significant areas of natural vegetation"
# legend("topright", legend = corine_classes$Area_type, fill = corine_classes$Color)

# Code + area class
factorToChar <- function(x) levels(x)[as.numeric(x)]
corine_classes$Area_type2 <- paste(factorToChar(corine_classes$CLC_CODE), factorToChar(corine_classes$Area_type))

corine_classes$ID <- NULL   # This is just confusing, so we delete it

# openxlsx::write.xlsx(corine_classes, "Data/02_CORINE_classes.xlsx")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Crop CORINE map to Lake Ohrid ----
# Just for example
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

ext <- extent(5212458, 5235640, 2036437, 2076818)
raster_ohrid <- crop(raster_corine, ext)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Read site data and turn into and sf object ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df_sites <- readxl::read_excel("Data/STAR-WALK sites.xlsx")
# df_sites

# Function to make a separaet point from each line of data
# Note: don't make multipoints, then you cannot set data for each point
# See example from ?st_geometry
make_point <- function(i){
  df_sites[i,c("Elongitude","Nlatitude")] %>% 
    as.numeric() %>%
    st_point()
}
# test
# make_point(1)

# Make list of points
point_list <- 1:nrow(df_sites) %>% purrr::map(make_point)

# Turn site data into sf object; also set crs (lat / long)
st_geometry(df_sites) <- st_sfc(point_list, crs = 4326)

# plot(df_sites)

# Transform long/lat to the standard European Coordinate Reference System defined 
# by the European Terrestrial Reference System 1989 (ETRS89) datum and 
# Lambert Azimuthal Equal Area (LAEA) projection (EPSG: 3035)
# See: https://land.copernicus.eu/pan-european/corine-land-cover/clc-2012?tab=metadata
df_sites <- df_sites %>% st_transform(3035) 

# Save
# saveRDS(df_sites, "Data/02_df_sites.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Plot Ohrid sites ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

plot(raster_ohrid)
points(subset(df_sites, lake == "Ohrid") %>% st_coordinates(), pch = 20, col = 1)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Get all angles ----
# If interactive = TRUE, you will find the angles by clicking your way 
# through a series of maps (one map per site). 
# If interactive = FSKE, use the stored angles
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Test
# make_triangle(1)
# make_triangle(2)

# Angles to store
N <- nrow(df_sites)
angles <- rep(NA, N)

interactive <- FALSE   # set to TRUE if you want to click your way through all the sites

if (interactive){   # Go through each site
  for (i in 1:N){
  # for (i in 13:18){
  ok <- "n"
    while (ok != "y"){
      angles[i] <- get_angle_from_click(i)
      ok <- readline(prompt = "Hit 'y' if OK, 'n' otherwise ")
    }
  }
} else {            # or use the saved values
  angles <- readRDS("Data/02_angles.rds")
}



#
# save as RDS
#
# saveRDS(angles, "Data/02_angles.rds")

#
# save as Excel
#
# df_sitedata <- as.data.frame(df_sites)
# df_sitedata$Angle <- angles
# df_sitedata$geometry <- NULL
# openxlsx::write.xlsx(df_sitedata, "Data/02_Sites_with_angles.xlsx")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Check one site ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

plot_site_trapeze(1)

get_trapeze_from_site(1)
plot(get_trapeze_from_site(1))

# Make trapeze
trapeze_landuse <- get_trapezeoverlap_from_site(1)
trapeze_landuse

# Plot
plot(trapeze_landuse[,"Land_use"])

trapeze_landuse_df <- data.frame(Area = st_area(trapeze_landuse), Land_use = trapeze_landuse$Land_use)
xtabs(Area ~ Land_use, trapeze_landuse_df)

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
  png(sprintf("Figures/Siteplots/Site%02.f_a.png", i), width = 20, height = 20, units = "cm", res = 200)
  plot_site_trapeze(i)
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
trapezes_landuse <- seq_len(nrow(df_sites)) %>% purrr::map(get_trapezeoverlap_from_site)
# plot(trapezes_landuse[[i]][,"Land_use"])

# Make "_b" plots
for (i in seq_len(nrow(df_sites))){
  title <- paste0("Site ", i, ": ", data_df[i,"lake"], ", ", data_df[i,"site"], " (", data_df[i,"name"], ")")
  png(sprintf("Figures/Siteplots/Site%02.f_b.png", i), width = 20, height = 20, units = "cm", res = 200)
  plot(trapezes_landuse[[i]][,"Land_use"], main = "")
  mtext(title, cex = 1.2, line = 0.5)
  dev.off()
  } 

# Getting area for a single site
get_area_rawdata <- function(i, trapeze_list, data){
  trapeze = trapeze_list[[i]]
  data.frame(i = i, 
             data[i,c("lake","site","name")], 
             Area = st_area(trapeze), 
             Land_use = trapeze$Land_use, 
             stringsAsFactors = FALSE)
  
}

# get_area_rawdata(1, trapeze_list = trapezes_landuse, data = data_df)

# Getting areas for all sites
areas_landuse_raw <- seq_len(nrow(df_sites)) %>% purrr::map_df(get_area_rawdata, trapeze_list = trapezes_landuse, data = data_df)

# Getting areas for all sites, summarised
areas_landuse <- areas_landuse_raw %>%
  group_by(i, lake, site, name, Land_use) %>%
  summarise(Area = sum(Area))

areas_landuse <- left_join(
  areas_landuse %>% mutate(Land_use = as.numeric(Land_use)), 
  corine_classes %>% select(BinValues, Color, CLC_CODE, Area_type, Area_type2),
  by = c("Land_use" = "BinValues") 
)

areas_landuse_broad <- areas_landuse %>%
  select(i, lake, site, name, Area_type2, Area) %>%
  tidyr::spread(Area_type2, Area)

# openxlsx::write.xlsx(areas_landuse, "Data/02_LandUse_10ha_trapeze.xlsx")
# openxlsx::write.xlsx(areas_landuse_broad, "Data/02_LandUse_10ha_trapeze_bred.xlsx")

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

areas_landuse %>%
  as.data.frame() %>%
  mutate(Lake_site = factor(paste(lake,site))) %>%                             # These two lines are required 
  mutate(Lake_site = factor(Lake_site, levels = rev(levels(Lake_site)))) %>%   # only to put lakes in correct order
  ggplot(aes(Lake_site, Area, fill = Area_type2)) + 
  geom_col() +
  scale_fill_manual(values = cols) +
  coord_flip() 
ggsave("Figures/02_Land_use_10ha_trapeze.png", width = 9, dpi = 500)





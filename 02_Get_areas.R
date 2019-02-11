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

# Used this one first, positions were edited using script 06 later
# df_sites <- readxl::read_excel("Data/STAR-WALK sites.xlsx")

# Positions after editing using script 06
df_sites <- readxl::read_excel("Data/STAR-WALK sites adjusted.xlsx", sheet = "For R import")
# df_sites

sel <- !is.na(df_sites$`Nlatitude NEW`)
df_sites$Nlatitude[sel] <- df_sites$`Nlatitude NEW`[sel]
df_sites$Elongitude[sel] <- df_sites$`Elongitude NEW`[sel]

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


# NOTE: these points were adjusted at the bottom of this script, then adjusted 
#  coordinates were inserted back into Excel (so all data should be consistent now)

# file.copy("Data/02_df_sites.rds", "Data/02_df_sites_OLD.rds") # after manual editing
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
# Get all angles 1 ----
#
# 'angles', readRDS("Data/02_angles.rds")
# NOTE: we end up using these only temporarily
# The final angles are 'angle_satellite'
#
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

# Extra editing
# i <- 6
# angles[i] <- get_angle_from_click(i)
# i <- 8
# angles[i] <- get_angle_from_click(i)



#
# save as RDS
#
# file.copy("Data/02_angles.rds", "Data/02_angles_OLD.rds")
# saveRDS(angles, "Data/02_angles.rds")

#
# save as Excel
#
# df_sitedata <- as.data.frame(df_sites)
# df_sitedata$Angle <- angles
# df_sitedata$geometry <- NULL
# openxlsx::write.xlsx(df_sitedata, "Data/02_Sites_with_angles.xlsx")

# OLD data ()
# file.copy("Data/02_Sites_with_angles.xlsx", "Data/02_Sites_with_angles_OLD.xlsx")



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Get all angles 2 ----
#
# Find angles, using satellite pictures and mapedit instead
# 'angle_satellite', readRDS("Data/02_angle_satellite.rds")
#
# If interactive = TRUE, you will find the angles by clicking your way 
# through a series of maps (one map per site). 
# If interactive = FSKE, use the stored angles
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Click correct position for each point on the lake:
library(mapedit)
library(mapview)


interactive <- FALSE

# angle_satellite <- readRDS("Data/angle_satellite_1to13.rds")

if (interactive){   # Go through each site
  N <- nrow(df_sites)
  angle_satellite <- rep(NA, N)
  for (i in 1:N){
    angle_satellite[i] <- get_angle_from_click_satellite(i)
  }
} else {            # or use the saved values
  angle_satellite <- readRDS("Data/02_angle_satellite.rds")
}

# cbind(angles, angle_satellite)

# Save result
# saveRDS(angle_satellite, "Data/02_angle_satellite.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Check all points and angles (method 1 and 2) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

mapview_no <- function(i){
  mapview(get_trapeze_polygon(i, df_sites, angle_satellite), map.types = "Esri.WorldImagery") +
    mapview(get_trapeze_polygon(i, df_sites, angles), col.regions = "red")
}

# Run through all trapezes using this:
mapview_no(6)

# For adustment af angle or point (below) using editmap: 
# click on marker icon (left egde), click once in desired place in map, then click 'done'

# To adjust angle:
i <- 30
angle_satellite[i] <- get_angle_from_click_satellite(i)
# saveRDS(angle_satellite, "Data/02_angle_satellite.rds")

# To adjust point:
i <- 6
new_point_feature <- editMap(mapview_site(i))$finished
new_point <- new_point_feature$geometry %>% st_transform(3035) 
df_sites[i,]$geometry <- new_point
# saveRDS(df_sites, "Data/02_df_sites.rds")


# keep these 'angles' after manual check
# i <- c(2, 12, 14, 16, 20, 22, 24, 26, 28, 32, 34, 36)
# angle_satellite[i] <- angles[i]

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Save adjusted coordinates and angles in Excel ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Copy coordinates to clipboard - after that, pasted into 'STAR-WALK sites adjusted.xlsx'
# columns 'Nlatitude NEW',	'Elongitude NEW'
for_copy <- df_sites %>% 
  st_transform(crs = 4326) %>% 
  st_coordinates() %>%
  .[,c(2,1)] %>%
  as.data.frame()

for_copy$Angle <- angle_satellite

write.table(for_copy, "clipboard", sep = "\t", dec = ",", col.names = FALSE, row.names = FALSE)


# for (i in 1:nrow(df_sites)){
#   mapview(get_trapeze_polygon(i, df_sites, angle_satellite)) +
#    mapview(get_trapeze_polygon(i, df_sites, angles), col.regions = "red")
#   ok <- readline(prompt = "Hit 'y' if OK, 'n' otherwise ")
#   }



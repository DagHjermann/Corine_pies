# install.packages("mapedit")

#
# Interactive editing of positions (on a satellite map)
#

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Libraries + data  ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

library(mapedit)
library(leaflet)

library(mapview)
library(dplyr)

source("04fun_mapview_functions.R")
source("02fun_Get_areas_functions.R")

## 1. Data ----
### Raster data
# folder_tif <- "C:/Data/Star-Walk/g100_clc12_V18_5"
# fn <- paste0(folder_tif, "/g100_clc12_V18_5.tif")
# raster_corine <- raster(fn)

### CORINE classes lookup table
corine_classes <- readxl::read_excel("Data/02_CORINE_classes.xlsx")

### Site data
df_sites <- readRDS("Data/02_df_sites.rds")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Do manual editing of points ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# Edit points for each lake
#

# Click correct position for each point on the lake:
df_sites_pick <- subset(df_sites, lake %in% "Ohrid")
nye_ohrid <- editMap(
  mapview(df_sites_pick, map.types = "Esri.WorldImagery")
)
save(nye_ohrid, "nye_ohrid.RData")

# Click correct position for each point on the lake:
df_sites_pick <- subset(df_sites, lake %in% "Prespa")
nye_prespa <- editMap(
  mapview(df_sites_pick, map.types = "Esri.WorldImagery")
)
save(nye_prespa, "nye_prespa.RData")

# Click correct position for each point on the lake:
df_sites_pick <- subset(df_sites, lake %in% "Lura")
nye_lura <- editMap(
  mapview(df_sites_pick, map.types = "Esri.WorldImagery")
)
save(nye_lura, "nye_lura.RData")

# Click correct position for each point on the lake:
df_sites_pick <- subset(df_sites, lake %in% "Sava")
nye_sava <- editMap(
  mapview(df_sites_pick, map.types = "Esri.WorldImagery")
)
save(nye_sava, "nye_sava.Rdata")

# Click correct position for each point on the lake:
df_sites_pick <- subset(df_sites, lake %in% "Biogradsko")
nye_biogradsko <- editMap(
  mapview(df_sites_pick, map.types = "Esri.WorldImagery")
)
save(nye_biogradsko, "nye_biogradsko.RData")

# Click correct position for each point on the lake:
df_sites_pick <- subset(df_sites, lake %in% "Crno")
nye_crno <- editMap(
  mapview(df_sites_pick, map.types = "Esri.WorldImagery")
)
save(nye_crno, "nye_crno.RData")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Find new positions corresponding to old ones and copy to Excel ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df_sites[]


# Check distances
library(sf)

# Return the p2 point (index 'closest_i") that is closest to p1 point number i
# Also return closest distance (in meter if crs = 3035 or a UTM)
find_closest <- function(i, points1, points2){
  # Find all distances (for all points2)
  dist <- sqrt((points2[,1] - points1[i,1])^2 + (points2[,2] - points1[i,2])^2)
  tibble(closest_i = which.min(dist), dist = min(dist))
}

# test
# p1 <- st_coordinates(df_sites_pick)
# p2 <- st_coordinates(nye_ohrid$finished %>% st_transform(crs = 3035))
# find_closest(1)

# Return the original positions and the closest-fitting new positions
match_positions <- function(points_orig, points_new){
  # Get coordinates - 3035 for distances in meters (p1a, p2a), 4326 (log-lat) for output
  p1a <- st_coordinates(points_orig)
  p1b <- st_coordinates(points_orig %>% st_transform(crs = 4326))
  p2a <- st_coordinates(points_new %>% st_transform(crs = 3035))
  p2b <- st_coordinates(points_new)
  # Run find_closest for all p1a points 
  df_dist <- 1:nrow(p1a) %>% purrr::map_df(find_closest, p1a, p2a)
  # Output p1b points, sorted p2b points, and the distances
  cbind(p1b, p2b[df_dist$closest_i,], df_dist)
}

#
# run for each lake
#
df <- match_positions(subset(df_sites, lake %in% "Ohrid"), nye_ohrid$finished)
df
df <- df[,c(2,1,4,3,5,6)]
write.table(df, "clipboard", dec = ",", sep = "\t", row.names = FALSE, col.names = FALSE)
# Paste into "STAR-WALK sites adjusted.xlsx"
# Mark the ones that have large distance and duplicated "closest_i" in red in Excel

df <- match_positions(subset(df_sites, lake %in% "Prespa"), nye_prespa$finished)
df
df <- df[,c(2,1,4,3,5,6)]
write.table(df, "clipboard", dec = ",", sep = "\t", row.names = FALSE, col.names = FALSE)
# Copy to Excel (as above)

df <- match_positions(subset(df_sites, lake %in% "Lura"), nye_lura$finished)
df
df <- df[,c(2,1,4,3,5,6)]
write.table(df, "clipboard", dec = ",", sep = "\t", row.names = FALSE, col.names = FALSE)
# Copy to Excel (as above)

df <- match_positions(subset(df_sites, lake %in% "Sava"), nye_sava$finished)
df
df <- df[,c(2,1,4,3,5,6)]
write.table(df, "clipboard", dec = ",", sep = "\t", row.names = FALSE, col.names = FALSE)
# Copy to Excel (as above)

df <- match_positions(subset(df_sites, lake %in% "Biogradsko"), nye_biogradsko$finished)
df
df <- df[,c(2,1,4,3,5,6)]
write.table(df, "clipboard", dec = ",", sep = "\t", row.names = FALSE, col.names = FALSE)
# Copy to Excel (as above)

df <- match_positions(subset(df_sites, lake %in% "Crno"), nye_crno$finished)
df
df <- df[,c(2,1,4,3,5,6)]
write.table(df, "clipboard", dec = ",", sep = "\t", row.names = FALSE, col.names = FALSE)
# Copy to Excel (as above)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Messed up Excel sheet and had to redo the matching ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

### Site data
df_sites <- readRDS("Data/02_df_sites.rds")
df_sites_new <- readxl::read_excel("Data/STAR-WALK sites adjusted.xlsx", sheet = "For R import")


# Function to make a separaet point from each line of data
# Note: don't make multipoints, then you cannot set data for each point
# See example from ?st_geometry
make_point <- function(i){
  df_sites_new[i,c("Elongitude NEW","Nlatitude NEW")] %>% 
    as.numeric() %>%
    st_point()
}
# test
# make_point(2)

# Make list of points
lines_with_data <- which(!is.na(df_sites_new[,"Elongitude NEW"]))
point_list <- lines_with_data %>% purrr::map(make_point)

# Turn site data into sf object; also set crs (lat / long)
df_sites_new <- df_sites_new[lines_with_data,]
st_geometry(df_sites_new) <- st_sfc(point_list, crs = 4326)


match_positions(df_sites[1:6,], df_sites_new[1:6,])
df <- match_positions(df_sites, df_sites_new)
df
df <- df[,c(2,1,4,3,5,6)]
write.table(df, "clipboard", dec = ",", sep = "\t", row.names = FALSE, col.names = FALSE)


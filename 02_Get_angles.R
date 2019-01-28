
### Libraries + functions
library(raster)
library(foreign)
library(sf)

source("01fun_plot_raster_functions.R")

### Raster data
folder_tif <- "C:/Data/Star-Walk/g100_clc12_V18_5"
fn <- paste0(folder_tif, "/g100_clc12_V18_5.tif")
raster_corine <- raster(fn)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Crop to Lake Ohrid ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

f3 <- extent(5212458, 5235640, 2036437, 2076818)
raster_ohrid <- crop(raster_corine, f3)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# plot_raster ----
# Not actually needed!
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

plot_raster <- function(raster, extend.x = 0, extend.y = 0, window_height = 8){
  rd <- rasterToImage(raster)
  df <- areaTab(raster)
  df$BinValues <- classToNum(df$BinValues)
  df <- df[order(df$BinValues),]
  ext.x <- extend.x*diff(range(rd$x))
  ext.y <- extend.y*diff(range(rd$y))
  xlim <- c(min(rd$x) - ext.x, max(rd$x) + ext.x)
  ylim <- c(min(rd$y) - ext.y, max(rd$y) + ext.y)
  aspect_ratio <- diff(xlim)/diff(ylim)
  windows(window_height*aspect_ratio, window_height)
  image(rd, useRaster = TRUE, breaks=c(0, df$BinValues+0.5), 
        col = df$Color, xlim = xlim, ylim = ylim)
}

plot_raster(raster_ohrid, extend.x = 0.4)

plot(raster_ohrid)

locator(1)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Get site coordinates ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df_sites <- readxl::read_excel("Data/STAR-WALK sites.xlsx")
df_sites

sites <- df_sites[,c("Elongitude","Nlatitude")] %>% 
  as.matrix() %>% 
  st_multipoint() %>%
  st_sfc(crs = 4326) %>%
  st_sf(df_sites[,c("lake", "site", "name")]) %>%
  st_transform(3035)                                # see https://land.copernicus.eu/pan-european/corine-land-cover/clc-2012?tab=metadata

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Plot map and sites ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

plot(raster_ohrid)
plot(sites, add = TRUE, pch = 20, col = 1)

plot(sites %>% filter(lake == "Ohrid"))
plot(subset(sites, lake == "Ohrid"))

plot(raster_ohrid)
plot(sites[sites$lake == "Ohrid", ], add = TRUE, pch = 20, col = 1)



sites[sites$lake == "Ohrid", ]

plot(raster_ohrid)


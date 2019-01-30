
### Libraries + functions
library(raster)
library(foreign)

source("05fun_plot_raster_functions.R")

### Raster data
folder_tif <- "C:/Data/Star-Walk/g100_clc12_V18_5"
fn <- paste0(folder_tif, "/g100_clc12_V18_5.tif")
raster_corine <- raster(fn)

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
# Plot Lake Ohrid
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

plotRaster(raster_ohrid, extend.x = 0.4)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Plot Lake Lura
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df_sites_pick <- subset(df_sites, lake %in% "Lura")
ext <- extent(df_sites_pick)
extra_x <- 1000
extra_y <- 1000
ext@xmin <- ext@xmin - extra_x
ext@xmax <- ext@xmax + extra_x
ext@ymin <- ext@ymin - extra_y
ext@ymax <- ext@ymax + extra_y
raster_pick <- crop(raster_corine, ext)

plotRaster(raster_pick, extend.x = 0.4)




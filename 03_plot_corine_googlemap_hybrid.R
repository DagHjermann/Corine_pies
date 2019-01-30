#
# From 
# "\\niva-of5\osl-userdata$\DHJ\Documents\seksjon 318\Star-Walk\Corine\read raster 2012.R"
#

# Cheat sheet for the 'raster' package:
# http://rpubs.com/etiennebr/visualraster

# update.packages("raster")
library(raster)
library(foreign)
# library(ggmap)
install.packages("dismo")
library(dismo)
library(RgoogleMaps)



# Plot overlay of categories over a Google map
#   Using raster::plot, not graphics::image, which is not compatible with par, so 
#   legend is plotted in a separate window (or in a separate file)
# Note that the raster map must be reprojected to the google map (Google pseudo-Mercator), 
#    which involves some interpolation. We use "method = 'ngb'" for nearest neghbor interpolation
# Note: "filename" must be given without the ".png" part (in order to make a file name for "legend")

plotRasterOverlay <- function(raster, type = "hybrid", zoom = 11, alpha = 0.5,
                              windows = TRUE, legend = TRUE, file = NULL){
  # googlemap <- gmap(x = raster, type = type, zoom = zoom)
  googlemap <- GetMap(x = raster, type = type, zoom = zoom)
  # Project to Google pseudo-Mercator, using "nearest neigbor"
  rasterT <- projectRaster(from = raster, crs = CRS("+init=epsg:3857"), method = 'ngb')  
  df <- areaTab(raster)
  df$BinValues <- classToNum(df$BinValues)
  df <- df[order(df$BinValues),]
  if (windows & is.null(file))
    windows(7,7)
  if (!is.null(file))
    png(paste0(file, ".png"), width = 17, height = 17, units = 'cm', 
        res = 150, type="cairo", antialias="default")
  plot(googlemap)
  plot(rasterT, add = T, legend = F, 
       breaks=c(0, df$BinValues+0.5), col = df$Color, alpha = alpha)
  if (!is.null(file))
    dev.off()
  if (legend){
    if (!is.null(file)){
      png(paste0(file, "_legend.png"), width = 17, height = 17, units = 'cm', 
          res = 150, type="cairo", antialias="default")
    } else {
      windows(7,7)
    }
    par( mar = c(2,2,2,2))
    plot(0,0, type="n", axes = FALSE, xlab = "", ylab = "")
    legend("topright", legend = paste0(df$Area_type2, " (", df$Perc, "%)"), fill = df$Color)
    if (!is.null(file))
      dev.off()
  }
}

# plot(r3)
# locator(2)

folder_tif <- "C:/Data/Star-Walk/g100_clc12_V18_5"
fn <- paste0(folder_tif, "/g100_clc12_V18_5.tif")
raster_corine <- raster(fn)

ext_ohrid_town <- extent(5223158, 5232004, 2058987, 2071310)
raster_crop <- crop(raster_corine, ext_ohrid_town)
# plot(r4)
plot(raster_crop)
table(raster_crop@data@values)
raster_crop

plotRasterOverlay(raster_crop, zoom = 12)
# plotRasterOverlay(r4, zoom = 12, type = 'hybrid', file = "Figures/Test_Corine_Ohrid_town_hybrid", legend = TRUE)
# plotRasterOverlay(r4, zoom = 12, type = 'roadmap', file = "Figures/Test_Corine_Ohrid_town_roadmap", legend = FALSE)
# plotRasterOverlay(r4, zoom = 12, type = 'terrain', file = "Figures/Test_Corine_Ohrid_town_terrain", legend = FALSE)


######################################################################
#
# Functions
#
######################################################################

# Crop with mouse (click in oppoiste corners of zoom area)
# Returns a new raster object
# Can only be used on maps plotted using "plot(raster)"! (does not work with plotRaster() below())

cropMouse <- function(raster){
  P <- locator(2)
  f <- extent(min(P$x), max(P$x), min(P$y), max(P$y))
  cat(c(min(P$x), max(P$x), min(P$y), max(P$y)), "\n")
      crop(raster, f)
}

# Makes a new area table , with only the area classes found in the actual raster object
areaTab <- function(raster){
  area_tab <- table(raster[])
  area_types <- as.numeric(names(area_tab))
  df <- data.frame(area_tab)
  colnames(df)[1] <- "BinValues"
  df2 <- merge(df, df_classes, all.x = TRUE, all.y = FALSE)
  df2$Perc <- with(df2, round(Freq/sum(Freq)*100, 1))
  df2
  }

# Get CORINE raster for data (e.g. point data) pluss 1000 m to each side
get_raster_pick <- function(data,
                            dx = 1000, dy = 1000,
                            raster = raster_corine){
  ext <- extent(data)
  ext@xmin <- ext@xmin - dx
  ext@xmax <- ext@xmax + dx
  ext@ymin <- ext@ymin - dy
  ext@ymax <- ext@ymax + dy
  crop(raster_corine, ext)
}

# Makes a file suitable for plotting with image()
# Input: raster object
# Output: list with x vector, y vector and a z matrix
# - note how we find the center coordinates for x and y 

rasterToImage <- function(r){
  X <- matrix(r[], nrow(r), ncol(r), byrow = TRUE)
  z <- t(X)[,seq(nrow(r),1,-1)]
  list(x = seq(xmin(r) + 50, xmax(r) - 50, 100),
  y = seq(ymin(r) + 50, ymax(r) - 50, 100),
  z = z)
  }

classToNum <- function(x)
  as.numeric(levels(x)[as.numeric(x)])


# Plot with legend, using graphics::image, not raster::plot
#   (We used graphics::image in order to get matching colours on map and legend. Found out later that this can be solved using,
#    raster::plot, see plotRasterOverlay below. However, graphics::image is also compatible with par(mfrow=...)
#    which allows legend on the same page.)
# The parameters extend.x and extend.y adds space on left/right side or at the top/bottom, as a very simplistic way of 
#   changing the aspect of the map manually (see below). Make a plot using "plot(raster)" in order to have a map 
#   with correct aspect to compare with
# If it gives no map, just the legend, just run it again :-)

plotRaster <- function(raster, windows = TRUE, legend = TRUE, file = NULL,
                       extend.x = NULL, extend.y = NULL, pointdata = NULL){
  rd <- rasterToImage(raster)
  df <- areaTab(raster)
  df$BinValues <- classToNum(df$BinValues)
  df <- df[order(df$BinValues),]
  if (windows & is.null(file))
    windows(14,7)
  if (!is.null(file))
    png(file, width = 35, height = 17, units = 'cm', 
        res = 150, type="cairo", antialias="default")
  if (legend)
    par(mfcol = c(1,2), mar = c(4,5,2,1))
  if (is.null(extend.x) & is.null(extend.y)){
    x_y_ratio <- diff(range(rd$x))/diff(range(rd$y))
    if (x_y_ratio > 1){
      extend.x <- 0; extend.y <- 1 - x_y_ratio
    } else {
      extend.x <- 1 - x_y_ratio; extend.y <- 0
    }
  } else {
    extend.x <- ifelse(is.null(extend.x), 0, extend.x)
    extend.y <- ifelse(is.null(extend.y), 0, extend.y)
  }
  ext.x <- extend.x*diff(range(rd$x))
  ext.y <- extend.y*diff(range(rd$y))
  image(rd, useRaster = TRUE, breaks=c(0, df$BinValues+0.5), col = df$Color,
        xlim = c(min(rd$x) - ext.x, max(rd$x) + ext.x),
        ylim = c(min(rd$y) - ext.y, max(rd$y) + ext.y))
  if (!is.null(pointdata)){
    points(pointdata)
  }
  if (legend){
    plot(0,0, type="n", axes = FALSE, xlab = "", ylab = "")
    legend("topright", legend = paste0(df$Area_type2, " (", df$Perc, "%)"), fill = df$Color)
  }
  if (!is.null(file))
    dev.off()
}


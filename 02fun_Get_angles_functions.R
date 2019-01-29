
get_angle <- function(dx,dy){
  k <- atan(dx/dy)/(2*pi)*360
  if (dx < 0 & dy > 0){
    k <- 360 + k
  } else if (dy < 0){
    k <- 180 + k
  }
  k
}
# get_angle(1,0)
# get_angle(1,1)

point_from_angle <- function(x,y,angle,distance){
  new_x <- x + sin(angle/360*2*pi)*distance
  new_y <- y + cos(angle/360*2*pi)*distance
  c(new_x, new_y)
}

# Test
# point_from_angle(10,10,45,3)
# point_from_angle(10,10,225,3)
# point_from_angle(10,10,225,5)


angle_sum <- function(angle1, angle2){
  sum <- angle1 + angle2
  if (sum < 0){
    sum <- sum + 360
  } else if (sum > 360){
    sum <- sum - 360
  }
  sum
}

# Test
# angle_sum(340, 40)
# angle_sum(45, -60)

get_triangle_from_angle <- function(coor, angle, triangle_size = 400, triangle_angle = 45){
  angle1 <- angle_sum(angle, triangle_angle/2)
  angle2 <- angle_sum(angle, -triangle_angle/2)
  triangle_edge1 <- point_from_angle(coor[1], coor[2], angle1, triangle_size)
  triangle_edge2 <- point_from_angle(coor[1], coor[2], angle2, triangle_size)
  polygon <- rbind(coor, triangle_edge1, triangle_edge2, coor)
  polygon
}
# get_triangle_from_angle(coor = c(1000, 1000), 270)

# As above, but makes the more advance kind of triangle (called a trapeze)
get_trapeze_from_angle <- function(coor, angle, 
                                   length_along_shore = 100, distance_from_shore = 500, width_of_trapeze = 300){
  angle1 <- angle_sum(angle, 90)
  angle2 <- angle_sum(angle, -90)
  shore_point1 <- point_from_angle(coor[1], coor[2], angle1, length_along_shore/2)
  shore_point2 <- point_from_angle(coor[1], coor[2], angle2, length_along_shore/2)
  triangle_midpoint <- point_from_angle(coor[1], coor[2], angle, distance_from_shore)
  triangle_midpoint <- c(triangle_midpoint[1], triangle_midpoint[2])
  inland_point1 <- point_from_angle(triangle_midpoint[1], triangle_midpoint[2], angle1, width_of_trapeze/2)
  inland_point2 <- point_from_angle(triangle_midpoint[1], triangle_midpoint[2], angle2, width_of_trapeze/2)
  # browser()
  polygon <- rbind(shore_point1, shore_point2, inland_point2, inland_point1, shore_point1)
  polygon
}

# debugonce(get_trapeze_from_angle)
# get_trapeze_from_angle(coor = c(1000, 1000), 270)

# Get extent for a single site
extent_site <- function(coor, dm = 1000){
  extent(coor[1] - dm, coor[1] + dm, coor[2] - dm, coor[2] + dm)
}
# coor <- df_sites[1,] %>% st_coordinates()
# extent_site(coor)

# Plot single-site map ----
plot_raster_site <- function(coor, dm = 2000, raster = raster_corine){
  plot(raster, ext = extent_site(coor, dm = dm))
  points(coor, pch = 19, col = "black", cex = 1)
}
# coor <- df_sites[1,] %>% st_coordinates()
# plot_raster_site(coor)

# Plot site number 'i' and prompts user to click on map at
#   a point perpendicular from the coastline (distance is irrelevant)
# A triangle is plotted but not used
get_angle_from_click <- function(i, dm = 2000, 
                                 length_along_shore = 100, distance_from_shore = 500, width_of_trapeze = 300,
                                 raster = raster_corine, data = df_sites){
  coor <- data[i,] %>% st_coordinates()
  plot_raster_site(coor, raster = raster)
  p <- locator(1)
  lines(c(coor[,1], p$x), c(coor[,2], p$y), col = "black", lty = "dashed")
  angle <- get_angle(p$x - coor[,1], p$y - coor[,2])
  trapeze <- get_trapeze_from_angle(coor, angle)
  # print(angle)
  # print(get_triangle_from_angle(coor, angle))
  lines(trapeze[,1], trapeze[,2], col = "white", lwd = 3)
  lines(trapeze[,1], trapeze[,2], col = 2)
  angle
}
# Test
# debugonce(get_angle_from_click)
# get_angle_from_click(1)



plot_site_trapeze <- function(i, angle_vector = angles,
                              length_along_shore = 100, distance_from_shore = 500, width_of_trapeze = 300,
                              data = df_sites, raster = raster_corine){
  coor <- data[i,] %>% st_coordinates()
  ext <- extent_site(coor)
  raster_site <- crop(raster, ext)
  data_df <- as.data.frame(data)
  title <- paste0("Site ", i, ": ", data_df[i,"lake"], ", ", data_df[i,"site"], " (", data_df[i,"name"], ")")
  plot(raster_site, axes = TRUE, main = "")
  mtext(title, cex = 1.2, line = 0.5)
  points(coor, pch = 19, col = "white", cex = 2)
  points(coor, pch = 19, col = 2)
  trapeze <- get_trapeze_from_angle(coor, angle_vector[i],
                                    length_along_shore = length_along_shore, 
                                    distance_from_shore = distance_from_shore,
                                    width_of_trapeze = width_of_trapeze)
  lines(trapeze, col = "white", lwd = 3)
  lines(trapeze, col = "red", lwd = 1)
}
# plot_site_trapeze(1)


get_trapeze_from_site <- function(i, data = df_sites, angle_vector = angles,
                                  length_along_shore = 100, distance_from_shore = 500, width_of_trapeze = 300
                                  ){
  coor <- data[i,] %>% st_coordinates()
  trapeze_data <- get_trapeze_from_angle(coor, angle_vector[i],
                                         length_along_shore = length_along_shore, 
                                         distance_from_shore = distance_from_shore,
                                         width_of_trapeze = width_of_trapeze)
  trapeze_sf <- st_polygon(list(trapeze_data)) 
  trapeze_sfc <- st_sfc(trapeze_sf, crs = st_crs(data))
  trapeze_sfc
}
# debugonce(get_trapeze_from_site)
# get_trapeze_from_site(1)
# plot(get_trapeze_from_site(1))


get_trapezeoverlap_from_site <- function(i, 
                                         length_along_shore = 100, distance_from_shore = 500, width_of_trapeze = 300,
                                         data = df_sites, raster = raster_corine, angle_vector = angles){
  trapeze <- get_trapeze_from_site(i=i, data = data, angle_vector = angle_vector,
                                   length_along_shore = length_along_shore, 
                                   distance_from_shore = distance_from_shore,
                                   width_of_trapeze = width_of_trapeze)
  coor <- data[i,] %>% st_coordinates()
  ext <- extent_site(coor)
  raster_site <- crop(raster, ext)
  raster_poly01 <- rasterToPolygons(raster_site)
  raster_poly02 <- raster_poly01 %>%
    st_as_sfc(crs = st_crs(df_sites)) %>%
    st_sf(x = as.data.frame(raster_poly01)[,1])
  overlap <- st_intersection(raster_poly02, trapeze)
  overlap$Land_use <- factor(overlap$x)
  overlap
  }

# trapeze_types <- get_trapezeoverlap_from_site(1)trapeze_types
# trapeze_types
# plot(trapeze_types[,"Land_use"])


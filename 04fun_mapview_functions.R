# Functions
# Get CORINE raster for lake pluss 1000 m to each side
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
# raster_pick <- get_raster_pick(df_sites_pick)

# Get correct colors
get_corine_values <- function(raster, corine_variable, corine_lookuptable = corine_classes){
  values <- unique(raster@data@values) %>% sort()
  df <- subset(corine_lookuptable, BinValues %in% values) %>% arrange(BinValues)
  df[[corine_variable]]
}
# cols <- raster_pick %>% get_corine_values("Color")
# area_type2 <- raster_pick %>% get_corine_values("Area_type2")

# Get raster data frame, i.e. the values of the raster in dataframe format, plus associated
# variables
get_raster_dataframe <- function(raster, corine_variables = c("CLC_CODE", "Area_type2"), 
                                 corine_lookuptable = corine_classes){
  rastervector <- raster %>% as.matrix() %>% as.numeric() 
  sort <- 1:length(rastervector)
  tibble(Sort = sort, BinValues = rastervector) %>%
    left_join(as.data.frame(corine_lookuptable)[,c("BinValues", corine_variables)], by = "BinValues") %>%
    arrange(Sort)
}
# raster_df <- raster_pick %>% get_raster_dataframe()

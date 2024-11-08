### Atlantis_3d: visualising an Atlantis polygon map in three dimensions

## Author: I. Stollberg, 2024

# libraries 
libs <- c("rayshader", "tidyverse", "sf", 
          "classInt", "giscoR", "terra", "exactextractr", "ggplot2")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# define longlat CRS
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
shp.file <- "EAA29_cleaned_v2.shp"
cum.depths <- c(20, 50, 100, 200, 300, 400, 750, 1000, 2000, 5000) # cumulative water layer depths

# draw.3dmap: function that extracts polygon coordinates from .shp file and maps them
draw.3dmap <- function(shp.file, depth.layers){
  data <- st_read(shp.file)
  
  # Categorize the depths using the breaks in cum.depths
  data$depth_category <- cut(-data$botz, breaks = c(0, depth.layers, Inf), 
                             labels = paste0("Layer_", 1:(length(depth.layers) + 1)), 
                             include.lowest = TRUE)

  pp <- ggplot(data) +
    geom_sf(aes(fill = -botz)) +
    coord_sf(crs = crsLONGLAT)

  return(pp)
}


pp <- draw.3dmap("EAA29_cleaned_v2.shp", cum.depths)

plot_gg(pp,
        triangulate = T,
        multicore = T,
        width=5,
        height=5,
        scale=200,
        offset_edges=T,
        windowsize=c(1400,866),
        zoom = .4, 
        theta = -30)

###############
# draw.3dmap: function that extracts polygon coordinates from .shp file and maps them
draw.3dmap <- function(shp.file, depth.layers){
  # Read the shapefile
  data <- st_read(shp.file)
  
  # Plot using ggplot
  pp <- ggplot(data) +
    geom_sf(aes(fill = botz)) +  # Use depth_category to differentiate layers
    coord_sf(crs = crsLONGLAT) +
    scale_fill_brewer(palette = "Blues", direction = 1)
  
  return(pp)
}

# Generate the 3D plot
pp <- draw.3dmap("EAA29_cleaned_v2.shp", cum.depths)

# Use plot_gg to create a 3D visualization with rayshader
plot_gg(pp,
        multicore = TRUE,
        width = 5,
        height = 5,
        scale = -200,
        offset_edges = TRUE,
        windowsize = c(1400, 866),
        zoom = 0.4,
        theta = -30)

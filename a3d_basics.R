### Atlantis_3d: visualising an Atlantis polygon map in three dimensions

## Author: I. Stollberg, 2024

# libraries 
libs <- c("rayshader", "tidyverse", "sf", 
          "classInt", "giscoR", "terra", "exactextractr")

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

# draw.3dmap: function that extracts polygon coordinates from .shp file and maps them 
draw.3dmap <- function(shp.file){
  data <- st_read(shp.file)
  
  pp <- ggplot(data) +
    geom_sf(aes(fill = botz)) +
    coord_sf(crs = crsLONGLAT)
    
  return(pp)
}

draw.3dmap("EAA29_cleaned_v2.shp")

plot_gg(pp,
        multicore = T,
        width=5,
        height=5,
        scale=200,
        offset_edges=T,
        adjust_extrude_depth = 0.5,  # avoid overly sharp extrusions
        windowsize=c(1400,866),
        zoom = .4, 
        phi = 30, 
        theta = -30)

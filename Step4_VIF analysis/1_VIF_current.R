#
# VIF
library(raster)
library(tidyverse)
library(usdm)
library(stringr)

# Current Data -------------------------------------------------------------------
# Variable path, make sure the paths 
# in current.csv are consistent with your paths
rst_path <- read_csv('current.csv')[[1]]

# Raster
current <- stack(rst_path)
plot(current[[2]])

# Study area
study_area <- shapefile("your path to\\3alphahull\\Aconitum heterophyllum_hull.shp")
plot(study_area, add=T)

# Tiles change the extent of the original raster object, but result in a rectangular extent;
# The mask does not change the extent of the original raster object, it simply turns the data outside the vector extent null.

# Extraction by mask (inside)
var_all <- raster::mask(raster::crop(current, study_area), study_area)
names(var_all) <- stringr::str_replace(basename(rst_path), '.tif', '')
plot(var_all[[2]])
plot(study_area, add=TRUE)

# Convert to data frame object
var_all_df <- raster::as.data.frame(var_all)

# VIF Remove collinearity
vif_re <- usdm::vifstep(var_all_df, 5)

# Write out results
for (var in names(var_all)){
  if (var %in% vif_re@results$Variables) {
    writeRaster(var_all[[var]],
                paste0('Current_autocorrelated/', var, '.tif')
    )}
}

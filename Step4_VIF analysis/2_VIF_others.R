#
# VIF
library(raster)
library(tidyverse)
library(usdm)
library(stringr)

# LGM -------------------------------------------------------------------
# Variable path
rst_path <- read_csv('LGM.csv')[[1]]
# Raster
rst_LGM <- stack(rst_path)
plot(rst_LGM[[2]])
# Study area
study_area <- shapefile("your path to\\3alphahull\\Aconitum heterophyllum_hull.shp")

# Extraction by mask (inside)
var_all <- mask(crop(rst_LGM, study_area), study_area)
names(var_all) <- str_replace(basename(rst_path), '.tif', '')

# Write out results
for (var in names(var_all)) {
  writeRaster(var_all[[var]],
              paste0('LGM_autocorrelated/', var, '.tif'))
}


# SSP126_2050
rst_path <- read_csv('SSP126_2050.csv')[[1]]
# Raster
rst_SSP126_2050 <- stack(rst_path)

# Study area
study_area <- shapefile("your path to\\3alphahull\\Aconitum heterophyllum_hull.shp")

# Extraction by mask (inside)
var_all <- mask(crop(rst_SSP126_2050, study_area), study_area)
names(var_all) <- str_replace(basename(rst_path), '.tif', '')

# Write out results
for (var in names(var_all)) {
  writeRaster(var_all[[var]],
              paste0('SSP126_2050_autocorrelated/', var, '.tif'))
}


# SSP585_2050
rst_path <- read_csv('SSP585_2050.csv')[[1]]
# Raster
rst_SSP585_2050 <- stack(rst_path)

# Study area
study_area <- shapefile("your path to\\3alphahull\\Aconitum heterophyllum_hull.shp")

# Extraction by mask (inside)
var_all <- mask(crop(rst_SSP585_2050, study_area), study_area)
names(var_all) <- str_replace(basename(rst_path), '.tif', '')

# Write out result

library(alphahull)
library(terra)
library(rangeBuilder)
library(raster)
library(sf)

# "Aconitum heterophyllum" is an example species
point_2_alphahull_line <-
  function(csv_path, alpha = 2.5,
           long = 'decimalLongitude',
           lat = 'decimalLatitude',
           crs = 'EPSG:4326',
           point_shp = 'Aconitum heterophyllum.shp',
           hull_shp = 'Aconitum heterophyllum_hull.shp') {
    
    data <- read.csv(csv_path, encoding = 'UTF-8')
    # Write out point file
    point <- vect(data, geom=c(long, lat), crs=crs)
    
    writeVector(point, point_shp, overwrite=TRUE)
    
    # Alphahull analysis
    ahull.obj <- ashape(data[, c(long, lat)], alpha = alpha)
    plot(ahull.obj)
    data_hull <- as.data.frame(ahull.obj$alpha)
    hull_point <- vect(data_hull, geom=c('c1', 'c2'), crs=crs)
    plot(hull_point)
    b <- as.lines(hull_point)
    plot(b)
    hull_polygon <- as.polygons(as.lines(hull_point))
    plot(hull_polygon)
    writeVector(hull_polygon, hull_shp, overwrite=TRUE)
    
    plot(point)
    plot(hull_polygon, add=TRUE)
  }

point_2_alphahull_curve <-
  function(csv_path,
           long = 'decimalLongitude',
           lat = 'decimalLatitude',
           crs = 'EPSG:4326',
           point_shp = 'Aconitum heterophyllum.shp',
           hull_shp = 'Aconitum heterophyllum_hull.shp') {
    data <- read.csv(csv_path, encoding = 'UTF-8')
    # Write out point file
    point <- vect(data, geom = c(long, lat), crs = crs)
    writeVector(point, point_shp, overwrite = TRUE)
    
    # Alphahull analysis
    range <- getDynamicAlphaHull(data,
                                 coordHeaders = c(long, lat),
                                 clipToCoast = 'no',
                                 fraction = 1,
                                 partCount = 1,
                                 buff = 200000)
    
    plot(range[[1]], col = transparentColor('dark green', 0.5), border = NA)
    points(data[, c(long, lat)], cex = 0.5, pch = 3)
    
    # 导出结果
    st_write(range[[1]], hull_shp, overwrite = TRUE)
    # shapefile(range[[1]], hull_shp, overwrite = TRUE)
  }


# main --------------------------------------------------------------------
csv_path <- "Aconitum heterophyllum_thin1.csv"
point_2_alphahull_curve(csv_path, 'decimalLongitude', 'decimalLatitude',
                        hull_shp = 'Aconitum heterophyllum_hull.shp')

         
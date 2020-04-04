lr <- lazyraster::lazyraster(raadtools::topofile("gebco_19"))
# class         : LazyRaster
# dimensions    : 43200, 86400 (nrow, ncol)
# resolution    : 0.004166667, 0.004166667 (x, y)
# extent        : -180.0000,  180.0000,  -90.0000,   90.0000 (xmin, xmax, ymin, ymax)
# crs           : <placeholder>
#   values        : NA, NA (min, max - range from entire extent)
# window extent : <whole extent>
#   window index  : <->
#
library(lazyraster)
gebco <- as_raster(lr, dim = c(720L, 360L))
library(raster)
m <- raster::getValues(gebco[[1]], format = "matrix")
m[] <- round(m)
mode(m) <- "integer"
gebco1 <- setExtent(raster(m), gebco)
projection(gebco1) <- projection(gebco)
gebco <- gebco1

#writeRaster(gebco, "gebco.tif", datatype = "INT2S",  options = "COMPRESS=DEFLATE", overwrite = TRUE)
saveRDS(gebco, "xz.rds" ,compress = "xz")
gebco <- readRDS("data-raw/xz.rds")
usethis::use_data(gebco, compress = "xz")

#system(sprintf("gdalinfo %s", raadtools::topofile("gebco_19")))
#
# Driver: netCDF/Network Common Data Format
# Files: /rdsi/PUBLIC/raad/data/www.bodc.ac.uk/data/open_download/gebco/GEBCO_15SEC/zip/GEBCO_2019.nc
# Size is 86400, 43200
# Origin = (-180.000000000000000,90.000000000000000)
# Pixel Size = (0.004166666666667,-0.004166666666667)
# Metadata:
#   elevation#long_name=Elevation relative to sea level
# elevation#sdn_parameter_name=Sea floor height (above mean sea level) {bathymetric height}
# elevation#sdn_parameter_urn=SDN:P01::ALATZZ01
# elevation#sdn_uom_name=Metres
# elevation#sdn_uom_urn=SDN:P06::ULAA
# elevation#standard_name=height_above_reference_ellipsoid
# elevation#units=m
# lat#axis=Y
# lat#long_name=latitude
# lat#sdn_parameter_name=Latitude north
# lat#sdn_parameter_urn=SDN:P01::ALATZZ01
# lat#sdn_uom_name=Degrees north
# lat#sdn_uom_urn=SDN:P06::DEGN
# lat#standard_name=latitude
# lat#units=degrees_north
# lon#axis=X
# lon#long_name=longitude
# lon#sdn_parameter_name=Longitude east
# lon#sdn_parameter_urn=SDN:P01::ALONZZ01
# lon#sdn_uom_name=Degrees east
# lon#sdn_uom_urn=SDN:P06::DEGE
# lon#standard_name=longitude
# lon#units=degrees_east
# NC_GLOBAL#comment=The data in the GEBCO_2019 Grid should not be used for navigation or any purpose relating to safety at sea.
# NC_GLOBAL#Conventions=CF-1.6
# NC_GLOBAL#history=Information on the development of the data set and the source data sets included in the grid can be found in the data set documentation available from https://www.gebco.net
# NC_GLOBAL#institution=On behalf of the General Bathymetric Chart of the Oceans (GEBCO), the data are held at the British Oceanographic Data Centre (BODC).
# NC_GLOBAL#node_offset=1
# NC_GLOBAL#references=DOI: 10.5285/836f016a-33be-6ddc-e053-6c86abc0788e
# NC_GLOBAL#source=The GEBCO_2019 Grid is the latest global bathymetric product released by the General Bathymetric Chart of the Oceans (GEBCO) and has been developed through the Nippon Foundation-GEBCO Seabed 2030 Project. This is a collaborative project between the Nippon Foundation of Japan and GEBCO. The Seabed 2030 Project aims to bring together all available bathymetric data to produce the definitive map of the world ocean floor and make it available to all.
# NC_GLOBAL#title=The GEBCO_2019 Grid - a continuous terrain model for oceans and land at 15 arc-second intervals
# Corner Coordinates:
#   Upper Left  (-180.0000000,  90.0000000)
# Lower Left  (-180.0000000, -90.0000000)
# Upper Right ( 180.0000000,  90.0000000)
# Lower Right ( 180.0000000, -90.0000000)
# Center      (   0.0000000,   0.0000000)
# Band 1 Block=86400x1 Type=Float32, ColorInterp=Undefined
# NoData Value=9.96920996838686905e+36
# Unit Type: m
# Metadata:
#   long_name=Elevation relative to sea level
# NETCDF_VARNAME=elevation
# sdn_parameter_name=Sea floor height (above mean sea level) {bathymetric height}
# sdn_parameter_urn=SDN:P01::ALATZZ01
# sdn_uom_name=Metres
# sdn_uom_urn=SDN:P06::ULAA
# standard_name=height_above_reference_ellipsoid
# units=m

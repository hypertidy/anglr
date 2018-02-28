#export afile=/rdsi/PRIVATE/raad/data_local/www.bodc.ac.uk/gebco/GEBCO_2014_2D.nc
#gdal_translate $afile inst/extdata/gebco1.tif -outsize 2% 2% -co COMPRESS=LZW -co TILED=NO -r bilinear -a_srs 4326

gebco1 <- raster::readAll(raster::raster("inst/extdata/gebco1.tif"))
#projection(gebco1) <- "+init=epsg:4326"
usethis::use_data(gebco1)

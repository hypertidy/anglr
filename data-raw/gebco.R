#export afile=/rdsi/PRIVATE/raad/data_local/www.bodc.ac.uk/gebco/GEBCO_2014_2D.nc
#gdal_translate $afile inst/extdata/gebco1.tif -outsize 2% 2% -co COMPRESS=LZW -co TILED=NO -r bilinear
context("sf")

## TODO: Rename context
## TODO: Add more tests
# library(sf)
# 
# pol <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
# lin <- st_cast(pol, "MULTILINESTRING")
# pnt <- st_cast(lin, "MULTIPOINT")
data("sf_data_zoo", package = "rangl")

test_that("rangl sf works", {
  #rangl(sf_data_zoo$point)
  #rangl(sf_data_zoo$multipoint) 
  #rangl(sf_data_zoo$linestring) 
  #rangl(sf_data_zoo$multilinestring)  
  rangl(sf_data_zoo$polygon)  
  rangl(sf_data_zoo$multipolygon)  
})

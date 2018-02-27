context("sf")

## TODO: Rename context
## TODO: Add more tests
# library(sf)
# 
# pol <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
# lin <- st_cast(pol, "MULTILINESTRING")
# pnt <- st_cast(lin, "MULTIPOINT")
data("sf_data_zoo", package = "anglr")

test_that("anglr sf works", {
  #anglr(sf_data_zoo$point)
  #anglr(sf_data_zoo$multipoint) 
  #anglr(sf_data_zoo$linestring) 
  #anglr(sf_data_zoo$multilinestring)  
  expect_that(anglr(sf_data_zoo$polygon), is_a("trimesh"))
  expect_that(anglr(sf_data_zoo$multipolygon)  , is_a("trimesh"))
})

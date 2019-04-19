context("test-extent-example.R")

# Create an extent to triangulate
library(sf)
library(anglr)
library(raster)
my_extent <- st_as_sf(as(extent(c(153.185183093, 153.19443135, -27.705328446, -27.6967222119999)), 
                         "SpatialPolygons"))
test_that("setting max area makes more triangles", {
  ## no change
  expect_that(nrow(DEL(my_extent, max_area = 0.008)$triangle), 
              equals(nrow(DEL(my_extent, max_area = 0.0008)$triangle)))
expect_true(nrow(DEL(my_extent, max_area = 0.000008)$triangle) >
             nrow(DEL(my_extent, max_area = 0.008)$triangle))
})
test_that("dataframes with only geometry are handled", {
  expect_that(DEL(my_extent, max_area = 0.008), is_a("TRI"))
})

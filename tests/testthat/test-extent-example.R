context("test-extent-example.R")

# Create an extent to triangulate
library(anglr)
library(raster)
library(sfheaders)
my_extent <- sf_polygon(data.frame(x = c(154, 155, 155, 154), y = c(-27, -27, -26, -27),
                                polygon_id = 1, linestring_id = 1),
                        x = "x", y = "y",
                        polygon_id = "polygon_id", linestring_id = "linestring_id")

test_that("setting max area makes more triangles", {
  ## exact count is a property of the triangulator (Triangle gives 96, CDT 99)
  n <- nrow(DEL(my_extent, max_area = 0.008)$triangle)
  if (identical(getOption("anglr.triangulator", "RTriangle"), "RTriangle")) {
    expect_equal(n, 96L)
  } else {
    expect_true(n >= 90L && n <= 110L)
  }
expect_true(nrow(DEL(my_extent, max_area = 0.00008)$triangle) >
             nrow(DEL(my_extent, max_area = 0.008)$triangle))
})
test_that("dataframes with only geometry are handled", {
  expect_that(DEL(my_extent, max_area = 0.008), is_a("TRI"))
})

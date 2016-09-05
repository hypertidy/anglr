context("lines")

## TODO: Rename context
## TODO: Add more tests

library(spbabel)
sph <- sp(holey)
spl <- as(sph, "SpatialLinesDataFrame")

test_that("lines works", {
  expect_that(rangl(spl), is_a("linemesh"))
  expect_that(rangl(geometry(spl)), is_a("linemesh"))
  
})

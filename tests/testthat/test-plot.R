library(testthat)
context("plot")

## TODO: Rename context
## TODO: Add more tests

library(spbabel)
sph <- sp(holey)
spl <- as(sph, "SpatialLinesDataFrame")
spp <- as(spl, "SpatialMultiPointsDataFrame")
sp0 <- as(spl, "SpatialPointsDataFrame")
splz <- rangl(spl)
splz$v$z_ <- rnorm(nrow(splz$v))

test_that("plot works", {
   expect_that(names(plot(rangl(spl))), equals(c("v", "it")))
  expect_that(plot(rangl(sph)), is_a("mesh3d"))
  expect_that(plot(rangl(sp0)), throws_error("you don't really need this function"))
  expect_that(plot(splz), is_a("list"))
  
})

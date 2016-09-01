library(testthat)

context("points")

## TODO: Rename context
library(rangl)
library(maptools)
data(wrld_simpl)
pts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialPointsDataFrame")
mpts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame")
test_that("points works", {
  expect_that(mesh(pts), throws_error("you don't really"))
})


test_that("multi-points works", {
  expect_that(mesh(mpts), is_a("pointsmesh"))
})

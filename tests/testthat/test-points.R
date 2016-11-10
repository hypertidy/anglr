library(testthat)

context("points")

## TODO: Rename context
library(rangl)
library(maptools)
data(wrld_simpl)
pts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialPointsDataFrame")
mpts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame")
test_that("points works", {
  expect_silent(rangl(pts))
})


test_that("multi-points works", {
  expect_that(rangl(mpts), is_a("pointmesh"))
  expect_that(rangl(geometry(mpts)), is_a("pointmesh"))
})

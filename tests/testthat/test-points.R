library(testthat)

context("points")

## TODO: Rename context
library(anglr)
library(maptools)
data(wrld_simpl)
pts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialPointsDataFrame")
mpts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame")
test_that("points works", {
  expect_silent(anglr(pts))
})


test_that("multi-points works", {
  expect_that(anglr(mpts), is_a("pointmesh"))
  expect_that(anglr(geometry(mpts)), is_a("pointmesh"))
})

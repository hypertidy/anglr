library(testthat)

context("points")

## TODO: Rename context
library(anglr)

data(simpleworld)
pts <- as(as(simpleworld, "SpatialLinesDataFrame"), "SpatialPointsDataFrame")
mpts <- as(as(simpleworld, "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame")
test_that("points works", {
  expect_silent(anglr(pts))
})


test_that("multi-points works", {
  expect_that(anglr(mpts), is_a("pointmesh"))
  expect_that(anglr(geometry(mpts)), is_a("pointmesh"))
})

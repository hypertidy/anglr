library(testthat)
context("deprecated")

## TODO: Rename context
## TODO: Add more tests

test_that("deprecated expected", {
  expect_that(mesh(), gives_warning("rangl"))
  expect_that(tri_mesh(), throws_error("defunct"))
  })

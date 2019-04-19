
context("basic")
library(anglr)
library(silicate)

a <- simpleworld[9, ]
library("spbabel")
data("holey")
test_that("holey polygons and non-holy polygons", {
  expect_that(b <- TRI(a), is_a("TRI"))
    sph <- sp(holey)
    sph$meta$proj <- "+proj=laea +ellps=WGS84"
    expect_that(  tm <- TRI(sph), is_a("TRI"))
  expect_true(!is.na(silicate::PATH(a)$meta$proj))
  expect_true(!is.na(b$meta$proj))
  expect_silent(  plot(b))
    expect_silent(globe(b))

})

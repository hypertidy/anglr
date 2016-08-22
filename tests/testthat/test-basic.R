library(testthat)
context("basic")

library(rworldxtra)
 data(countriesHigh)

 a <- subset(countriesHigh, SOVEREIGNT == "Australia")

 


library("spbabel")
data("holey")
test_that("holey polygons and non-holy polygons", {
  expect_silent({
    b <- tri_mesh(a)
  })
  expect_silent({
    sph <- sp(holey)
    tm <- tri_mesh(sph)
  })
  
})


test_that("simple plot", {
  expect_silent({
    b <- tri_mesh(a)
    plot(b)
    
  })
})

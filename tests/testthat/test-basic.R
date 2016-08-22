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

library(maptools)
data(wrld_simpl)
test_that("simple plot", {
  expect_silent({
    b <- tri_mesh(a)
    plot(b)
    
  })
  expect_silent({
    b <- tri_mesh(a)
    globe(b)
    
      rgl::rgl.clear()
  })
 expect_silent({
   for (i in sample(seq(nrow(wrld_simpl)), 10)) {
     globe(tri_mesh(wrld_simpl[i, ]))
   }
 })
})

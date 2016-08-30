library(testthat)
context("basic")
library(rangl)

library(rworldxtra)
 data(countriesHigh)

 a <- subset(countriesHigh, SOVEREIGNT == "Australia")

 


library("spbabel")
data("holey")
test_that("holey polygons and non-holy polygons", {
  expect_that(b <- mesh(a), is_a("trimesh"))
    sph <- sp(holey)
    sph$meta$proj <- "+proj=laea +ellps=wGS84"
    
    expect_that(  tm <- mesh(sph), is_a("trimesh"))
  
  
})

library(maptools)
data(wrld_simpl)
test_that("simple plot", {
 b <- mesh(a)
    plot(b)
    
    b <- mesh(a)
    globe(b)
    
    ## check that we work without a DataFrame
    b <- mesh(geometry(a))
      rgl::rgl.clear()
   for (i in sample(seq(nrow(wrld_simpl)), 10)) {
     globe(mesh(wrld_simpl[i, ]))
     rgl::rgl.clear()

 }
})

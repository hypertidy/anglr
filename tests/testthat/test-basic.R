library(testthat)
context("basic")
library(rangl)

library(rworldxtra)
 data(countriesHigh)

 a <- subset(countriesHigh, SOVEREIGNT == "Australia")

 


library("spbabel")
data("holey")
test_that("holey polygons and non-holy polygons", {
  expect_that(b <- tri_mesh(a), is_a("trimesh"))
    sph <- sp(holey)
    sph$meta$proj <- "+proj=laea +ellps=wGS84"
    
    expect_that(  tm <- tri_mesh(sph), is_a("trimesh"))
  
  
})

library(maptools)
data(wrld_simpl)
test_that("simple plot", {
 b <- tri_mesh(a)
    plot(b)
    
    b <- tri_mesh(a)
    globe(b)
    
    ## check that we work without a DataFrame
    b <- tri_mesh(geometry(a))
      rgl::rgl.clear()
   for (i in sample(seq(nrow(wrld_simpl)), 10)) {
     globe(tri_mesh(wrld_simpl[i, ]), halo = as.logical(i %% 2))
     rgl::rgl.clear()

 }
})

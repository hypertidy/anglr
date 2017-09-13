library(testthat)
context("basic")
library(anglr)

library(rworldxtra)
 data(countriesHigh)

 a <- subset(countriesHigh, SOVEREIGNT == "Australia")

 
library("spbabel")
data("holey")
test_that("holey polygons and non-holy polygons", {
  expect_that(b <- anglr(a), is_a("trimesh"))
    sph <- sp(holey)
    sph$meta$proj <- "+proj=laea +ellps=wGS84"
    
    expect_that(  tm <- anglr(sph), is_a("trimesh"))
  
  
})

library(maptools)
data(wrld_simpl)
test_that("simple plot", {
 b <- anglr(a)
    plot(b)
    
    b <- anglr(a)
    globe(b)
    
    ## check that we work without a DataFrame
    b <- anglr(geometry(a))
      rgl::rgl.clear()
   for (i in sample(seq(nrow(wrld_simpl)), 10)) {
     globe(anglr(wrld_simpl[i, ]))
     rgl::rgl.clear()

 }
})

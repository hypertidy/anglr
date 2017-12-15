library(testthat)
context("plot")

## TODO: Rename context
## TODO: Add more tests

library(spbabel)
sph <-sp(holey)
spl <- as(sph, "SpatialLinesDataFrame")
spp <- as(spl, "SpatialMultiPointsDataFrame")
spz <- anglr(spp)
spz$v$z_ <- 10
sp0 <- as(spl, "SpatialPointsDataFrame")
splz <- anglr(spl)
splz$v$z_ <- rnorm(nrow(splz$v))

 test_that("plot works", {
    expect_that(names(plot(anglr(spl))), equals(c("v", "it")))
   expect_that(plot(anglr(sph)), is_a("mesh3d"))
   #expect_that(plot(anglr(sp0)), throws_error("you don't really need this function"))
   expect_that(plot(splz), is_a("list"))
   expect_that(plot(anglr(spp)), is_a("list"))
   expect_that(plot(spz), is_a("list"))
 })
 

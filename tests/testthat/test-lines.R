context("lines")

## TODO: Rename context
## TODO: Add more tests

library(spbabel)
sph <- sp(holey)
spl <- as(sph, "SpatialLinesDataFrame")
 test_that("lines works", {
   expect_that(SC(spl), is_a("SC"))
   expect_that(SC(sp::geometry(spl)), is_a("SC"))
   
})



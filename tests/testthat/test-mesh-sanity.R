context("mesh-sanity")

library(raster)
library(sf)

v <- raster(diag(3))
p <- st_as_sf(rasterToPolygons(v, dissolve = TRUE))
tp <- st_cast(sfdct::ct_triangulate(p), warn = FALSE)
nverts <- 16

test_that("vertex de-duplication is sane", {
  expect_equal(sc_coord(p) %>% distinct() %>% nrow(), 
               nverts)
  expect_equal(sc_coord(tp) %>% distinct() %>% nrow(), 
               nverts)
  expect_equal(anglr(p)$v  %>% nrow(), 
               nverts)
  expect_equal(anglr(tp)$v  %>% nrow(), 
               nverts)
  
})

ntriangles <- nrow(gibble::gibble(tp))
test_that("triangle set is equivalent", {
  expect_equal(ntriangles, 18L)
  
  ## triangulating p here and below fails because of https://github.com/hypertidy/anglr/issues/54
  ## but it works for tp because those triangles already exist and the mesh comes out the same
  anglr(p)$t %>% nrow() %>%  expect_equal(ntriangles)
  anglr(tp)$t %>% nrow() %>%  expect_equal(ntriangles)

  ## we expect 18 because although the (constant) z value requires distinct features
  ## the number of triangles is the same, as one feature is in the gaps of the other
  anglr(p, z = "layer")$t %>% nrow() %>%  expect_equal(ntriangles)
  anglr(tp, z = "layer")$t %>% nrow() %>%  expect_equal(ntriangles)
  
})

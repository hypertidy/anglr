context("rgl")

## TODO: Rename context
## TODO: Add more tests

library(rgl)
o <- oh3d()
tt <- tetrahedron3d()
p1 <- cbind(x = c(0, 0, 0.75, 1,   0.5, 0.8, 0.69, 0), 
            y = c(0, 1, 1,    0.8, 0.7, 0.6, 0,    0))
p <- extrude3d(p1)
test_that("rgl objects can be ingested", {
  expect_that(anglr(tt), is_a("trimesh"))
  expect_that(anglr(p), gives_warning("quad"))
  expect_that(anglr(o), throws_error("is not TRUE"))
  
})


test_that("converting RTriangle works", {
  set.seed(101)
  pts <- matrix(runif(50L), ncol = 2L)
  seg <- path2seg(chull(pts))
  seg <- rbind(seg, c(seg[length(seg)], seg[1L]))
  p <- RTriangle::pslg(pts, S = seg)
  tr <- RTriangle::triangulate(p)

  mesh <- as.mesh3d(tr)
  expect_s3_class(mesh, "mesh3d")
  expect_equal(dim(mesh$it), c(3L, 41L))
})

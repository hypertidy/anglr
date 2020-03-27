context("as.mesh3d")


test_that("as.mesh3d is working", {
  ## first object has a hole in it
  mintri <- DEL(silicate::minimal_mesh)
  mintri0 <- silicate::TRI0(silicate::minimal_mesh)
  fill <- as.mesh3d(mintri, keep_all = TRUE)
  hole <- as.mesh3d(mintri, keep_all = FALSE)
  tri0 <- as.mesh3d(mintri0)
  expect_equal(dimfill <- dim(fill$it), c(3L, 20))
  expect_equal(dimhole <- dim(hole$it), c(3L, 14))
  expect_names(tri0, c("vb", "it", "material", "normals",
                       "texcoords", "meshColor"))
                                                                      xpect_equal(dim(tri0$vb), c(4L, 14L))
  expect_equal(dim(tri0$vb), c(3L, 14L))

  tri_waters <- DEL(silicate::inlandwaters[c(3, 4, 6), ])
  expect_equal(dim(tri_waters$triangle),
               c(41835L, 5L))
  tri_mesh <- as.mesh3d(tri_waters)
  expect_equal(c(dim(tri_mesh$vb), dim(tri_mesh$it)),
               c(4L, 23608L, 3L, 41835L))

  tri_waters$vertex <-   tri_waters$vertex %>%
    dplyr::mutate(x_ = scales::rescale(x_, c(0, ncol(volcano))),
                  y_ = scales::rescale(y_, c(0, nrow(volcano))))

  r <- palr::image_raster(volcano)
  tri_mesh <- as.mesh3d(tri_waters, image_texture = r)
  ## todo test file path, clean up file, png sanity, range of texcoords, color of material (not black) etc
  plot(tri_mesh)
})


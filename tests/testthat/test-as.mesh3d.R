context("as.mesh3d")
library(sp)  ## avoid messages later
library(raster) ## avoid messages later

test_that("as.mesh3d on triangles is working", {
  ## first object has a hole in it
  mintri <- DEL(silicate::minimal_mesh)
  mintri0 <- silicate::TRI0(silicate::minimal_mesh)
  fill <- as.mesh3d(mintri, keep_all = TRUE)
  hole <- as.mesh3d(mintri, keep_all = FALSE)
  tri0 <- as.mesh3d(mintri0)
  expect_equal(dimfill <- dim(fill$it), c(3L, 14))
  expect_equal(dimhole <- dim(hole$it), c(3L, 14))
  expect_named(tri0, c("vb", "it", "material", "normals",
                       "texcoords", "meshColor"))

  expect_equal(dim(tri0$vb), c(4L, 14L))

  tri_waters <- DEL(silicate::inlandwaters[c(3, 4, 6), ])
  expect_equal(dim(tri_waters$triangle),
               c(41835L, 5L))
  tri_mesh <- as.mesh3d(tri_waters)
  expect_equal(c(dim(tri_mesh$vb), dim(tri_mesh$it)),
               c(4L, 23608L, 3L, 25811L))

  tri_waters$vertex <-   tri_waters$vertex %>%
    dplyr::mutate(x_ = scales::rescale(x_, c(0, ncol(volcano))),
                  y_ = scales::rescale(y_, c(0, nrow(volcano))))

  r <- palr::image_raster(volcano)
  tri_mesh <- as.mesh3d(tri_waters, image_texture = r)
  ## todo test file path, clean up file, png sanity, range of texcoords, color of material (not black) etc
  mesh_plot(tri_mesh)
  plot3d(tri_mesh)

  ##  a warning this is not how to do it
  expect_warning(as.mesh3d(mintri0, material = list(color = "red")))

  expect_silent(as.mesh3d(mintri0, color = "red"))

  expect_warning(xx <- as.mesh3d(mintri0, image_texture = r,
                           texcoords = matrix(runif(prod(dim(tri0$it)), 4))))

  expect_warning(xx1 <- as.mesh3d(mintri0, image_texture = r,
                                 texcoords = matrix(runif(prod(dim(tri0$it)), 4))))

  expect_warning(xx2 <- as.mesh3d(mintri0, image_texture = r, texture = tempfile(),
                                 texcoords = matrix(runif(prod(dim(tri0$it)), 4))),
                 "good PNG filename")

  ## cleanup
  file.remove(xx1$material$texture)
  file.remove(xx2$material$texture)
  file.remove(tri_mesh$material$texture)

  expect_equal(as.mesh3d(mintri0, smooth = TRUE)$normals,
               matrix(c(0, 0, 1, 1), nrow  = 4L, ncol = 14L))

  ## copy down z
  expect_silent(dem <- as.mesh3d(mintri0, raster::raster(volcano)))
  expect_true(max(dem$vb, na.rm = TRUE) > 160)  ## 161.2 on 2020-03-27


  ## dunno what they are ../
  expect_warning(dem <- as.mesh3d(silicate::TRI0(cad_tas), z = gebco))
})


test_that("as.mesh3d on quads is working", {
  qd <- as.mesh3d(volcano)
  expect_named(qd[1:2], c("vb", "ib"))
  expect_named(as.mesh3d(volcano, triangles = TRUE)[1:2], c("vb", "it"))
  m <- matrix(c(2, 4, 5, 1,  3, 10, 9, 8, 7, 6, 12, 11), 3)
  expect_true(is.null(as.mesh3d(m, smooth  = FALSE)$normals))
  expect_true(all(as.mesh3d(m, smooth  = TRUE)$normals < 1.1))

  rr <- raster::raster(scales::rescale(volcano, 0, 255))
  expect_silent(as.mesh3d(rr))
  expect_message(as.mesh3d(rr, image_texture = raster::brick(rr, rr, rr)),
                "writing texture image to")
library(raster)
  sc <- silicate::TRI0(as(extent(rr), "SpatialPolygons"))
  expect_message(as.mesh3d(sc, z = rr * 2, image_texture = raster::brick(rr, rr, rr)),
                 "writing texture image to")

  expect_message(as.mesh3d(sc, z = rr, image_texture = raster::brick(rr, rr, rr)))

  expect_silent(xxx <- as.mesh3d(sc, z = c(20, 22)))
  expect_equal(xxx$vb[3, ], c(20, 22, 20, 22))
    })




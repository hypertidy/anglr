library(silicate)

path <- PATH(minimal_mesh)
test_that("copy down works", {
  expect_message({

    tri <- silicate::TRI(cad_tas)
    x <- copy_down(tri, gebco)
    expect_true(!anyNA(sc_vertex(x)))

    x <- copy_down(tri, 10)
    expect_equal(x$vertex$z_, rep(10, nrow(sc_vertex(x))))
    x <- copy_down(tri, 10:20)
    expect_equal(sort(unique(x$vertex$z_)), 10:20)

    x2 <- copy_down(tri, "PID")
    expect_equal(range(match(x2$vertex$z_, tri$object$PID)), c(1, 246))

    tri$object$value <- 2.5
    x4 <- copy_down(tri, "value")
    expect_true(all(x4$vertex$z_ == 2.5))


    rz <- raster::raster(volcano)
    copy_down(path, rz)

    expect_equal(copy_down(path, "a")$vertex$z_,
                 c(rep(1, 12), rep(2, 4)))

  })

  expect_silent(copy_down(SC(path), rz))
  expect_silent(copy_down(SC0(path), rz))

  expect_silent(copy_down(TRI(path), rz))
  expect_silent(copy_down(TRI0(path), rz))


  expect_silent(copy_down(SC(path), "a"))
  expect_silent(copy_down(SC0(path), "a"))

  expect_silent(copy_down(TRI(path), "a"))
  expect_silent(copy_down(TRI0(path), "a"))


})

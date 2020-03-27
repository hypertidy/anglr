library(silicate)
test_that("copy down works", {
  expect_message({

    tri <- silicate::TRI(cad_tas)
    x <- copy_down(tri, quadmesh::etopo)
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

    path <- PATH(minimal_mesh)
    copy_down(path, raster::raster(volcano))

    expect_equal(copy_down(path, "a")$vertex$z_,
                 c(rep(1, 12), rep(2, 4)))

  })

})

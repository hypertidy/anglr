inlandwaters <- silicate::inlandwaters[1:2, ]

test_that("auto_3d does something", {
  expect_message({
    plot3d(QUAD(volcano))
    auto_3d()
}, "applying")

  expect_silent({
    plot3d(QUAD(volcano))
    auto_3d(verbose = FALSE)
  })

  expect_message({
      plot3d(copy_down(SC(inlandwaters),
                   quadmesh::etopo))
  auto_3d()
}, "applying")

    plot3d(copy_down(SC(inlandwaters),
                     quadmesh::etopo))
    auto_3d(verbose = FALSE)


})

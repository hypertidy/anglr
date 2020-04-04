context("test-sanity")

test_that("sanity prevails 1", {
  expect_silent(DEL(minimal_mesh))
  expect_silent(TRI(minimal_mesh))
  expect_silent(DEL(minimal_mesh))
  expect_silent(TRI(minimal_mesh))
})
test_that("sanity prevails 2", {
  expect_silent(plot(DEL(minimal_mesh)))
  expect_silent(plot(TRI(minimal_mesh)))
  expect_silent(plot(DEL(minimal_mesh)))
  expect_silent(plot(TRI(minimal_mesh)))

})
test_that("sanity prevails 3", {
  expect_silent(plot3d(DEL(minimal_mesh)))
  expect_silent(plot3d(TRI(minimal_mesh)))
  expect_silent(plot3d(DEL(minimal_mesh)))
  expect_silent(plot3d(TRI(minimal_mesh)))
})
test_that("sanity prevails 4", {
  z <- copy_down(DEL(minimal_mesh), gebco)
  expect_equivalent(z$vertex$z_, c(-4890, -4916.05, -4828.88, -4795.832, -4846.382, -4813.1744,
                                   -4841.022, -4877.1168, -4849.578, -4846.94, -4862.78, -4875.4552,
                                   -4772.29496, -4758.55736))
  expect_message(bb <- copy_down(SC(cont_tas), gebco), "transforming")
  expect_true(all(bb$vertex$z_ > 150 & bb$vertex$z_ < 200))
  expect_silent(copy_down(DEL(minimal_mesh), gebco))
  expect_silent(copy_down(TRI(minimal_mesh), gebco))
  expect_silent(copy_down(DEL(minimal_mesh), "a"))
  expect_silent(copy_down(TRI(minimal_mesh), "a"))
})
test_that("sanity prevails 5", {
  expect_silent(plot(copy_down(DEL(minimal_mesh), gebco)))
  expect_silent(plot(copy_down(TRI(minimal_mesh), gebco)))
  expect_silent(plot(copy_down(DEL(minimal_mesh), "a")))
  expect_silent(plot(copy_down(TRI(minimal_mesh), "a")))
})
test_that("sanity prevails 6", {
  expect_silent(plot3d(copy_down(DEL(minimal_mesh), gebco)))
  expect_silent(plot3d(copy_down(TRI(minimal_mesh), gebco)))
  expect_silent(plot3d(copy_down(DEL(minimal_mesh), "a")))
  expect_silent(plot3d(copy_down(TRI(minimal_mesh), "a")))

})

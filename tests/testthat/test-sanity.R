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
  expect_equivalent(z$vertex$z_, c(-4928.25, -4889.75, -4814.5, -4782.75, -4845.25, -4822.09,
                                   -4857.72, -5029.45, -4937.55, -4915.75, -4900.55, -4992.54, -4778.644,
                                   -4787.672))
  expect_message(bb <- copy_down(SC(cont_tas), gebco), "transforming")
  expect_true(all(bb$vertex$z_ > 11 & bb$vertex$z_ < 20))
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

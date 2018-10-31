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
  expect_silent(copy_down(DEL(minimal_mesh), gebco1))
  expect_silent(copy_down(TRI(minimal_mesh), gebco1))
  expect_silent(copy_down(DEL(minimal_mesh), "a"))
  expect_silent(copy_down(TRI(minimal_mesh), "a"))
})
test_that("sanity prevails 5", {
  expect_silent(plot(copy_down(DEL(minimal_mesh), gebco1)))
  expect_silent(plot(copy_down(TRI(minimal_mesh), gebco1)))
  expect_silent(plot(copy_down(DEL(minimal_mesh), "a")))
  expect_silent(plot(copy_down(TRI(minimal_mesh), "a")))
})
test_that("sanity prevails 6", {
  expect_silent(plot3d(copy_down(DEL(minimal_mesh), gebco1)))
  expect_silent(plot3d(copy_down(TRI(minimal_mesh), gebco1)))
  expect_silent(plot3d(copy_down(DEL(minimal_mesh), "a")))
  expect_silent(plot3d(copy_down(TRI(minimal_mesh), "a")))

})

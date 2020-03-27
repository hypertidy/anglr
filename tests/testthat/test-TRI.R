test_that("TRI works", {
  ## not doing anything useful atm
  ## can this go into silicate?
  expect_silent(triq <- TRI.QUAD(QUAD(volcano)))
  plot3d(QUAD(volcano))
  mesh_plot(QUAD(volcano))
})

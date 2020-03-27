
library(silicate)
mm <- silicate::minimal_mesh
test_that("DEL0 works", {
  expect_silent({

    DEL0(PATH0(silicate::minimal_mesh))
DEL0(silicate::PATH0(silicate::minimal_mesh))

plot3d(SC(mm))
plot3d(mm, add = TRUE)
plot3d(SC0(mm))
plot3d(TRI(mm))
plot3d(TRI0(mm))
plot3d(ARC(mm))
plot3d(DEL(mm))
plot3d(DEL0(mm))
plot3d(PATH(mm))
plot3d(silicate::PATH0(mm))

  })
})

context("Icanhavez")


library(spbabel)
hol <- silicate::TRI(sp(holey))
hol <- copy_down(hol, hol$object$rownumber_)
hol$meta$proj[1] <- "+proj=laea +a=80"

test_that("3d plot is cool", {
  expect_silent({
rglobj <- plot3d(hol)
globeobj <- plot3d(globe(hol))
})
})


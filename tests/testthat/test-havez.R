context("Icanhavez")


library(spbabel)
hol <- silicate::TRI(sp(holey))
hol <- copy_down(hol, hol$object$rownumber_)
hol$meta$proj[1] <- "+proj=laea +a=80"

rglobj <- plot3d(hol)
globeobj <- plot3d(globe(hol))
test_that("Z is handled properly", {
  expect_equal(rglobj$vb[3, ], hol$v$z_)
  expect_true(sum(globeobj$v[3,] - hol$v$z_) > 1e6)
})

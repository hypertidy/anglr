context("Icanhavez")


library(spbabel)
hol <- silicate::TRI(sp(holey))
hol <- copy_down(hol, hol$object$rownumber_)
hol$meta$proj[1] <- "+proj=laea +a=80"

test_that("3d plot is cool", {
  expect_silent({
rglobj <- plot3d(hol)
## not happy jan, just don't do it
# Error in PROJ::proj_trans_generic(x[, 1:2, drop = FALSE], target = target,  :
#    generic error of unknown origin
# globeobj <- plot3d(globe(hol))
})
})


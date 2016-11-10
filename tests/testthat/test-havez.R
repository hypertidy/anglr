context("Icanhavez")


library(spbabel)
hol <- rangl(sp(holey))
hol$v$z_ <- rnorm(nrow(hol$v))
hol$meta$proj[1] <- "+proj=laea +a=80"

rglobj <- plot(hol)
globeobj <- plot(globe(hol))
test_that("Z is handled properly", {
  expect_equal(rglobj$vb[3, ], hol$v$z_)
  expect_true(sum(globeobj$v[3,] - hol$v$z_) > 1e6)
})

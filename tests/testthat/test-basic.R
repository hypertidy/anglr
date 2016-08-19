library(testthat)
context("basic")

library(rworldxtra)
 data(countriesHigh)

test_that("simple plot", {
  expect_silent({
    a <- subset(countriesHigh, SOVEREIGNT == "Australia")
    b <- tri_mesh(a)
    plot(b)

  })
})

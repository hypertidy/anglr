context("trip")

## TODO: Rename context
## TODO: Add more tests

library(trip)
example(trip)

test_that("trip works", {
  expect_that(rangl(tr), is_a("linemesh"))
})

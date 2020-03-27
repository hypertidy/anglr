# test_that("internal make_local works", {
#   expect_true(grepl("\\+proj=laea \\+lon_0=136",
#                     make_local(SC(silicate::inlandwaters)
#                )))
#
#   expect_true(grepl("\\+proj=laea",
#                     make_local(QUAD(volcano))
#                     ))
#
# })

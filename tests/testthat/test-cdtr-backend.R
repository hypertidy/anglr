context("cdtr backend")
skip_if_not_installed("cdtr")
library(silicate)

tri_area <- function(x) {
  v <- as.matrix(x$vertex[c("x_", "y_")])
  tt <- do.call(rbind, lapply(x$object$topology_, function(t) as.matrix(t[c(".vx0", ".vx1", ".vx2")])))
  a <- v[tt[, 1], , drop = FALSE]; b <- v[tt[, 2], , drop = FALSE]; c <- v[tt[, 3], , drop = FALSE]
  0.5 * abs((b[, 1] - a[, 1]) * (c[, 2] - a[, 2]) - (c[, 1] - a[, 1]) * (b[, 2] - a[, 2]))
}
with_backend <- function(b, expr) { old <- options(anglr.triangulator = b); on.exit(options(old)); force(expr) }

test_that("DEL0 agrees across backends without refinement", {
  for (x in list(minimal_mesh, inlandwaters)) {
    rt <- with_backend("RTriangle", DEL0(x))
    cd <- with_backend("cdtr", DEL0(x))
    expect_equal(nrow(cd$vertex), nrow(rt$vertex))
    expect_equal(nrow(cd$object), nrow(rt$object))
    expect_equal(sum(tri_area(cd)), sum(tri_area(rt)))
    ## per-object area is the thing the path classification must get right
    expect_equal(vapply(cd$object$topology_, nrow, 1L) > 0, vapply(rt$object$topology_, nrow, 1L) > 0)
  }
})

test_that("DEL0 with max_area honours the bound on both backends", {
  rt <- with_backend("RTriangle", DEL0(minimal_mesh, max_area = 0.005))
  cd <- with_backend("cdtr", DEL0(minimal_mesh, max_area = 0.005))
  expect_lte(max(tri_area(cd)), 0.005 + 1e-12)
  expect_lte(max(tri_area(rt)), 0.005 + 1e-12)
  expect_equal(sum(tri_area(cd)), sum(tri_area(rt)))
  expect_gt(nrow(cd$vertex), nrow(DEL0(minimal_mesh)$vertex))
})

test_that("DEL (per-object) and DEL0 on sf/polygons run under cdtr", {
  with_backend("cdtr", {
    expect_s3_class(DEL(minimal_mesh), "DEL")
    expect_s3_class(DEL(SC(minimal_mesh), max_area = 0.01), "DEL")
    expect_s3_class(DEL0(SC0(minimal_mesh)), "DEL0")
    expect_s3_class(DEL0(TRI(minimal_mesh)), "DEL0")
  })
})

test_that("z_ is carried onto Steiner vertices under cdtr", {
  m <- minimal_mesh
  v <- silicate::sc_vertex(PATH0(m)); v$z_ <- 2 * v$x_ + v$y_
  p0 <- PATH0(m); p0$vertex <- v
  cd <- with_backend("cdtr", DEL0(p0, max_area = 0.005))
  expect_true("z_" %in% names(cd$vertex))
  expect_equal(cd$vertex$z_, 2 * cd$vertex$x_ + cd$vertex$y_)
  rt <- with_backend("RTriangle", DEL0(p0, max_area = 0.005))
  expect_equal(rt$vertex$z_, 2 * rt$vertex$x_ + rt$vertex$y_)
})

test_that("unsupported Triangle flags are ignored with a message", {
  expect_message(with_backend("cdtr", DEL0(minimal_mesh, Y = TRUE)), "ignored")
})

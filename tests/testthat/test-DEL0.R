
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


area_sc.sf <- function(x) {
  area_sc.sfc(x[[attr(x, "sf_column")]])
}
area_sc.sfc <- function(x) {
  ring_area <- rapply(x,
                      lda)
  g <- gibble::gibble(x)
  ## if object changes, it's not a hole
  ## if subobject changes, it's not a hole so
  ## parallel max on diff to flip the sign
  sgn <- c(-1, 1)[(pmax(c(1, diff(g$object)), c(1, diff(g$subobject))) == 1) + 1]
  sum(ring_area * sgn)
}

## area, get every ring but then negate sign according to position in polygon > 1
ld <- function(x) x[2:(length(x) + 1)]
lead_area_abs <- function(x, y) {
  abs(sum(ld(x) * y - x * ld(y), na.rm = TRUE)/2)
}

## function for rapply
lda <-
  function(x) {
    lead_area_abs(x[,1], x[,2])
  }


test_that("DEL0 fix works", {
  ## We Suggest gibble, but it's present because silicate imports it
  if (!requireNamespace("gibble")) {
    skip()
  }
  #sum(sf::st_area(sf::st_set_crs(sf::st_as_sfc(silicate::minimal_mesh$geom), sf::st_crs(NULL))))
  ##minimal_mesh [1] 0.86895
  ##
  ## inlandwaters [1] 1.923706e+12
  xx <- silicate::minimal_mesh
  (area_ <- area_sc.sf(xx))
  tri <- DEL0(xx)
  idx <- t(as.matrix(do.call(rbind, tri$object$topology_)[c(".vx0", ".vx1", ".vx2")]))
  area_triangles <- silicate::tri_area(as.matrix(silicate::sc_vertex(xx)[as.vector(idx), c("x_", "y_")]))
  expect_equivalent(area_, sum(area_triangles))

#
#   xx <- silicate::inlandwaters
#   (area_ <- area_sc.sf(xx))
#   tri <- DEL0(xx)
#   idx <- t(as.matrix(do.call(rbind, tri$object$topology_)[c(".vx0", ".vx1", ".vx2")]))
#   area_triangles <- silicate::tri_area(as.matrix(silicate::sc_vertex(xx)[as.vector(idx), c("x_", "y_")]))
#   expect_equivalent(area_, sum(area_triangles))

})

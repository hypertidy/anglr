#' persp3d
#'
#' Plot surface
#'
#' @name persp3d
#' @param x sp, sf, raster, trip, or any other model understood by anglr/silicate
#' @param ... pass [material3d properties][rgl::rgl.material] to rgl
#' @param add add to existing plot or start a new one (the default)
#' @seealso [plot3d] [as.mesh3d] [wire3d] [dot3d] [shade3d]
#' @export
persp3d.TRI <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name persp3d
#' @export
persp3d.TRI0 <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name persp3d
#' @export
persp3d.BasicRaster <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name persp3d
#' @export
persp3d.DEL <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name persp3d
#' @export
persp3d.DEL0 <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name persp3d
#' @export
persp3d.QUAD <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}

#' @name persp3d
#' @export
persp3d.matrix <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}

#' @name persp3d
#' @export
persp3d.sf <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name persp3d
#' @export
persp3d.sfc <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name persp3d
#' @export
persp3d.Spatial <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name persp3d
#' @export
persp3d.triangulation <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}

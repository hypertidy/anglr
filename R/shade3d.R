#' shade3d
#'
#' Plot surface
#'
#' @name shade3d
#' @param x sp, sf, raster, trip, or any other model understood by anglr/silicate
#' @param ... pass [material3d properties][rgl::rgl.material] to rgl
#' @param add add to existing plot or start a new one (the default)
#' @seealso [plot3d] [as.mesh3d] [persp3d] [dot3d] [wire3d]
#' @importFrom rgl shade3d
#' @export shade3d
#' @export
shade3d.TRI <- function(x, ..., add = FALSE) {
  shade3d(as.mesh3d(x, ...), add = add)
}
#' @name shade3d
#' @export
shade3d.TRI0 <- function(x, ..., add = FALSE) {
  shade3d(as.mesh3d(x, ...), add = add)
}

#' @name shade3d
#' @export
shade3d.DEL <- function(x, ..., add = FALSE) {
  shade3d(as.mesh3d(x, ...), add = add)
}
#' @name shade3d
#' @export
shade3d.DEL0 <- function(x, ..., add = FALSE) {
  shade3d(as.mesh3d(x, ...), add = add)
}
#' @name shade3d
#' @export
shade3d.QUAD <- function(x, ..., add = FALSE) {
  shade3d(as.mesh3d(x, ...), add = add)
}

#' @name shade3d
#' @export
shade3d.matrix <- function(x, ..., add = FALSE) {
  shade3d(as.mesh3d(x, ...), add = add)
}

#' @name shade3d
#' @export
shade3d.sf <- function(x, ..., add = FALSE) {
  shade3d(as.mesh3d(x, ...), add = add)
}
#' @name shade3d
#' @export
shade3d.Spatial <- function(x, ..., add = FALSE) {
  shade3d(as.mesh3d(x, ...), add = add)
}

#' Draw a mesh as surfaces in 3D
#'
#' Draw surfaces with rgl from any [shape3d][rgl::tmesh3d] classed object. Produces
#' a 3D surface plot from a mesh-alike object.
#'
#' Objects that are not explicitly surfaces will be triangulated in order to
#' produce the mesh. Whether this is a good idea or not is an open question, and
#' some conversions will fail due to "extra" attributes like z or time stored on
#' vertices. Polygons are only implicit surfaces but these are usually
#' unproblematic to triangulate so this is done.
#' @name shade3d
#' @param x sp, sf, raster, or any other surface model understood by anglr/silicate
#' @param ... pass [material3d properties][rgl::material3d] to rgl
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

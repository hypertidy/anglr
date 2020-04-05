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
#' @seealso [plot3d] [as.mesh3d] [persp3d] [dot3d] [wire3d] [mesh_plot]
#' @importFrom rgl shade3d
#' @export shade3d
#' @export
#' @examples
#' rgl::open3d()
#' shade3d(volcano)
#'
#' \donttest{
#' ## create a globe plot of land areas with elevation
#' rgl::open3d()
#' world <- copy_down(DEL(simpleworld, max_area = 0.5), gebco * 50)
#' shade3d(globe(world), specular = "black", color = "white")
#' rgl::spheres3d(0, 0, 0, radius = 6378000, col = "dodgerblue", alpha = 0.75)
#' rgl::bg3d("black")
#' }
shade3d.TRI <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.TRI0 <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.PATH <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.PATH0 <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.DEL <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.DEL0 <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.QUAD <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.BasicRaster <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.matrix <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.sfc <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.sf <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.SC <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.SC0 <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.Spatial <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}
#' @name shade3d
#' @export
shade3d.triangulation <- function(x, ...) {
  shade3d(as.mesh3d(x), ...)
}

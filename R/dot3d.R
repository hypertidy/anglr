#' Draw a mesh as points in 3D
#'
#' Draw points with rgl from any mesh-alike or [shape3d][rgl::tmesh3d] classed
#' object. Produces a 3D scatterplot like that of [rgl::points3d()], but
#' from a mesh-alike object.
#'
#' The class [mesh3d][rgl::dot3d] extends 'shape3d' and allows methods to plot
#' non-surface properties. Note that `dot3d()` will always add to an existing
#' scene.
#'
#' It is not currently *technically defined or clear* how colour properties are
#' mapped to dots by default ... there is a problem of what property to use from
#' features that share the same vertex, and we have put that aside and erred on
#' the side of inaccuracy in favour of getting a pretty plot (hopefully).
#' (Properties that come later - lower rows - win, I think.
#'
#' (For some reason `size` is not vectorized like `col` is, but this is not
#' explored in detail from an anglr view).
#'
#' @name dot3d
#' @param x sc, sp, sf, raster, trip, or any other model understood by anglr/silicate
#' @param ... pass [material3d properties][rgl::rgl.material] to rgl
#' @importFrom rgl dot3d
#' @seealso [as.mesh3d] [persp3d] [wire3d] [plot3d] [shade3d]
#' @export dot3d
#' @export
#' @examples
#' dot3d(cad_tas)
#' dot3d(volcano, size = 10)
#' auto_3d()
#'
#'
#' rgl::open3d()
#' ## from ?persp
#' y <- x <- seq(-10, 10, length= 80)
#' z <- outer(x, y,
#'      function(x, y) {
#'         r <- sqrt(x^2+y^2); 10 * sin(r)/r
#'         })
#' dot3d(z)
#' dot3d(raster::raster(volcano), size = 10)
#' auto_3d()
#' \donttest{
#' dot3d(silicate::SC(cad_tas))
#'
#'
#' rgl::open3d()
#' dot3d(as.mesh3d(copy_down(DEL(cad_tas, max_area = 1e3), "CID")))
#' auto_3d()
#' }
dot3d.sf <- function(x, ...) {
  dot3d(as.mesh3d(silicate::TRI0(x)), ...)
}
#' @name dot3d
#' @export
dot3d.sfc <- function(x, ...) {
  dot3d(as.mesh3d(silicate::TRI0(x)), ...)
}
#' @name dot3d
#' @export
dot3d.Spatial <- function(x, ...) {
  dot3d(as.mesh3d(silicate::TRI0(x)), ...)
}
#' @name dot3d
#' @export
dot3d.matrix <- function(x, ...) {
  dot3d(as.mesh3d(x), ...)
}

#' @name dot3d
#' @export
dot3d.BasicRaster <- function(x, ...) {
  dot3d(as.mesh3d(x), ...)
}

#' @name dot3d
#' @export
dot3d.sc <- function(x, ...) {
  dot3d(as.mesh3d(x), ...)
}

#' @name dot3d
#' @export
dot3d.SC <- function(x, ...) {
  dot3d(as.mesh3d(x), ...)
}

#' @name dot3d
#' @export
dot3d.SC0 <- function(x, ...) {
  dot3d(as.mesh3d(x), ...)
}

#' @name dot3d
#' @export
dot3d.triangulation <- function(x, ...) {
  dot3d(as.mesh3d(x), ...)
}

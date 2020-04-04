#' Draw a mesh as line segments in 3D
#'
#' Draw line segments with rgl from any [shape3d][rgl::tmesh3d] classed object. Produces
#' a 3D scatterplot like that produced by [rgl::plot3d()], but from a mesh-alike
#' object.
#'
#' Objects that are not explicitly surfaces will be triangulated in order to produce the
#' mesh. Whether this is a good idea or not is an open question.
#'
#' It is not currently *technically defined or clear* how colour properties are
#' mapped to line segments by default ... there is a problem of what property to
#' use from features that share the same vertex or edge, and we have put that
#' aside and erred on the side of inaccuracy in favour of getting a pretty plot
#' (hopefully). (Properties that come later - lower rows - win, I think.
#'
#' @name wire3d
#' @param x sp, sf, raster, trip, or any other model understood by anglr/silicate
#' @param ... pass [material3d properties][rgl::rgl.material] to rgl
#' @importFrom rgl wire3d
#' @export wire3d
#' @seealso [plot3d] [as.mesh3d] [persp3d] [dot3d] [shade3d]
#' @export
wire3d.sc <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}
#' @name wire3d
#' @export
wire3d.TRI <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}
#' @name wire3d
#' @export
wire3d.TRI0 <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}

#' @name wire3d
#' @export
wire3d.DEL <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}
#' @name wire3d
#' @export
wire3d.DEL0 <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}

#' @name wire3d
#' @export
wire3d.QUAD <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}
#' @name wire3d
#' @export
wire3d.matrix <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}

#' @name wire3d
#' @export
wire3d.sf <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}
#' @name wire3d
#' @export
wire3d.Spatial <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}
#' @name wire3d
#' @export
wire3d.triangulation <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}

#' @name wire3d
#' @export
wire3d.trip <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}
#' @name wire3d
#' @export
wire3d.BasicRaster <- function(x, ...) {
  wire3d(as.mesh3d(x), ...)
}



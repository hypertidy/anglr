#' wire3
#'
#' plot lines from mesh
#'
#' @name wire3d
#' @param x sp, sf, raster, trip, or any other model understood by anglr/silicate
#' @param ... pass [material3d properties][rgl::rgl.material] to rgl
#' @importFrom rgl wire3d
#' @export wire3d
#' @seealso plot3d as.mesh3d persp3d dot3d
#' @export
wire3d.sc <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}
#' @name wire3d
#' @export
wire3d.TRI <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}
#' @name wire3d
#' @export
wire3d.TRI0 <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}

#' @name wire3d
#' @export
wire3d.DEL <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}
#' @name wire3d
#' @export
wire3d.DEL0 <- function(x, ...) {
  wire3d(as.mesh3d(x, ...), ...)
}

#' @name wire3d
#' @export
wire3d.QUAD <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}
#' @name wire3d
#' @export
wire3d.matrix <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}

#' @name wire3d
#' @export
wire3d.sf <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}
#' @name wire3d
#' @export
wire3d.Spatial <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}
#' @name wire3d
#' @export
wire3d.triangulation <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}

#' @name wire3d
#' @export
wire3d.trip <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}
#' @name wire3d
#' @export
wire3d.BasicRaster <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}



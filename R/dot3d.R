#' Draw points in 3d
#'
#' Draw points with rgl from any mesh-alike object.
#' @name dot3d
#' @param x sc, sp, sf, raster, trip, or any other model understood by anglr/silicate
#' @inheritDotParams rgl::dot3d
#' @importFrom rgl dot3d
#' @export dot3d
#' @export
#' @examples
#' dot3d(cad_tas)
dot3d.sf <- function(x, ...) {
  dot3d(as.mesh3d(silicate::TRI0(x)), ...)
}

#' @name dot3d
#' @export
#' @examples
#' dot3d(volcano, size = 10)
#' auto_3d(z = 14)
#'
#'
#' rgl::clear3d()
#' ## from ?persp
#' y <- x <- seq(-10, 10, length= 80)
#' z <- outer(x, y,
#'      function(x, y) {
#'         r <- sqrt(x^2+y^2); 10 * sin(r)/r
#'         })
#' dot3d(z)
#' auto_3d()
dot3d.matrix <- function(x, ...) {
  dot3d(as.mesh3d(x), ...)
}

#' @name dot3d
#' @export
#' @examples
#' dot3d(raster::raster(volcano), size = 10)
#' auto_3d(z = 14)
dot3d.BasicRaster <- function(x, ...) {
  dot3d(as.mesh3d(x), ...)
}

#' @name dot3d
#' @export
#' @examples
#' dot3d(silicate::SC(cad_tas))
#' auto_3d(z = 14)
dot3d.sc <- function(x, ...) {
  dot3d(as.mesh3d(x), ...)
}



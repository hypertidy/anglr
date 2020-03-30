#' @name plot3d
#' @aliases wire3d
#' @importFrom rgl wire3d
#' @export wire3d
#' @export
wire3d.sc <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}
#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.TRI <- function(x, ...) {
  wire3d(as.mesh3d(x, ...))
}
#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.TRI0 <- function(x, ..., add = FALSE) {
  wire3d(as.mesh3d(x, ...), add = add)
}

#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.DEL <- function(x, ..., add = FALSE) {
  wire3d(as.mesh3d(x, ...), add = add)
}
#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.DEL0 <- function(x, ...) {
  wire3d(as.mesh3d(x, ...), ...)
}

#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.QUAD <- function(x, ..., add = FALSE) {
  wire3d(as.mesh3d(x, ...), add = add)
}
#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.matrix <- function(x, ..., add = FALSE) {
  wire3d(as.mesh3d(x, ...), add = add)
}

#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.sf <- function(x, ..., add = FALSE) {
  wire3d(as.mesh3d(x, ...), add = add)
}
#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.Spatial <- function(x, ..., add = FALSE) {
  wire3d(as.mesh3d(x, ...), add = add)
}
#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.triangulation <- function(x, ..., add = FALSE) {
  wire3d(as.mesh3d(x, ...), add = add)
}

#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.trip <- function(x, ..., add = FALSE) {
  wire3d(as.mesh3d(x, ...), add = add)
}
#' @name plot3d
#' @aliases wire3d
#' @export
wire3d.BasicRaster <- function(x, ..., add = FALSE) {
  wire3d(as.mesh3d(x, ...), add = add)
}



#' @name plot3d
#' @aliases persp3d
#' @export
persp3d.TRI <- function(x, ...) {
  plot3d(as.mesh3d(x, ...))
}
#' @name plot3d
#' @aliases persp3d
#' @export
persp3d.TRI0 <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}

#' @name plot3d
#' @aliases persp3d
#' @export
persp3d.DEL <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name plot3d
#' @aliases persp3d
#' @export
persp3d.DEL0 <- function(x, ...) {
  plot3d(as.mesh3d(x, ...), ...)
}

#' @name plot3d
#' @aliases persp3d
#' @export
persp3d.QUAD <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name plot3d
#' @aliases persp3d
#' @export
persp3d.QUAD <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name plot3d
#' @aliases persp3d
#' @export
persp3d.matrix <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}

#' @name plot3d
#' @aliases persp3d
#' @export
persp3d.sf <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}
#' @name plot3d
#' @aliases persp3d
#' @export
persp3d.Spatial <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add)
}

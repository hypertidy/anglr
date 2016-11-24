#' Deprecated from rangl
#' @rdname rangl-defunct
#' @name rangl-defunct
#' @param x nothing
#' @param ... ignored
#' @export
tri_mesh <- function(x, ...) {
  .Defunct("rangl", package= "rangl")
}

#' @rdname rangl-defunct
#' @name rangl-defunct
#' @export
mesh <- function(x, ...) {
  .Deprecated("rangl", package= "rangl", old = "mesh")
}
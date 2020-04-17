#' Auto aspect ratio
#'
#' Automatically modify the aspect ratio of a scene to
#' rescale drastically different data ranges into a cube.
#'
#' This is a simple alias to [rgl::aspect3d(1)][rgl::aspect3d].
#'
#' This is typically used to rescale data in different units, for example
#' longitude and latitude in degrees and elevation in metres.
#'
#' Note that running `rgl::aspect3d("iso")` which show the realistic ratio of
#' the plot axes, ignoring units. Running this function is equivalent to
#' `rgl::aspectd(1)` (or `rgl::aspect(x = 1, y = 1, z = 1)`) which sets the
#' _apparent_ ratios of the current bounding box.
#'
#' @return the original value of [rgl::par3d()] before update
#' @export
#' @param ... unused, check for input of arguments which are ignored with a message
#' @examples
#' topo <- copy_down(silicate::SC(simpleworld), gebco)
#' plot3d(topo)
#' ## update aspect ratio to be an apparent cube, not a needle
#' auto_3d()
auto_3d <- function(...) {
  p3d <- rgl::par3d("scale")
  rgl::aspect3d(1L)
  if (length(list(...)) > 0) .Deprecated("auto_3d", new = "auto_3d", msg = "use only 'auto_3d()', arguments to 'auto_3d()' are no longer accepted")
  invisible(p3d)
}

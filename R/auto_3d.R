#' Auto aspect ratio
#'
#' Automatically modify the aspect ratio of a scene to
#' rescale drastically different data ranges into something more
#' pleasing.
#'
#' This is typically used to rescale data in different units, for example longitude and
#' latitude in degrees and elevation in metres.
#'
#' @param x exaggeration for x
#' @param y exaggeration for y
#' @param z exaggeration for z
#' @param keep_xy should xy be forced to maintain their current aspect ratio
#' @param exag should the x, y, z factors be applied, set to `FALSE` to ignore
#' @param verbose keep shush
#'
#' @return the output of `rgl::par3d` invisibly
#' @export
#'
#' @examples
#' topo <- copy_down(silicate::SC(simpleworld), gebco1)
#' plot3d(topo)
#'
#' auto_3d(z = 4)
auto_3d <- function(x = 1, y = 1, z = 1, keep_xy = TRUE, exag = TRUE, verbose = TRUE) {
  thr <- apply(matrix(rgl::par3d()$bbox, 2), 2, function(a) diff(a))
  if (any(zz)) thr[zz] <- mean(thr[!zz])
  dxy <- thr[2]/thr[1]
  asp <- 1/(thr/min(thr))
  if (keep_xy) asp[1:2] <- 1
  if (exag) asp <- asp * c(x, y, z)
  #if (!is.null(getOption("rgl.useNULL")) && interactive() && runif(1, 0, 1) > 0.96) {
  #  message("rgl NULL device in use, do you need to run rgl::rglwidget()?")
  #}
  if (verbose) {
    psp <- format(asp, digits = 3)
    message(sprintf("original axis lengths x,y,z: %s", paste(format(thr), collapse = ",", sep = "")))
    message(sprintf("applying 'aspect3d(%s, %s, %s)'", psp[1], psp[2], psp[3]))
  }
  rgl::aspect3d(asp[1], asp[2], asp[3])
}

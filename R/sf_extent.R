#' Extent of simple features
#'
#' Get Extent of sf objects
#'
#' This function exists to avoid raster needing the sf package. We don't need
#' GDAL, GEOS, and all the other fun stuff to find four numbers that describe
#' coordinate range, so this is a workaround.
#' @param x sf object
#'
#' @return a raster Extent
#' @export
#'
#' @examples
#' sf_extent(silicate::inlandwaters)
sf_extent <- function(x) {
  cds <- silicate::sc_coord(x)
  raster::extent(range(cds$x_, na.rm = TRUE), range(cds$y_, na.rm = TRUE))
}

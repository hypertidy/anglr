#' Generate primitive-based spatial structures
#'
#' Create primitive-based "mesh" structures from various inputs.
#'
#' #' Methods exist for SpatialPolygons, SpatialLines, rgl mesh3d(triangle) ...
#' @param x input data
#' @param ... arguments passed to methods
#' @param max_area maximum area in coordinate system of x, passed to \code{\link[RTriangle]{triangulate}} 'a' argument
#' @return a list of tibble data frames, using the gris-map_table model
#' @export
#' @section Warning:
#' anglr only checks for presence of triangle centres within
#' known holes, so this doesn't pick up examples of overlapping areas e.g. 
#' https://github.com/hypertidy/anglr/issues/39
#' 
#' @examples
#' ## -----------------------------------------------
#' ## POLYGONS
#' library(maptools)
#' data(wrld_simpl)
#' b <- anglr(wrld_simpl)
#' plot(b)
#' #if (require(rworldxtra)) {
#'
#' #data(countriesHigh)
#' #sv <- c("New Zealand", "Antarctica", "Papua New Guinea",
#' #  "Indonesia", "Malaysia", "Fiji", "Australia")
#' #a <- subset(countriesHigh, SOVEREIGNT %in% sv)
#' #b7 <- anglr(a, max_area = 0.5)
#' #plot(globe(b7))
#' #}
#' ## -----------------------------------------------
#' ## LINES
#' #l1 <- anglr(as(a, "SpatialLinesDataFrame") )
#' #plot(l1)
#' #plot(globe(l1))
#' 
#' #data("flight_tracks", package = "silicate")
#' #r <- anglr(flight_tracks)
#' #plot(r)
#' #rgl::aspect3d(1, 1, 0.001)
#' #rgl::rglwidget()
#' 
#' ## copy feature attributes onto vertices
#' #library(sf)
#' #example(st_read)
#' #library(rgl)
#' #rgl.clear(); plot(anglr(st_cast(nc, "MULTILINESTRING"), z = "BIR74"));
#' 
#' #x <- nc
#' ## copy raster attributes onto vertices (must be same projection for now)
#' #topo <- raster(system.file("extdata", "gebco1.tif", package = "ang
#' #rgl.clear(); plot(anglr(x, z = topo/10));
#' #data("wrld_simpl", package= "maptools")
#' # rgl.clear(); plot(globe(anglr(st_as_sf(wrld_simpl[c(9, 160), ]), z = topo*1e4, max_area = .1)));
anglr <- function(x, ...) {
  UseMethod("anglr")
}
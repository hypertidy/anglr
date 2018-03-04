#' Generate primitive-based spatial structures
#'
#' Create primitive-based "mesh" structures from various inputs.
#'
#' #' Methods exist for SpatialPolygons, SpatialLines, rgl mesh3d(triangle) ...
#' @param x input data
#' @param type type of topology to create (LINE or POLYGON)
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
#' data(simpleworld)
#' b <- anglr(simpleworld)
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
#' 
#' ## we can nominate the output type so we aren't bound to what's come in
#' #rgl.clear(); plot(anglr(st_cast(x, "MULTILINESTRING"), z = topo/1000, type = "POLYGON"));
#' #rgl.clear(); plot(anglr(x, z = topo/1000, type = "LINE"));
anglr <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
  UseMethod("anglr")
}


## anglr generic will have a "type" - surface or segment
## use this to specify or override the inferred type, and that calls anglr_lines or anglr_polys

## generic will
## get_proj




#' @rdname anglr
#' @importFrom dplyr %>%  arrange distinct mutate
#' @importFrom sp geometry  over SpatialPoints proj4string CRS SpatialPolygonsDataFrame
#' @importFrom dplyr inner_join
#' @importFrom RTriangle pslg triangulate
#' @importFrom spbabel map_table
#' @importFrom tibble tibble
#' @importFrom methods slotNames
#' @export

#' @name anglr
#' @export
anglr.default <- function(x, z = NULL, ..., type = NULL, max_area = NULL) {
  ## we don't need methods for sf, Spatial etc because PATH covers those
  anglr(PATH(x), z = z, ..., type = type, max_area = max_area)
}


anglr_lines <- function(tabs,   ...) {
  .Deprecated(new = "silicate::SC or silicate::TRI, and plot3d.SC", 
              package = "anglr", 
              old = "anglr(<line-topology>)")
  tabs$vertex$countingIndex <- seq_len(nrow(tabs$vertex))
  nonuq <- dplyr::inner_join(tabs$path_link_vertex, tabs$vertex, "vertex_")
  
  tabs$path$object_id <- as.integer(factor(tabs$path$object_))
  nonuq <- dplyr::inner_join(nonuq, tabs$path, "path_")
  
  S = do.call(rbind, lapply(split(nonuq, nonuq$path_),
                            function(x) cbind(path2seg(x$countingIndex), x$object_id[1])))
  tabs$vertex$countingIndex <- NULL
  tabs$vertex$vertex_ <- silicate::sc_uid(nrow(tabs$vertex))
  tabs$l <- tibble::tibble(segment_ = silicate::sc_uid(nrow(S)), object_ = tabs$o$object_[S[,3]])
  
  tabs$lXv <- tibble::tibble(segment_ = rep(tabs$l$segment_, each = 2), 
                             vertex_ = tabs$v$vertex_[as.vector(t(S[,1:2]))])
  
  ## renormalize the vertices
  allverts <- dplyr::inner_join(tabs$lXv, tabs$vertex, "vertex_")
  #browser()
  allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, sep = "_")))
  allverts$vertex_ <- silicate::sc_uid(length(unique(allverts$uvert)))[allverts$uvert]
  tabs$lXv <- allverts[, c("segment_", "vertex_")]
  tabs$v <- dplyr::distinct(allverts, .data$uvert, .keep_all = TRUE)
  tabs$o <- tabs$object
  tabs$object <- tabs$path <- tabs$vertex <- tabs$path_link_vertex <- NULL
  
  tabs <- tabs[c("o", "l", "lXv", "v", "meta")]
  attr(tabs, "join_ramp") <- c("o", "l", "lXv", "v")
  class(tabs) <- "linemesh"
  tabs
}




#' @rdname anglr
#' @export
anglr.SpatialMultiPoints <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
  pr4 <- proj4string(x)
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialMultiPointsDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  tabs$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", ctime = format(Sys.time(), tz = "UTC"))
  
  class(tabs) <- "pointmesh"
  tabs
}


#' @rdname anglr
#' @export
anglr.SpatialPoints <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
  pr4 <- proj4string(x)
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialPointsDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  tabs$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", ctime = format(Sys.time(), tz = "UTC"))
  
  class(tabs) <- "pointmesh"
  tabs
}



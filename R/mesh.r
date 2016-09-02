#' Generate primitives mesh
#'
#' Create primitives mesh structures from various inputs.
#'
#' #' Methods exist for SpatialPolygons, SpatialLines, rgl mesh3d(triangle) ...
#' @param x input data
#' @param ... arguments passed to methods
#' @param max_area maximum area in coordinate system of x, passed to \code{\link[RTriangle]{triangulate}} 'a' argument
#' @return a list of tibble data frames, using the gris-map_table model
#' @export
#' @examples
#' ## -----------------------------------------------
#' ## POLYGONS
#' library(maptools)
#' data(wrld_simpl)
#' b <- mesh(wrld_simpl)
#' plot(b)
#' if (require(rworldxtra)) {
#'
#' data(countriesHigh)
#' sv <- c("New Zealand", "Antarctica", "Papua New Guinea",
#'  "Indonesia", "Malaysia", "Fiji", "Australia")
#' a <- subset(countriesHigh, SOVEREIGNT %in% sv)
#' b7 <- mesh(a, max_area = 0.5)
#' plot(globe(b7))
#' }
#' ## -----------------------------------------------
#' ## LINES
#' l1 <- mesh(as(a, "SpatialLinesDataFrame") )
#' plot(l1)
#' plot(globe(l1))
mesh <- function(x, ...) {
  UseMethod("mesh")
}

line_mesh_map_table1 <- function(tabs) {
  tabs$v$countingIndex <- seq(nrow(tabs$v))
  nonuq <- dplyr::inner_join(tabs$bXv, tabs$v, "vertex_")
  
  ps <- RTriangle::pslg(P = as.matrix(tabs$v[, c("x_", "y_")]),
                        S = do.call(rbind, lapply(split(nonuq, nonuq$branch_),
                                                  function(x) path2seg(x$countingIndex))))
  
  tabs$v <- tibble::tibble(x_ = ps$P[,1], y_ = ps$P[,2], vertex_ = spbabel:::id_n(nrow(ps$P)))
  tabs$b <- tabs$bXv <- NULL
  tabs$l <- tibble::tibble(segment_ = spbabel:::id_n(nrow(ps$S)), object_ = tabs$o$object_[1])
  tabs$lXv <- tibble::tibble(segment_ = rep(tabs$l$segment_, each = 2), 
                             vertex_ = tabs$v$vertex_[as.vector(t(ps$S))])
  
  tabs
}
#' @rdname mesh
#' @export
mesh.SpatialLines <- function(x, ...) {
  pr4 <- proj4string(x)
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialLinesDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  ll <- vector("list", nrow(tabs$o))
  for (i_obj in seq(nrow(tabs$o))) {
    tabs_i <- tabs; tabs_i$o <- tabs_i$o[i_obj, ]
    tabs_i <- spbabel:::semi_cascade(tabs_i)
    tt_i <- line_mesh_map_table1(tabs_i)
    # plot.trimesh(tt_i)
    # scan("", 1L)
    # rgl::rgl.clear()
    ll[[i_obj]] <- tt_i
  }
  
  outlist <- vector("list", length(ll[[1]]))
  nms <- names(ll[[1]])
  names(outlist) <- nms
  for (i in seq_along(outlist)) {
    outlist[[i]] <- dplyr::bind_rows(lapply(ll, "[[", nms[i]))
  }
  
  ## renormalize the vertices
  allverts <- dplyr::inner_join(outlist$lXv, outlist$v, "vertex_")
  allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, sep = "_")))
  allverts$vertex_ <- spbabel:::id_n(length(unique(allverts$uvert)))[allverts$uvert]
  outlist$lXv <- allverts[, c("segment_", "vertex_")]
  outlist$v <- dplyr::distinct_(allverts, "x_", "y_", "vertex_")
  ## finally add longitude and latitude
  outlist$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_")
  class(outlist) <- "linemesh"
  outlist
}

#' @export
plot.linemesh <- function(x,  ...) {
  if (!"color_" %in% names(x$o)) {
    x$o$color_ <- trimesh_cols(nrow(x$o))
  }
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl required")
  haveZ <- "z_" %in% names(x$v)
  #tt <- th3d()
  
  if (haveZ) {
    vb <- t(cbind(x$v$x_, x$v$y_, x$v$z_))
  } else {
    
    vb <- t(cbind(x$v$x_, x$v$y_, 0))
  }
  vv <- x$v[, "vertex_"]; vv$row_n <- seq(nrow(vv))
  pindex <- dplyr::inner_join(dplyr::inner_join(x$o[, c("object_", "color_")], x$l), 
                              x$lXv)
  
  vindex <- dplyr::inner_join(x$lXv, vv, "vertex_")
  itex <- t(matrix(vindex$row_n, ncol = 2, byrow = TRUE))
  rgl::segments3d(t(vb)[itex,], col = pindex$color_, ...)
  
  
  invisible(list(v = vb, it = itex))
}


#' Convert map coordinates to Geocentric (XYZ) coordinates. 
#'
#' 
#' @param x list of tibbles, in \code{\link{mesh}} form
#' @param gproj Geocentric PROJ.4 string, defaults to WGS84
#' @param ... arguments to methods (none used)
#'
#' @return mesh object with vertices table modified
#' @export
#'
#' @examples
#' library(maptools)
#' data(wrld_simpl)
#' g <- globe(mesh(as(wrld_simpl, "SpatialLinesDataFrame")))
#' plot(g, lwd = 3)
globe <- function(x, ...) {
  UseMethod("globe")
}

#' @export
#' @rdname globe
globe.default <- function(x, gproj = "+proj=geocent +ellps=WGS84", ...) {
  p4 <- x$meta$proj[1]
  haveZ <- "z_" %in% names(x$v)
  
  ## need to handle if we already have a "z_"
  if (haveZ) {
    ll <- as.matrix(x$v[, c("x_", "y_", "z_")])
    
  } else { 
    ll <- cbind(as.matrix(x$v[, c("x_", "y_")]), 0)
  }
  
  if (grepl("longlat", p4)) ll <- ll * pi / 180
  xyz <- proj4::ptransform(ll, src.proj = p4, dst.proj = gproj)
  x$v$x_ <- xyz[,1]
  x$v$y_ <- xyz[,2]
  x$v$z_ <- xyz[,3]
  x
}

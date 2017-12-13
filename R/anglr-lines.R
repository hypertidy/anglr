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
anglr <- function(x, z = NULL, ...) {
  UseMethod("anglr")
}

line_mesh_map_table1 <- function(tabs) {
  tabs$v$countingIndex <- seq_len(nrow(tabs$v))
  nonuq <- dplyr::inner_join(tabs$bXv, tabs$v, "vertex_")
  
  tabs$b$object_id <- as.integer(factor(tabs$b$object_))
  nonuq <- dplyr::inner_join(nonuq, tabs$b, "branch_")
  
  #pl <- list(P = as.matrix(tabs$v[, c("x_", "y_")]),
             S = do.call(rbind, lapply(split(nonuq, nonuq$branch_),
                                       function(x) cbind(path2seg(x$countingIndex), x$object_id[1])))
  tabs$v$countingIndex <- NULL
  tabs$v$vertex_ <- silicate::sc_uid(nrow(tabs$v))
  #tabs$v <- tibble::tibble(x_ = pl$P[,1], y_ = pl$P[,2], vertex_ = spbabel:::id_n(nrow(pl$P)))
  
  tabs$b <- tabs$bXv <- NULL
  #tabs$l <- tibble::tibble(segment_ = spbabel:::id_n(nrow(pl$S)), object_ = tabs$o$object_[1])
  tabs$l <- tibble::tibble(segment_ = silicate::sc_uid(nrow(S)), object_ = tabs$o$object_[S[,3]])
  
  tabs$lXv <- tibble::tibble(segment_ = rep(tabs$l$segment_, each = 2), 
                             vertex_ = tabs$v$vertex_[as.vector(t(S[,1:2]))])
  
  tabs
}

anglr_lines <- function(tabs, ...) {
  ll <- vector("list", nrow(tabs$o))
 # for (i_obj in seq(nrow(tabs$o))) {
  # tabs_i <- tabs; tabs_i$o <- tabs_i$o[i_obj, ] 
#  tabs_i <- semi_cascade(tabs_i)
#  tt_i <- line_mesh_map_table1(tabs_i)
 #   ll[[i_obj]] <- tt_i
#  }
#  outlist <- vector("list", length(ll[[1]]))
#  nms <- names(ll[[1]])
#  names(outlist) <- nms
#  for (i in seq_along(outlist)) {
#    outlist[[i]] <- dplyr::bind_rows(lapply(ll, "[[", nms[i]))
#  }
 
  outlist <- line_mesh_map_table1(tabs) 
  ## renormalize the vertices
  allverts <- dplyr::inner_join(outlist$lXv, outlist$v, "vertex_")
  #browser()
  allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, sep = "_")))
  allverts$vertex_ <- spbabel:::id_n(length(unique(allverts$uvert)))[allverts$uvert]
  outlist$lXv <- allverts[, c("segment_", "vertex_")]
  outlist$v <- dplyr::distinct(allverts, uvert, .keep_all = TRUE)
#  allverts$uvert <- NULL
 # outlist$v <- allverts 
  # qs <- rlang::quos(...)
  # if (length(qs) < 1L) {
  #   geometry_dimension_label <- rev(class(x[[attr(x, "sf_column")]][[1]]))[3L]
  #   qs <- rlang::syms(switch(geometry_dimension_label, 
  #                            XY = c("x_", "y_", "vertex_"), 
  #                            XYZ = c("x_", "y_", "z_", "vertex_"), 
  #                            XYM = c("x_", "y_", "m_", "vertex_"), 
  #                            XYZM = c("x_", "y_", "z_", "m_", "vertex_"))    )
  # }
  # print(names(allverts))
  # print(qs)
  ## finally add longitude and latitude
  #outlist$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", ctime = format(Sys.time(), tz = "UTC"))
  class(outlist) <- "linemesh"
  outlist
}



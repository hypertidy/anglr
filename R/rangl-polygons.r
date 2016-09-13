#' @importFrom utils head
path2seg <- function(x) {
  head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)
}

#' Deprecated from rangl
#' @rdname rangl-deprecated
#' @param x nothing
#' @param ... ignored
#' @export
tri_mesh <- function(x, ...) {
  .Deprecated("mesh", package= "rangl", old = "tri_mesh")
}

#' @rdname rangl-deprecated
#' @export
mesh <- function(x, ...) {
  .Deprecated("rangl", package= "rangl", old = "mesh")
}

tri_mesh_map_table1 <- function(tabs, max_area = NULL) {
  tabs$v$countingIndex <- seq(nrow(tabs$v))
  nonuq <- dplyr::inner_join(tabs$bXv, tabs$v, "vertex_")
  
  ps <- RTriangle::pslg(P = as.matrix(tabs$v[, c("x_", "y_")]),
                        S = do.call(rbind, lapply(split(nonuq, nonuq$branch_),
                                                  function(x) path2seg(x$countingIndex))))
  
  ## FIXME: need to pick sensible behaviour for a
  tr <- RTriangle::triangulate(ps, a = max_area)
  
  ## process the holes if needed
  ## may be quicker than testing entire object
  if (any(!tabs$b$island_)) {
    holes <- spbabel::sp(dplyr::inner_join(dplyr::inner_join(dplyr::filter_(tabs$b, quote(!island_)), tabs$bXv, "branch_"), 
                                           tabs$v, "vertex_"))
    centroids <- matrix(unlist(lapply(split(tr$P[t(tr$T), ], rep(seq(nrow(tr$T)), each = 3)), .colMeans, 3, 2)), 
                        ncol = 2, byrow = TRUE)
    
    badtris <- !is.na(over(SpatialPoints(centroids), sp::geometry(holes)))
    if (any(badtris)) tr$T <- tr$T[!badtris, ]
  }
  
  ## trace and remove any unused triangles
  
  tabs$v <- tibble::tibble(x_ = tr$P[,1], y_ = tr$P[,2], vertex_ = spbabel:::id_n(nrow(tr$P)))
  tabs$b <- tabs$bXv <- NULL
  #tabs$o <- tabs$o[1,]  ## FIX ME
  tabs$t <- tibble::tibble(triangle_ = spbabel:::id_n(nrow(tr$T)), object_ = tabs$o$object_[1])
  tabs$tXv <- tibble::tibble(triangle_ = rep(tabs$t$triangle_, each = 3), 
                             vertex_ = tabs$v$vertex_[as.vector(t(tr$T))])
  
  tabs
}

#' @rdname rangl
#' @export
#' @importFrom sp geometry  over SpatialPoints proj4string CRS SpatialPolygonsDataFrame
#' @importFrom dplyr inner_join
#' @importFrom RTriangle pslg triangulate
#' @importFrom spbabel map_table
#' @importFrom tibble tibble
#' @importFrom methods slotNames
rangl.SpatialPolygons <- function(x, max_area = NULL, ...) {
  pr4 <- proj4string(x)
  x0 <- x
  ## kludge for non DataFrames
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialPolygonsDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  
  ll <- vector("list", nrow(tabs$o))
  for (i_obj in seq(nrow(tabs$o))) {
    tabs_i <- tabs; tabs_i$o <- tabs_i$o[i_obj, ]
    tabs_i <- semi_cascade(tabs_i)
    tt_i <- tri_mesh_map_table1(tabs_i, max_area = max_area)
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
  allverts <- dplyr::inner_join(outlist$tXv, outlist$v, "vertex_")
  allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, sep = "_")))
  allverts$vertex_ <- spbabel:::id_n(length(unique(allverts$uvert)))[allverts$uvert]
  outlist$tXv <- allverts[, c("triangle_", "vertex_")]
  outlist$v <- dplyr::distinct_(allverts,  "vertex_", .keep_all = TRUE)[, c("x_", "y_", "vertex_")]
  ## finally add longitude and latitude
  outlist$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", ctime = format(Sys.time(), tz = "UTC"))
  class(outlist) <- "trimesh"
  outlist
}

th3d <- function() {
  structure(list(vb = NULL, it = NULL, primitivetype = "triangle",
                 material = list(), normals = NULL, texcoords = NULL), .Names = c("vb",
                                                                                  "it", "primitivetype", "material", "normals", "texcoords"), class = c("mesh3d",
                                                                                                                                                        "shape3d"))
}

trimesh_cols <- function(n) {
  viridis::viridis(n)
}

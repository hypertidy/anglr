#' @importFrom utils head
path2seg <- function(x) {
  head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)
}


#' Generate triangle mesh
#'
#' Create triangle mesh structures from various inputs.
#'
#' Methods exist for SpatialPolygons, ...
#' @param x input data
#' @param max_area maximum area in coordinate system of x, passed to \code{\link[RTriangle]{triangulate}} 'a' argument
#' @param ... arguments passed to methods
#' 
#' @return a list of tibble data frames, using the gris-map_table model
#' @export
tri_mesh <- function(x, ...) {
  UseMethod("tri_mesh")
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
#' @rdname tri_mesh
#' @export
#' @importFrom sp geometry  over SpatialPoints proj4string CRS SpatialPolygonsDataFrame
#' @importFrom dplyr inner_join
#' @importFrom RTriangle pslg triangulate
#' @importFrom spbabel map_table
#' @importFrom tibble tibble
#' @importFrom methods slotNames
#' @examples
#' if (require(rworldxtra)) {
#'
#' data(countriesHigh)
#' sv <- c("New Zealand", "Antarctica", "Papua New Guinea",
#'  "Indonesia", "Malaysia", "Fiji", "Australia")
#' a <- subset(countriesHigh, SOVEREIGNT %in% sv)
#' b <- tri_mesh(a)
#' #b <- tri_mesh(a, max_area = 0.1)
#' }
#' 
#' library(maptools)
#' data(wrld_simpl)
#' b <- tri_mesh(wrld_simpl)
tri_mesh.SpatialPolygons <- function(x, max_area = NULL, ...) {
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
    tabs_i <- spbabel:::semi_cascade(tabs_i)
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
  outlist$v <- dplyr::distinct_(allverts, "x_", "y_", "vertex_")
  ## finally add longitude and latitude
  outlist$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_")
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
#' plot the triangles in the tables
#'
#' plot
#' @param x object from tri_mesh
#' @param ... args for underlying plotting
#'
#' @return the rgl mesh3d object, invisibly
#' @export
#' @importFrom rgl shade3d
#' @examples
#' example(tri_mesh)
#' if(exists("b")) { 
#'  plot(b)
#'  }
plot.trimesh <- function(x, ...) {
  cols <- trimesh_cols(nrow(x$o))
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl required")
  haveZ <- "z_" %in% names(x)
  for (i_obj in seq(nrow(x$o))) {
    xx <- x; xx$o <- xx$o[i_obj, ]
    xx <- spbabel:::semi_cascade(xx, tables = c("o", "t", "tXv", "v"))
    tt <- th3d()
    tt$vb <- t(cbind(xx$v$x_, xx$v$y_, 0, 1))
    vv <- xx$v[, "vertex_"]; vv$row_n <- seq(nrow(vv))
    index <- dplyr::inner_join(xx$tXv, vv, "vertex_")
    tt$it <- t(matrix(index$row_n, ncol = 3, byrow = TRUE))
    rgl::shade3d(tt, col = cols[i_obj], ...)
    
  }
  invisible(tt)
}

#' Title
#'
#' @param x object from tri_mesh
#' @param halo show the radius
#' @param ... passed to plot
#' @param rad radius
#'
#' @return the mesh object, invisibly
#' @export
#' @examples
#' example(tri_mesh)
#' if(exists("b")) { 
#'  globe(b, halo = TRUE)
#'  rgl::rgl.clear()
#'  globe(b, halo = FALSE)
#'  
#'  }
#'   
globe <- function(x, ...) {
  UseMethod("globe")
}

#' @rdname globe
#' @importFrom rgl ellipse3d plot3d
#' @export
globe.trimesh <- function(x, halo = FALSE, ..., rad = 1) {
  cols <- trimesh_cols(nrow(x$o))
  
  gproj <- sprintf("+proj=geocent +a=%f +b=%f", rad, rad)
  p4 <- x$meta$proj[1]
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl required")
  haveZ <- "z_" %in% names(x$v)
  for (i_obj in seq(nrow(x$o))) {
    xx <- x; xx$o <- xx$o[i_obj, ]
    xx <- suppressMessages(spbabel:::semi_cascade(xx, tables = c("o", "t", "tXv", "v")))
    
    ## need to handle if we already have a "z_"
    if (haveZ) {
      ll <- as.matrix(xx$v[, c("x_", "y_", "z_")])
    
    } else { 
      ll <- cbind(as.matrix(xx$v[, c("x_", "y_")]), 0)
    }
    if (grepl("longlat", p4)) ll <- ll * pi / 180
    xyz <- proj4::ptransform(ll, src.proj = p4, dst.proj = gproj)
    tt <- th3d()
    tt$vb <- t(cbind(xyz, 1))
    vv <- xx$v[, "vertex_"]; vv$row_n <- seq(nrow(vv))
    index <- dplyr::inner_join(xx$tXv, vv, "vertex_")
    tt$it <- t(matrix(index$row_n, ncol = 3, byrow = TRUE))
    
    rgl::shade3d(tt, col = cols[i_obj], ...)
  }
  if (halo) {
    #rgl::spheres3d(0, 0, 0, radius = rad * 0.99, fog = FALSE, specular = "black", col = "dodgerblue", alpha = 0.4)
    rgl::shade3d(rgl::ellipse3d(diag(3) * rad, centre = c(0, 0, 0)), specular = "black", col = "dodgerblue", alpha = 0.4)
  }
  invisible(tt)
}
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
#' @param ... arguments passed to methods
#'
#' @return a list of tibble data frames, using the gris-map_table model
#' @export
#'
#' @examples
#' if (require(rworldxtra)) {
#'
#' data(countriesHigh)
#' a <- subset(countriesHigh, SOVEREIGNT == "Australia")
#' b <- tri_mesh(a)
#' }
tri_mesh <- function(x, ...) {
  UseMethod("tri_mesh")
}

#' @rdname tri_mesh
#' @export
#' @importFrom sp over SpatialPoints proj4string CRS
#' @importFrom dplyr inner_join
#' @importFrom RTriangle pslg triangulate
#' @importFrom spbabel map_table
#' @importFrom tibble tibble
tri_mesh.SpatialPolygons <- function(x, ...) {
  pr4 <- proj4string(x)
  x0 <- x
  tabs <- spbabel::map_table(x)

  ## FIXME: loop over SpatialPolygons
  #spbabel:::semi_cascade
  tabs$v$countingIndex <- seq(nrow(tabs$v))
  nonuq <- dplyr::inner_join(tabs$bXv, tabs$v, "vertex_")
  #> Joining, by = "vertex_"
  ps <- RTriangle::pslg(P = as.matrix(tabs$v[, c("x_", "y_")]),
             S = do.call(rbind, lapply(split(nonuq, nonuq$branch_),
                                       function(x) path2seg(x$countingIndex))))

  ## FIXME: need to pick sensible behaviour for a
  tr <- RTriangle::triangulate(ps)

  ## process the holes
  centroids <- t(apply(tr$T, 1, function(x) apply(tr$P[x, ], 2, mean)))
  badtris <- is.na(over(SpatialPoints(centroids, proj4string = CRS(pr4)),
                        x0)[[1]])
  ## trace and remove any unused triangles
  if (any(badtris)) tr$T <- tr$T[!badtris, ]

  tabs$v <- tibble::tibble(x_ = tr$P[,1], y_ = tr$P[,2], vertex_ = seq(nrow(tr$P)))
  tabs$b <- tabs$bXv <- NULL
  tabs$o <- tabs$o[1,]  ## FIX ME
  tabs$t <- tibble::tibble(triangle_ = seq(nrow(tr$T)), object_ = tabs$o$object_[1])
  tabs$tXv <- tibble::tibble(triangle_ = rep(tabs$t$triangle_, each = 3), vertex_ = as.vector(t(tr$T)))
  class(tabs) <- "trimesh"
  tabs
}

th3d <- function() {
  structure(list(vb = NULL, it = NULL, primitivetype = "triangle",
                 material = list(), normals = NULL, texcoords = NULL), .Names = c("vb",
                                                                                  "it", "primitivetype", "material", "normals", "texcoords"), class = c("mesh3d",
                                                                                                                                                        "shape3d"))
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
#' if(exists("b") & interactive()) { plot(b)}
plot.trimesh <- function(x, ...) {
  tt <- th3d()
  tt$vb <- t(cbind(x$v$x_, x$v$y_, 0, 1))
  tt$it <- t(matrix(x$tXv$vertex_, ncol = 3, byrow = TRUE))
  rgl::shade3d(tt, ...)
  invisible(tt)
}

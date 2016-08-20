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
tri_mesh <- function(x, ...) {
  UseMethod("tri_mesh")
}

#' @rdname tri_mesh
#' @export
#' @importFrom sp over SpatialPoints proj4string CRS
#' @importFrom dplyr inner_join
#' @importFrom RTriangle pslg triangulate
#' @importFrom sp geometry
#' @importFrom spbabel map_table
#' @importFrom tibble tibble
#' @examples
#' if (require(rworldxtra)) {
#'
#' data(countriesHigh)
#' sv <- "Australia"
#' a <- subset(countriesHigh, SOVEREIGNT == sv)
#' b <- tri_mesh(a)
#' }
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

  ## process the holes if needed
  ## may be quicker than testing entire object
  if (any(!tabs$b$island_)) {
    
#    holes <- spbabel::sp(tabs$b %>% 
 #                          dplyr::filter(!island_) %>% 
  #                         inner_join(tabs$bXv, "branch_") %>% 
   #                        inner_join(tabs$v, "vertex_"))
    holes <- spbabel::sp(dplyr::inner_join(dplyr::inner_join(dplyr::filter_(tabs$b, quote(!island_)), tabs$bXv, "branch_"), 
                               tabs$v, "vertex_"))
  ## no contest
   ## system.time({  centroids <- t(apply(tr$T, 1, function(x) apply(tr$P[x, ], 2, mean)))})
  #system.time({ 
    centroids <- matrix(unlist(lapply(split(tr$P[t(tr$T), ], rep(seq(nrow(tr$T)), each = 3)), .colMeans, 3, 2)), 
               ncol = 2, byrow = TRUE)
    ## marginally faster than matrix split
    #centroids <- setNames(as_tibble(tr$P[t(tr$T), ]), c("x", "y")) %>% 
    #  mutate(g = rep(seq(nrow(tr$T)), each = 3)) %>% 
    #  group_by(g) %>% summarize(x = mean(x), y = mean(y))
  #})
  
    badtris <- !is.na(over(SpatialPoints(centroids),
                    sp::geometry(holes)))
    if (any(badtris)) tr$T <- tr$T[!badtris, ]
    
  }
 ## trace and remove any unused triangles

  tabs$v <- tibble::tibble(x_ = tr$P[,1], y_ = tr$P[,2], vertex_ = seq(nrow(tr$P)))
  tabs$b <- tabs$bXv <- NULL
  tabs$o <- tabs$o[1,]  ## FIX ME
  tabs$t <- tibble::tibble(triangle_ = seq(nrow(tr$T)), object_ = tabs$o$object_[1])
  tabs$tXv <- tibble::tibble(triangle_ = rep(tabs$t$triangle_, each = 3), vertex_ = as.vector(t(tr$T)))
  
  ## finally add longitude and latitude
  tabs$meta <- tibble(proj = pr4, x = "x_", y = "y_")
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
#' if(exists("b")) { 
#'  ##plot(b)
#'  }
plot.trimesh <- function(x, ...) {
  tt <- th3d()
  tt$vb <- t(cbind(x$v$x_, x$v$y_, 0, 1))
  tt$it <- t(matrix(x$tXv$vertex_, ncol = 3, byrow = TRUE))
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl required")
  rgl::shade3d(tt, ...)
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
#'  ##globe(b, halo = TRUE)
#'  }
globe <- function(x, ...) {
  UseMethod("globe")
}

#' @rdname globe
#' @export
globe.trimesh <- function(x, halo = FALSE, ..., rad = 1) {
  gproj <- sprintf("+proj=geocent +a=%f +b=%f", rad, rad)
  p4 <- x$meta$proj[1]
  ll <- cbind(as.matrix(x$v[, c("x_", "y_")]), 0)
  if (grepl("longlat", p4)) ll <- ll * pi / 180
  xyz <- proj4::ptransform(ll, src.proj = p4, dst.proj = gproj)
  tt <- th3d()
  tt$vb <- t(cbind(xyz, 1))
  tt$it <- t(matrix(x$tXv$vertex_, ncol = 3, byrow = TRUE))
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl required")
  rgl::shade3d(tt, ...)
  if (halo) rgl::spheres3d(0, 0, 0, radius = rad * 0.99, fog = FALSE, specular = "black", col = "dodgerblue", alpha = 0.4)
  invisible(tt)
}
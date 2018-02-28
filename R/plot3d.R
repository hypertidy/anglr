#' 3D object plot
#'
#' @param x silicate model, SC, TRI, ARC, or PATH
#' @param ... 
#' @param add 
#' 
#' @return vertices and segment indices, invisibly
#' @export
#' @importFrom rgl plot3d
#' @examples
#' library(silicate)
#' x <- SC(sf::read_sf(system.file("shape", "nc.shp", package = "sf")) %>% 
#'    mutate(color_ = rainbow(100)))
#' plot3d(x); rglwidget()
plot3d.SC <- function(x, ..., add = FALSE) {
  if (!"color_" %in% names(x$object)) {
    x$object$color_ <- trimesh_cols(nrow(x$object))
  }
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl required")
  haveZ <- "z_" %in% names(x$vertex)
  if (haveZ) {
    vb <- cbind(x$vertex$x_, x$vertex$y_, x$vertex$z_)
  } else {
    vb <- cbind(x$vertex$x_, x$vertex$y_, 0)
  }
  pindex <- dplyr::inner_join(x$object_link_edge, x$object[, c("object_", "color_")], "object_")
  vindex <- rbind(match(x$edge$.vertex0, x$vertex$vertex_),
                  match(x$edge$.vertex1, x$vertex$vertex_))
  if (!add) {
    rgl::rgl.clear()
  }
  rgl::segments3d(vb[vindex,], 
      col = rep(pindex$color_[match(x$edge$edge_, pindex$edge_)], each = 2))
   if (getOption("rgl.useNULL") && interactive() && runif(1, 0, 1) > 0.96) {
     message("rgl NULL device in use, do you need to run rgl::rglwidget()?")
   }
  ## TODO need an rgl level classed object
  invisible(list(v = vb, is = vindex))
}
#' @name plot3d
#' @export
plot3d.PATH <- function(x, ...) {
  plot3d(silicate::SC(x), ...)
}
#' @name plot3d
#' @export
plot3d.sf <- function(x, ...) {
  plot3d(silicate::SC(x), ...)
}
#' @name plot3d
#' @export
plot3d.sfc <- function(x, ...) {
  plot3d(silicate::SC(x), ...)
}
#' @name plot3d
#' @export
plot3d.Spatial <- function(x, ...) {
  plot3d(silicate::SC(x), ...)
}
#' @name plot3d
#' @export
plot3d.trip <- function(x, ...) {
  plot3d(silicate::SC(x), ...)
}




#' @rdname mesh
#' @export
mesh.SpatialMultiPoints <- function(x, ...) {
  pr4 <- proj4string(x)
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialMultiPointsDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  class(tabs) <- "pointmesh"
  tabs
}


#' @rdname mesh
#' @export
mesh.SpatialPoints <- function(x, ...) {
  stop("you don't really need this function, just use `as.data.frame(x)`")
  pr4 <- proj4string(x)
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialPointsDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  class(tabs) <- "pointmesh"
  tabs
}

#' @export
plot.pointmesh <- function(x,  ...) {
  if (!"color_" %in% names(x$o)) {
    x$o$color_ <- viridis::viridis(nrow(x$o))
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

  pindex <- dplyr::inner_join(dplyr::inner_join(x$o[, c("object_", "color_")], x$b), 
                              x$bXv)
  
 # vindex <- dplyr::inner_join(x$bXv, vv, "vertex_")
#itex <- t(matrix(vindex$row_n, ncol = 2, byrow = TRUE))
  rgl::rgl.points(t(vb), col = pindex$color_, ...)
  
  
  invisible(list(v = vb, material = list(col = pindex$color_)))
}


#' Plot QUAD
#'
#' Plot a QUAD mesh. 
#' @param x a QUAD object
#' @param ... passed to set up plot when silicate mesh is plotted
#' @param add add to the current plot?
#' @name plot
#' @return returns the data plotted in the form used, invisibly
#' @export
#' @importFrom graphics plot
#' @export plot
#' @examples
#' plot(QUAD(raster::raster(volcano)))
plot.QUAD <- function(x, ..., add = FALSE) {
  if (!is.null(x$quad)) {
    x$object$crs <- x$meta$proj
    qd <- do.call(raster::raster, 
                  x$object[c("xmn", "xmx", "ymn", "ymx", "nrows", "ncols", "crs")])
    qd[] <- x$quad$value
    l <- list(...)
    if (is.null(l$col)) l$col <- viridis::viridis(100)
    l$x <- qd
    do.call(raster::plot, l)
    return(invisible(NULL))
  } 
  tr <- TRI(x)
  plot(tr, border = NA, ...)
  invisible(tr)
}


#' @name plot
#' @export
plot.linemesh <- function(x,  ..., add = FALSE) {
  .Deprecated(new = "plot.PATH or plot.SC", package = "anglr", old = "plot(<line_mesh>)")
  
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
  vv <- x$v["vertex_"]; vv$row_n <- seq(nrow(vv))
  pindex <- dplyr::inner_join(dplyr::inner_join(x$o[, c("object_", "color_")], x$l), 
                              x$lXv)
  
  vindex <- dplyr::inner_join(x$lXv, vv, "vertex_")
  itex <- t(matrix(vindex$row_n, ncol = 2, byrow = TRUE))
  if (!add & length(rgl::rgl.dev.list()) < 1L) rgl::rgl.clear()
  
  rgl::segments3d(t(vb)[itex,], col = pindex$color_, ...)
  
  #if ( rgl::rgl.useNULL()) rgl::rglwidget() 
  
  invisible(list(v = vb, it = itex))

}

#' @name plot
#' @export
plot.pointmesh <- function(x,  ..., add = FALSE) {
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
  if (!add & length(rgl::rgl.dev.list()) < 1L) rgl::rgl.clear()
  
  rgl::rgl.points(t(vb), col = pindex$color_, ...)
  
 # if ( rgl::rgl.useNULL()) rgl::rglwidget()
  invisible(list(v = vb, material = list(col = pindex$color_)))
}

#' @importFrom rgl as.mesh3d
#' @export
#' @name mesh3d
#' @importFrom rgl as.mesh3d
#' @export as.mesh3d
as.mesh3d.QUAD <- function(x, ...) {
  scl <- function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE))
  ## etc blah
  ob <- mkq_3d()
  #exy <- get_edges(x)
  #v <- tibble(x_ = exy[,1], y_ = exy[,2], z_ = if (is.null(x$vertex)) 0 else x$vertex$z_)
  v <- get_vertex(x)
  v[["z_"]] <- if (is.null(x$vertex$z_))  0 else x$vertex$z_
  ob$vb <- t(cbind(as.matrix(v[, c("x_", "y_", "z_")]), 1))
  qXv <- get_qXv(x)
  ob$ib <- matrix(qXv$vertex_, nrow = 4)
  cols <- viridis::viridis(min(c(1000, prod(unlist(x$object[c("nrows", "ncols")])))))
  #qXv <- x$quad_link_vertex
  if (!is.null(x$quad)) {
    qXv$value <- x$quad$value[qXv$quad_]
    
    
  } else {
    qXv$value <- x$vertex$z_[qXv$vertex_]
  }
  ob$material$color <- cols[scl(qXv$value) * length(cols) + 1]
  ob
}
#' @export
#' @name mesh3d
as.mesh3d.default <- function(x, ...) {
  as.mesh3d(TRI(x, ...))
}
#' @export
#' @name mesh3d
as.mesh3d.TRI <- function(x, ...) {
  if (!"color_" %in% names(x$object)) {
    x$object$color_ <- trimesh_cols(nrow(x$object))
  }
  haveZ <- "z_" %in% names(x$vertex)
  if (haveZ) {
    vb <- cbind(x$vertex$x_, x$vertex$y_, x$vertex$z_)
  } else {
    vb <- cbind(x$vertex$x_, x$vertex$y_, 0)
  }
  pindex <- x$triangle %>% #%>%  dplyr::inner_join(x$object_link_triangle,  "triangle_") %>% 
    dplyr::inner_join(x$object[, c("object_", "color_")], "object_") 
  if ("visible_" %in% names(pindex)) {
    
    pindex <- pindex %>% dplyr::filter(.data$visible_)
    if (nrow(pindex) < 1) warning("all visible_ property on '$triangle' are set to 'FALSE', nothing to plot")
  }
  ##vindex <- dplyr::inner_join(x$triangle, x$vertex, "vertex_")
  vindex <- match(c(t(as.matrix(pindex[c(".vx0", ".vx1", ".vx2")]))), x$vertex$vertex_)
  
  structure(list(vb = rbind(t(vb), 1), it = matrix(vindex, nrow = 3),
                           primitivetype = "triangle", 
                           material = list(col = rep(pindex$color_, each = 3)), 
                           normals = NULL, texcoords = NULL), 
                      class = c("mesh3d", "shape3d"))
  
}
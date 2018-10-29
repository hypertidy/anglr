#' 3D object plot
#'
#' For SC edges are matched to their object/s. One object's properties is applied as colour. 
#' If `color_` column is present on the data object table it is used. 
#' @param x silicate model, SC, TRI, ARC, or PATH
#' @param ... 
#' @param add 
#' 
#' @return vertices and segment indices, invisibly
#' @importFrom rgl plot3d
#' @export plot3d
#' @name plot3d
#' @examples
#' library(silicate)
#' x <- SC(sf::read_sf(system.file("shape", "nc.shp", package = "sf")) %>% 
#'    mutate(color_ = rainbow(100)))
#' plot3d(x); rglwidget()
#' worldz <- QUAD(gebco1)
#' ## an easy way to exaggerate z is to reduce the radius of the globe
#' plot3d(globe(worldz, gproj = "+proj=geocent +a=10000"))
#' @export
plot3d.SC <- function(x, ..., add = FALSE) {
  if (!"color_" %in% names(x$object)) {
    x$object$color_ <- trimesh_cols(nrow(x$object))
  }
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl required")
  Z <- if("z_" %in% names(x$vertex)) x$vertex$z_ else 0
  vb <- cbind(x$vertex$x_, x$vertex$y_, Z)

  pindex <- dplyr::inner_join(dplyr::inner_join(x$edge[c("edge_")], x$object_link_edge[c("edge_", "object_")], "edge_"), 
                              x$object[, c("object_", "color_")], "object_")
  ## one object wins
  pindex <- dplyr::distinct(pindex, .data$edge_, .keep_all = TRUE)
  vindex <- rbind(match(x$edge$.vx0, x$vertex$vertex_),
                  match(x$edge$.vx1, x$vertex$vertex_))
#browser()
  if (!add) {
    rgl::rgl.clear()
  }
  ## make a default set of colours if not passed in
  if ("col" %in% names(list(...))) {
    rgl::segments3d(vb[vindex,], ...)
   } else {
     rgl::segments3d(vb[vindex,], 
                     col = rep(pindex$color_, each = 2), ...)
     
    }
   #if (!is.null(getOption("rgl.useNULL")) && interactive() && runif(1, 0, 1) > 0.96) {
  #   message("rgl NULL device in use, do you need to run rgl::rglwidget()?")
  # }
  ## TODO need an rgl level classed object
  invisible(list(v = vb, is = vindex))
}
#' @name plot3d
#' @export
plot3d.QUAD <- function(x, ..., add = FALSE) {
  scl <- function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE))
  ## etc blah
  ob <- mkq_3d()
  exy <- get_edges(x)
  v <- tibble(x_ = exy[,1], y_ = exy[,2], z_ = if (is.null(x$vertex)) 0 else x$vertex$z_)
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
  ob$material$col <- cols[scl(qXv$value) * length(cols) + 1]
  if (!add) {
    rgl::rgl.clear()
  }
  rgl::shade3d(ob, ...)
  invisible(ob)
}


#' @name plot3d
#' @export
plot3d.PATH <- function(x, ..., add = FALSE) {
  plot3d(silicate::SC(x), ..., add = add)
}
#' @name plot3d
#' @export
plot3d.sf <- function(x, ..., add = FALSE) {
  plot3d(silicate::SC(x), ..., add = add)
}
#' @name plot3d
#' @export
plot3d.sfc <- function(x, ..., add = FALSE) {
  plot3d(silicate::SC(x), ..., add = add)
}
#' @name plot3d
#' @export
plot3d.Spatial <- function(x, ..., add = FALSE) {
  plot3d(silicate::SC(x), ..., add = add)
}
#' @name plot3d
#' @export
plot3d.trip <- function(x, ..., add = FALSE) {
  plot3d(silicate::SC(x), ..., add = add)
}


#' @name plot3d
#' @export
plot3d.ARC <- function(x, ..., add = FALSE) {
  if (!"color_" %in% names(x$object)) {
    x$object$color_ <- trimesh_cols(nrow(x$object))
  }
  haveZ <- "z_" %in% names(x$vertex)
  if (haveZ) {
    vb <- cbind(x$vertex$x_, x$vertex$y_, x$vertex$z_)
  } else {
    vb <- cbind(x$vertex$x_, x$vertex$y_, 0)
  }
  pindex <- dplyr::inner_join(x$object_link_arc, x$object[, c("object_", "color_")], "object_")
  vindex <- dplyr::inner_join(x$arc_link_vertex, x$vertex, "vertex_")
 v_id <- lapply(split(vindex, vindex$arc_), function(x) as.vector(path2seg(x$vertex_)))
  if (!add) {
    rgl::rgl.clear()
  }
 vindex <- match(unlist(v_id), x$vertex$vertex_)
  rgl::segments3d(vb[vindex,], 
                  col = rep(pindex$color_[match(x$arc_link_vertex$arc_, pindex$arc_)], each = 2))
  #if (!is.null(getOption("rgl.useNULL")) && interactive() && runif(1, 0, 1) > 0.96) {
  #  message("rgl NULL device in use, do you need to run rgl::rglwidget()?")
  #}
  ## TODO need an rgl level classed object
  invisible(list(v = vb, is = vindex))
  
}
#' @name plot3d
#' @export
plot3d.TRI <- function(x, ..., add = FALSE) {
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
    
  ##vindex <- dplyr::inner_join(x$triangle, x$vertex, "vertex_")
vindex <- match(c(t(as.matrix(pindex[c(".vx0", ".vx1", ".vx2")]))), x$vertex$vertex_)
  #v_id <- lapply(split(vindex, vindex$arc_), function(x) as.vector(path2seg(x$vertex_)))
  if (!add) {
    rgl::rgl.clear()
  }
  #vindex <- match(unlist(v_id), x$vertex$vertex_)
  rgl::triangles3d(vb[vindex,], col = rep(pindex$color_, each = 3))
  #if (!is.null(getOption("rgl.useNULL")) && interactive() && runif(1, 0, 1) > 0.96) {
  #  message("rgl NULL device in use, do you need to run rgl::rglwidget()?")
  #}
  ## TODO need an rgl level classed object
  invisible(list(vb = t(vb), it = t(vindex)))
  
}






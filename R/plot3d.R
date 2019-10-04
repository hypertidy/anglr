#' 3D object plot
#'
#' For SC edges are matched to their object/s. One object's properties is applied as colour.
#' If `color_` column is present on the data object table it is used.
#' @param x silicate model, SC, TRI, ARC, or PATH
#' @param ... passed to segments3d
#' @param add add to plot or not
#'
#' @return rgl shape3d types (note that "segment3d" is currently an imaginary shape3d type)
#' @importFrom rgl plot3d
#' @export plot3d
#' @name plot3d
#' @examples
#' library(silicate)
#' x <- SC(sf::read_sf(system.file("shape", "nc.shp", package = "sf")) %>%
#'    dplyr::mutate(color_ = rainbow(100)))
#' plot3d(x); rgl::rglwidget()
#' worldz <- QUAD(gebco1)
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
  #invisible(list(v = vb, is = vindex))
  invisible(structure(list(vb = rbind(t(vb), 0),
                           is = vindex,
                 primitivetype = "segment",
                 material = list(col = pindex$color_)),
            class = c("segment3d", "shape3d")))
}

shade3d.segment3d <- function(x, ...) {
  rgl::segments3d(t(x$vb[1:3,])[x$is, ], col = rep(x$material$col, each = 2), ...)
}
#' @name plot3d
#' @export
plot3d.QUAD <- function(x, ..., add = FALSE) {
  ob <- as.mesh3d(x)
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
  ## there's no shape3d for segments
  invisible(structure(list(vb = rbind(t(vb), 0),
                           is = matrix(vindex, nrow = 2),
                           primitivetype = "segment",
                           material = list(col = pindex$color_)),
                      class = c("segment3d", "shape3d")))

}
#' @name plot3d
#' @export
plot3d.TRI <- function(x, ..., add = FALSE) {

  #v_id <- lapply(split(vindex, vindex$arc_), function(x) as.vector(path2seg(x$vertex_)))
  if (!add) {
    rgl::rgl.clear()
  }
  ob <- as.mesh3d(x)
  #vindex <- match(unlist(v_id), x$vertex$vertex_)
  rgl::triangles3d(t(ob$vb[, ob$it]), col = ob$material$color)
}






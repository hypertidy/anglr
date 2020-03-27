#' 3D object plot
#'
#' For SC edges are matched to their object/s. One object's properties is applied as colour.
#' If `color_` column is present on the data object table it is used.
#' @param x silicate model, SC, TRI, ARC, or PATH
#' @param ... passed to material properties
#' @param add add to plot or not
#'
#' @return rgl shape3d types (note that "segment3d" is currently an imaginary shape3d type)
#' @importFrom rgl plot3d persp3d
#' @export plot3d
#' @export persp3d
#' @examples
#' library(silicate)
#' x <- SC(sf::read_sf(system.file("shape", "nc.shp", package = "sf")) %>%
#'    dplyr::mutate(color_ = rainbow(100)))
#' plot3d(x)
#' @export
#' @name plot3d
plot3d.TRI <- function(x, ...) {
  persp3d(x, ...)
}
#' @export
#' @name plot3d
plot3d.TRI0 <- function(x, ...) {
  persp3d(x, ...)
}
#' @export
#' @name plot3d
plot3d.DEL <- function(x, ...) {
  persp3d(x, ...)
}
#' @export
#' @name plot3d
plot3d.DEL0 <- function(x, ...) {
  persp3d(x, ...)
}
#' @export
#' @name plot3d
plot3d.QUAD <- function(x, ...) {
  persp3d(x, ...)
}

#' @export
#' @name plot3d
plot3d.matrix <- function(x, ...) {
  persp3d(x, ...)
}
#' @export
#' @name plot3d
plot3d.BasicRaster <- function(x, ...) {
  persp3d(x, ...)
}

## linear types:


#' @export
#' @name plot3d
plot3d.sc <- function(x, ...) {
  ## try the universal way
  plot3d(silicate::SC0(x), ...)
}

#' @export
#' @name plot3d
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

                 material = list(col = pindex$color_)),
            class = c("segment3d", "shape3d")))
}
#' @export
#' @name plot3d
plot3d.SC0 <- function(x, ..., add = FALSE) {
  if (!"color_" %in% names(x$object)) {
    x$object$color_ <- trimesh_cols(nrow(x$object))
  }
  vv <- silicate::sc_vertex(x)
  vb <- as.matrix(vv[c("x_", "y_")])
  if ("z_" %in%  names(vv)) {
    vb <- cbind(vb, z = vv$z_)
    } else {
      vb <- cbind(vb, z = 0)
    }
  vb <- cbind(vb, h = 1)
  lt <- silicate::sc_object(x)$topology_
  pindex <- rep(x$object$color_, unlist(lapply(lt, nrow)))
  vindex <- t(do.call(rbind, lapply(lt, function(adf) adf[c(".vx0", ".vx1")])))
  if (!add) {
    rgl::rgl.clear()
  }
  if ("col" %in% names(list(...))) {
    rgl::segments3d(vb[vindex,], ...)
  } else {
    rgl::segments3d(vb[vindex,],
                    col = rep(pindex, each = 2), ...)

  }

  invisible(structure(list(vb = rbind(t(vb), 0),
                           is = vindex,

                           material = list(col = pindex)),
                      class = c("segment3d", "shape3d")))
}


#' @export
#' @name plot3d
plot3d.PATH <- function(x, ..., add = FALSE) {
  plot3d(silicate::SC0(x), ..., add = add)
}
#' @export
#' @name plot3d
plot3d.sf <- function(x, ..., add = FALSE) {
  plot3d(silicate::SC0(x), ..., add = add)
}
#' @export
#' @name plot3d
plot3d.sfc <- function(x, ..., add = FALSE) {
  plot3d(silicate::SC0(x), ..., add = add)
}
#' @export
#' @name plot3d
plot3d.Spatial <- function(x, ..., add = FALSE) {
  plot3d(silicate::SC0(x), ..., add = add)
}
#' @export
#' @name plot3d
plot3d.trip <- function(x, ..., add = FALSE) {
  plot3d(silicate::SC0(x), ..., add = add)
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

                           material = list(col = pindex$color_)),
                      class = c("segment3d", "shape3d")))

}






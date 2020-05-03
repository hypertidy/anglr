#' 3D object plot
#'
#' This is the workhorse function for anglr, the idea is that just about
#' anything can be plotted in a 3D scene, polygons, lines, rasters, matrix.
#' These objects from sp, sf, raster, trip, and silicate should all work.
#'
#' The function [plot3d()] covers the full suite of plotting functions from
#' [rgl::plot3d()] for meshes, points, and lines. This main function includes the
#'  family of [dot3d()], [wire3d()], and [persp3d()] and each works with
#' matrix, raster, sf, sp, trip, RTriangle, and silicate models. Each of the
#' mesh-surface forms rely on [as.mesh3d()] conversion behind the scenes,
#' whereas [plot3d()] for the linear types (sf, sp, trip, and from silicate SC,
#' SC0, PATH, PATH0, and ARC0) all are plotted using rgl segments without going
#' through a triangulated surface form. This reflects their underlying topology
#' when it comes to 3D visualization and analysis.
#'
#' If the scene looks funny the aspect ratio might be poor, we've decided not to
#' automatically update this with normal plots, but running `auto_3d()` will
#' attempt to set a reasonable aspect ratio. It can also be used to set
#' exaggerations in different axes.
#'
#' For SC edges are matched to their object/s. One object's properties is
#' applied as colour. If `color_` column is present on the data object table it
#' is used.
#'
#' If the argument 'color' is used, this is passed down to the rgl plot function  -
#' and will be applied per primitive, not per silicate object. This provides flexibility
#' but does require knowledge of the underlying structures in use.
#' @param x silicate model, SC, TRI, ARC, or PATH
#' @param ... passed to material properties
#' @param add add to plot or not
#' @seealso [wire3d] [as.mesh3d] [persp3d] [dot3d] [shade3d]
#' @return rgl shape3d types (note that "segment3d" is currently an imaginary
#'   shape3d type)
#' @importFrom rgl plot3d persp3d
#' @export plot3d
#' @export persp3d
#' @examples
#' library(silicate)
#' cad_tas$color_ <- rainbow(nrow(cad_tas))
#' x <- SC(cad_tas)
#' plot3d(x)
#'
#' ## plot3d anything
#' plot3d(volcano)
#' wire3d(volcano)
#' dot3d(volcano)
#'
#' \donttest{
#' plot3d(cad_tas)
#' persp3d(cad_tas)
#' wire3d(cad_tas)
#' dot3d(cad_tas)
#' }
#' ## add Z elevation to an sf polygon in a mesh
#' plot3d(copy_down(as.mesh3d(silicate::minimal_mesh), raster::raster(volcano)))
#'
#' ## but make it much more interesting
#' \donttest{
#' plot3d(copy_down(as.mesh3d(DEL(silicate::minimal_mesh, max_area = 0.0001)),
#'   raster::raster(-volcano)), col = c("black", "orange")); auto_3d()
#' wire3d(silicate::minimal_mesh)
#' }
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
  ## watch out
  nc <- dim(x)[2L]
  if (nc %in% c(2, 3)) {
    ## they probably wanted a scatter plot, not a surface
    if (nc == 2L) {
      rgl::plot3d(x = x[, 1L], y = x[, 2L], ...)
    }
    if (nc == 3L) {
      rgl::plot3d(x = x[, 1L], y = x[, 2L], z = x[, 3L], ...)
    }
  } else {
    persp3d(x, ...)
  }
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
  if ("color" %in% names(list(...))) {
    rgl::segments3d(vb[vindex,], ...)
  } else {
    rgl::segments3d(vb[vindex,],
                    color = rep(pindex, each = 2), ...)

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
plot3d.triangulation <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x), ..., add = add)
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
  # if (!"color_" %in% names(x$object)) {
  #   x$object$color_ <- trimesh_cols(nrow(x$object))
  # }
  haveZ <- "z_" %in% names(x$vertex)
  if (haveZ) {
    vb <- cbind(x$vertex$x_, x$vertex$y_, x$vertex$z_)
  } else {
    vb <- cbind(x$vertex$x_, x$vertex$y_, 0)
  }
  pindex <- dplyr::inner_join(x$object[, c("object_")], x$object_link_arc, "object_")
  vindex <- pindex %>% dplyr::inner_join(x$arc_link_vertex, "arc_") %>% dplyr::inner_join(x$vertex, "vertex_")
  vindex[["color_"]] <- viridis::viridis(length(unique(vindex$arc_)))[as.integer(factor(vindex$arc_))]
 v_id <- lapply(split(vindex, vindex$arc_)[unique(vindex$arc_)], function(x) as.vector(t(path2seg(x$vertex_))))
 ## super dodgy but I just can't see this through rn MDS 2020-04-05
 c_id <- lapply(split(vindex, vindex$arc_)[unique(vindex$arc_)], function(x) as.vector(t(path2seg(x$color_))))
#browser()
  if (!add) {
    rgl::rgl.clear()
  }
 vidx <- match(unlist(v_id), x$vertex$vertex_)
  rgl::segments3d(vb[vidx,],
                  col = unlist(c_id))

#                  col = rep(pindex$color_[match(x$arc_link_vertex$arc_, pindex$arc_)], each = 2))
  ## there's no shape3d for segments
  invisible(structure(list(vb = rbind(t(vb), 0),
                           is = matrix(vindex, nrow = 2),

                           material = list(col = unlist(c_id))),
                      class = c("segment3d", "shape3d")))

}






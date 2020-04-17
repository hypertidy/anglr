#' Copy down values to vertices
#'
#' Copy down provides ways to transfer object level data values to
#' vertex level.
#'
#' Various methods are used depending on the second argument `z`.
#'
#' If `z` is a raster (`BasicRaster`) a numeric value for each vertex is found by bilinear
#' interpolation using `raster::extract(raster, vertex, method = "bilinear")`. Vertices
#' are transformed into the space used by the raster if possible.
#'
#' If `z` is a character value, that column is taken from the object table.
#'
#' The `.id` argument must be character and exist as a column name in the object table.
#'
#' If z is a vector or a constant value it's simply copied down.
#'
#' No checking is done on the type of the result, and so there's nothing to stop the use of the recycling rule
#' to expand out values, and nothing to stop the use of non numeric values being copied down.
#'
#' Use [silicate::TRI0()][silicate::TRI0] or [DEL0()] or [silicate::SC0()] to convert
#' various spatial formats into suitable forms for this function.
#' @param x a mesh3d or a silicate object
#' @param z object specifying values to copy down, a vector of values, a column name, a raster (see details)
#' @param ... currently ignored
#' @param .id character value, the name of the resulting column in the vertices, default is "z_"
#'
#' @return a mesh3d or silicate model with vertex values copied to vertices (depending
#' on the input argument 'x')
#' @export
#'
#' @examples
#' library(raster)
#' r <- raster(volcano)
#' cl <- silicate::SC(rasterToContour(r))
#' plot3d(copy_down(cl, r))
#' ## looks funny?
#' auto_3d(z = 15)
#'
#' \donttest{
#' sc <- copy_down(SC0(cont_tas), "ELEVATION")
#' sc$object$color_ <- hcl.colors(nrow(sc$object), "YlOrRd")
#'  plot3d(sc)
#'
#'  ## a planar straight line graph with x, y (UTM) and ELEVATION (metres)
#'  sc
#' }
copy_down <- function(x, z = NULL, ..., .id = "z_") {
  stopifnot(is.character(.id))

  UseMethod("copy_down", )
}

#' @name copy_down
#' @export
copy_down.mesh3d <- function(x, z = NULL, ..., .id = "z_") {
  #z <- find_z(x, z)
  if (inherits(z, "BasicRaster")) {
    zz <- raster::extract(z, t(x$vb[1:2, ]), method = "bilinear")
    if (mean(is.na(zz)) > 0.8) {
      prop <- "most"
      if (all(is.na(zz))) {
        prop <- "ALL"
      }
      warning(sprintf("%s of the z values extracted are NA, perhaps the raster is \nin a different projection to the mesh? \n (very likely that visualization will not work)", prop))
    }
    x$vb[3, ] <- zz
    return(x)
    #return(copy_downRaster(x, z = z, ..., .id = .id) )
  }
  stop("breaking the mesh is not yet possible with mesh3d")
  denorm_PRIM_addZ(x, z = z, ..., .id = .id)
}

#' @name copy_down
#' @export
#' @importFrom dplyr inner_join
copy_down.SC <- function(x, z = NULL, ..., .id = "z_") {
  z <- find_z(x, z)
  if (inherits(z, "BasicRaster")) {
   return(copy_downRaster(x, z = z, ..., .id = .id) )
  }
  denorm_PRIM_addZ(x, z = z, ..., .id = .id)
}
#' @name copy_down
#' @export
copy_down.SC0 <- function(x, z = NULL, ..., .id = "z_") {
  ## FIXME: obvsly inefficient, should work the other way
 copy_down(SC(x), z  = z, ..., .id = .id)
}
#' @name copy_down
#' @export
copy_down.TRI <- function(x, z = NULL, ..., .id = "z_") {
  z <- find_z(x, z)
  if (inherits(z, "BasicRaster")) {
    return(copy_downRaster(x, z = z, ..., .id = .id) )
  }

  denorm_PRIM_addZ(x, z = z, ..., .id = .id)

}
#' @name copy_down
#' @export
copy_down.TRI0 <- function(x, z = NULL, ..., .id = "z_") {
  ## FIXME: obvsly inefficient, should work the other way
  copy_down(silicate::TRI(x), z  = z, ..., .id = .id)
}
#' @name copy_down
#' @export
copy_down.PATH <- function(x, z = NULL, ..., .id = "z_") {
  z <- find_z(x, z)
  if (inherits(z, "BasicRaster")) {
    return(copy_downRaster(x, z = z, ..., .id = .id) )
  }

  denorm_SEQ_addZ(x, z = z, ..., .id = .id)
}
#' @name copy_down
#' @export
copy_down.PATH0 <- function(x, z = NULL, ..., .id = "z_") {
  ## FIXME: obvsly inefficient, should work the other way
  copy_down(silicate::PATH(x), z  = z, ..., .id = .id)
}
#' @name copy_down
#' @export
copy_down.ARC <- function(x, z = NULL, ..., .id = "z_") {
  z <- find_z(x, z)
  if (inherits(z, "BasicRaster")) {
    return(copy_downRaster(x, z = z, ..., .id = .id) )
  }

  denorm_SEQ_addZ(x, z = z, ..., .id = .id)
}

#' @name copy_down
#' @export
#' @importFrom stats setNames
copy_down.QUAD <- function(x, z = NULL, ..., .id = "z_") {
 #vertex <- tibble(x_ = exy[,1], y_ = exy[,2], z_ = 0)
  z <- find_z(x, z)
  if (is.null(z)) {
    qXv <- get_qXv(x)
    ##
    if (is.null(x$quad)) stop("nothing to copy down")
    qXv$value <- x$quad$value[qXv$quad_]
    qXv <- dplyr::distinct(qXv, .data$vertex_, .keep_all = TRUE)

    ##if (!"vertex_" %in% names(vertex)) vertex[["vertex_"]] <- seq(nrow(vertex))
    vertex <- setNames(tibble(a = qXv$value), .id)
  } else {
    exy <- get_edges(x)
    vertex <- setNames(tibble(a = raster::extract(z, exy)), .id)
  }

  x$vertex <- vertex
  x$quad <- NULL
  x
}

#  #@name copy_down
#  #@export
# copy_down.SC <- function(x, z = NULL, ..., .id = "z_") {
#   dmap <- x$object[c(z, "object_")] %>%
#     dplyr::inner_join(x$object_link_edge[c("edge_", "object_")], "object_") %>%
#     dplyr::select(z, edge_) %>%
#     dplyr::inner_join(x$edge, "edge_")  %>%
#     dplyr::select(z, vertex_) %>% dplyr::distinct(vertex_, .keep_all = TRUE)
#   x$vertex[[.id]] <- dmap[[z]][match(dmap$vertex_, x$vertex$vertex_)]
#
# }
#  #@name copy_down
#  #@export
# copy_down.PATH <- function(x, z = NULL, ..., .id = "z_") {
#   stopifnot(.id %in% names(x$object))
#   if (is.character(z)) {
#     ## we need the join_ramp
#     dmap <- x$object[c(z, "object_")] %>%
#       dplyr::inner_join(x$path[c("path_", "object_")], "object_") %>%
#       dplyr::select(z, path_) %>%
#       dplyr::inner_join(x$path_link_vertex, "path_")  %>%
#       dplyr::select(z, vertex_) %>% dplyr::distinct(vertex_, .keep_all = TRUE)
#     x$vertex[[.id]] <- dmap[[z]][match(dmap$vertex_, x$vertex$vertex_)]
#   }
#   x
# }
#  #@name copy_down
#  #@export
# copy_down.TRI <- function(x, z = NULL, ..., .id = "z_") {
#   stopifnot(.id %in% names(x$object))
#   if (is.character(z)) {
#     ## we need the join_ramp
#     dmap <- x$object[c(z, "object_")] %>%
#       dplyr::inner_join(x$triangle)
# ##    WIP this is way trickier because we have to split the vertices again
#     }
#   x
# }

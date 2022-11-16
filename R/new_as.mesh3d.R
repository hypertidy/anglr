## copy color_ from object

#' @name as.mesh3d
#' @export
as.mesh3d.ARC <- function(x, ...) {
  as.mesh3d(SC0(x), ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.SC0 <- function(x, ...) {
  v <- silicate::sc_vertex(x)
  if (!"z_" %in% names(v)) v[["z_"]] <- 0
  l <- silicate::sc_edge(x)
  rgl::mesh3d(vertices = rbind(x = v[["x_"]], y = v[["y_"]], z = v[["z_"]], h = 1),
              segments = rbind(l[,1L, drop = TRUE], l[,2L, drop = TRUE]), ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.SC <- function(x, ...) {
  as.mesh3d(SC0(x), ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.PATH0 <- function(x, ...) {
  as.mesh3d(SC0(x), ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.PATH <- function(x, ...) {
  as.mesh3d(SC0(x), ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.sfc_LINESTRING <- function(x, ...) {
  as.mesh3d(SC0(x), ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.sfc_MULTILINESTRING <- function(x, ...) {
  as.mesh3d(SC0(x), ...)
}

#' @name as.mesh3d
#' @export
as.mesh3d.sfc_POINT <- function(x, ...) {
  v <- silicate::sc_vertex(x)
  if (!"z_" %in% names(v)) v[["z_"]] <- 0
  rgl::mesh3d(vertices = rbind(x = v[["x_"]], y = v[["y_"]], z = v[["z_"]], h = 1),
             points = seq_len(dim(v)[1L]), ...)

}
#' @name as.mesh3d
#' @export
as.mesh3d.sfc_MULTIPOINT <- function(x, ...) {
 # g <- gibble::gibble(x)
  v <- silicate::sc_vertex(x)
  if (!"z_" %in% names(v)) v[["z_"]] <- 0
  rgl::mesh3d(vertices = rbind(x = v[["x_"]], y = v[["y_"]], z = v[["z_"]], h = 1),
              points = seq_len(dim(v)[1L]), ...)

}

# ## copy color_ from object
#
# as.mesh3d.ARC <- function(x, ...) {
#   as.mesh3d(SC0(x), ...)
# }
# as.mesh3d.SC0 <- function(x, ...) {
#   v <- sc_vertex(x)
#   if (!"z_" %in% names(v)) v[["z_"]] <- 0
#   l <- sc_edge(x)
#   rgl::mesh3d(vertices = rbind(x = v[["x_"]], y = v[["y_"]], z = v[["z_"]], h = 1),
#               segments = rbind(l[,1L, drop = TRUE], l[,2L, drop = TRUE]), ...)
# }
#
# as.mesh3d.SC <- function(x, ...) {
#   as.mesh3d(SC0(x), ...)
# }
# as.mesh3d.PATH0 <- function(x, ...) {
#   as.mesh3d(SC0(x), ...)
# }
# as.mesh3d.PATH <- function(x, ...) {
#   as.mesh3d(SC0(x), ...)
# }
#
# as.mesh3d.sfc_LINESTRING <- function(x, ...) {
#   as.mesh3d(SC0(x), ...)
# }
#
# as.mesh3d.sfc_MULTILINESTRING <- function(x, ...) {
#   as.mesh3d(SC0(x), ...)
# }
#
#
# as.mesh3d.sfc_POINT <- function(x, ...) {
#   v <- sc_vertex(x)
#   if (!"z_" %in% names(v)) v[["z_"]] <- 0
#   rgl::mesh3d(vertices = rbind(x = v[["x_"]], y = v[["y_"]], z = v[["z_"]], h = 1),
#              points = seq_len(dim(v)[1L]), ...)
#
# }
#
# as.mesh3d.sfc_MULTIPOINT <- function(x, ...) {
#  # g <- gibble::gibble(x)
#   v <- sc_vertex(x)
#   if (!"z_" %in% names(v)) v[["z_"]] <- 0
#   rgl::mesh3d(vertices = rbind(x = v[["x_"]], y = v[["y_"]], z = v[["z_"]], h = 1),
#               points = seq_len(dim(v)[1L]), ...)
#
# }
#

#' QUAD model
#'
#' The QUAD model is a silicate-like model for raster data, with
#' implicit geometry.
#'
#' The object table only stores the raster extent, and the pixel values
#' are on the 'quad' table. This is only supported for single-layer 2D
#' regular rasters.
#'
#' The 'color_' idiom works, but must be put on the '$quad' table.
#' Very much still in-development.
#' @param x raster alike, or a matrix
#' @param ... ignored
#'
#' @return QUAD model
#' @export
#'
#' @examples
#' qq <-  QUAD(raster::raster(volcano))
#' mesh_plot(qq)
#' qq$quad$color_ <- rep(c("black", "white"), length.out = nrow(qq$quad))
#' mesh_plot(qq)
#' qq$quad$color_ <- palr::image_pal(qq$quad$value, col = grey.colors(10))
#' mesh_plot(qq)
QUAD <- function(x, ...) {
  UseMethod("QUAD")
}
#' @name QUAD
#' @export
QUAD.matrix <- function(x, ...) {
  r <- raster::setExtent(raster::raster(t(x)[ncol(x):1, ]),
                         raster::extent(0, nrow(x), 0, ncol(x)))

  QUAD(r, ...)
}
#' @name QUAD
#' @export
#' @importFrom raster xmin xmax ymin ymax
QUAD.BasicRaster <- function(x, ...) {
  x <- x[[1]]  ## just the oneth raster for now
  pr4 <- get_proj(x)
  #exy <- edges0(x)
  #ind <- apply(prs0(seq(ncol(x) + 1)), 1, p_4, nc = ncol(x) + 1)
  ## all face indexes
  #ind0 <- as.vector(ind) +
  #  rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x))
  #ind1 <- matrix(ind0, nrow = 4L)

  o <- tibble(object_ = 1L, xmn = xmin(x), xmx = xmax(x), ymn = ymin(x), ymx = ymax(x), nrows = nrow(x), ncols = ncol(x))
  #qXv <- tibble(vertex_ = as.vector(ind1), quad_ = rep(seq(ncol(ind1)), each = 4))
  #v <- tibble(x_ = exy[,1], y_ = exy[,2], z_ = 0)
  l <- list(object = o, quad = tibble(value = raster::values(x))) ## , quad = tibble(quad = seq(ncol(ind1)), object_ = 1, value = raster::values(x)), quad_link_vertex = qXv, vertex = v)
  l$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", ctime = format(Sys.time(), tz = "UTC"))
  class(l) <- c("QUAD", "sc")
  l
}



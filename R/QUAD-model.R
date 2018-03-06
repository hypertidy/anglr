#' QUAD model
#' 
#' The QUAD model
#'
#' @param x raster alike
#' @param ... ignored
#'
#' @return QUAD model
#' @export
#'
#' @examples
#' QUAD(raster::raster(volcano))
QUAD <- function(x, ...) {
  UseMethod("QUAD")
}
#' @name QUAD
#' @export
QUAD.BasicRaster <- function(x, ...) {
   x <- x[[1]]  ## just the oneth raster for now
  pr4 <- get_proj(x)
  exy <- edges0(x)
  ind <- apply(prs0(seq(ncol(x) + 1)), 1, p_4, nc = ncol(x) + 1)
  ## all face indexes
  ind0 <- as.vector(ind) +
    rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x))
  ind1 <- matrix(ind0, nrow = 4L)
#  if (na.rm) {
#    ind1 <- ind1[,!is.na(values(x))]
#  }
  o <- tibble(object_ = 1L, xmin = xmin(x), xmax = xmax(x), ymin = ymin(x), ymax = ymax(x), nrow = nrow(x), ncol = ncol(x))
  qXv <- tibble(vertex_ = as.vector(ind1), quad_ = rep(seq(ncol(ind1)), each = 4))
  #z <- raster::extract(x, exy, method = "bilinear")
  v <- tibble(x_ = exy[,1], y_ = exy[,2], z_ = 0)
  l <- list(object = o, quad = tibble(quad = seq(ncol(ind1)), object_ = 1, value = raster::values(x)), quad_link_vertex = qXv, vertex = v)
  l$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", ctime = format(Sys.time(), tz = "UTC"))
  class(l) <- c("QUAD", "sc")
  l
}

mkq_3d <- function() {
  structure(list(vb = NULL, ib = NULL, primitivetype = "quad",
                 material = list(), normals = NULL, texcoords = NULL), .Names = c("vb",
                                                                                  "ib", "primitivetype", "material", "normals", "texcoords"), class = c("mesh3d",
                                                                                                                                                        "shape3d"))
  
}
p_4 <- function(xp, nc) {
  (xp + c(0, 0, rep(nc, 2)))[c(1, 2, 4, 3)]
}
#' @importFrom utils tail head
prs0 <- function(x) {
  cbind(head(x, -1), tail(x, -1))
}
edges0 <- function(x) {
  #eps <- sqrt(.Machine$double.eps)
  #as.matrix(expand.grid(seq(xmin(x), xmax(x) -eps, length = ncol(x) + 1),
  #                      seq(ymax(x), ymin(x) + eps, length = nrow(x) + 1)
  #))
  as.matrix(expand.grid(seq(xmin(x), xmax(x), length = ncol(x) + 1),
                        seq(ymax(x), ymin(x), length = nrow(x) + 1)
  ))
}
#' Raster anglr
#' 
#' Colours not supported, this just gives the viridis palette sequentially. 
#' @param z \code{\link[raster]{raster}}, by default \code{x} is used
#' @param na.rm remove missing values
#' @return quad_mesh
#' @name anglr
#' @export
#' @importFrom raster projection values xmin xmax ymin ymax
#' @examples
#' library(raster)
#' w <- raster(volcano)
#' plot(anglr(w/300))
#' 
anglr.RasterLayer <-function (x, z = NULL, ..., na.rm = TRUE, type = NULL, max_area = NULL) {
  if (!is.null(max_area)) {
    pixlen <- sqrt(max_area)
    fact <- ceiling(mean(res(x)/pixlen))
    if (fact > 1) x <- raster::disaggregate(x, fact = fact, method = "bilinear")
  }
  if (is.null(z)) z <- x
  x <- x[[1]]  ## just the oneth raster for now
  pr4 <- projection(x)
  exy <- edges0(x)
  ind <- apply(prs0(seq(ncol(x) + 1)), 1, p_4, nc = ncol(x) + 1)
  ## all face indexes
  ind0 <- as.vector(ind) +
    rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x))
  ind1 <- matrix(ind0, nrow = 4)
  if (na.rm) {
    ind1 <- ind1[,!is.na(values(x))]
  }
  o <- tibble(object_ = 1, xmin = xmin(x), xmax = xmax(x), ymin = ymin(x), ymax = ymax(x), nrow = nrow(x), ncol = ncol(x), proj = projection(x))
  qXv <- tibble(vertex_ = as.vector(ind1), quad_ = rep(seq(ncol(ind1)), each = 4))
  if (!is.null(z)) z <- raster::extract(z, exy, method = "bilinear") else z <- 0
  v <- tibble(x_ = exy[,1], y_ = exy[,2], z_ = z)
  l <- list(o = o, qd = tibble(quad = seq(ncol(ind1)), object_ = 1), qXv = qXv, v = v)
  l$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", ctime = format(Sys.time(), tz = "UTC"))
  class(l) <- "quad_mesh"
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
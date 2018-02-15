#' @noRd
#' @examples 
#' library(anglr)
#' library(raster)
#' r <- raster(volcano)
#' a <- anglr(r)
#' x <- quadToTriangle(a)
quadToTriangle <- function(x) {
  v <- x$v
  v$vertex_ <- seq(nrow(v))
  meta <- x$meta
  tab <- x$qXv
  n4 <- nrow(tab) / 4L
  tXv <- tibble::tibble(vertex_ = x$qXv$vertex_[rep(c(1, 2, 3, 1, 3, 4), n4) + rep(seq(1, length = n4, by = 4)-1, each = 6)])
  tXv$triangle_ <- rep(seq(nrow(tXv)/3), each = 3)
  x <- list(o = tibble::tibble(object_ = "1"), t = tibble::tibble(triangle_ = seq(nrow(tXv)/3), object_ = "1"), 
            tXv = tXv, v = v, meta = meta)
  class(x) <- "trimesh"
  x
}


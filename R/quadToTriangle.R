#' TRI model extensions
#'
#' TRI model from silicate is extended by methods for QUAD. 
#' 
#' @param x QUAD
#' @export
#' @examples 
#' library(anglr)
#' library(raster)
#' r <- setExtent(raster(volcano), extent(0, nrow(volcano), 0, ncol(volcano)))
#' a <- QUAD(r)
#' x <- copy_down(TRI(a), r)
#' nrow(x$vertex)
#' mesh <- DEL(x, max_area = .2)
#' mesh <- copy_down(mesh, r)
#' nrow(mesh$vertex)
#' @importFrom silicate TRI
#' @export TRI
TRI.QUAD <- function(x, ...) {
  quadToTriangle(x)
}



quadToTriangle <- function(x) {
  v <-x$vertex
  v$vertex_ <- seq(nrow(v))
  meta <- x$meta
  qXv <- x$quad_link_vertex
  n4 <- nrow(qXv) / 4L
  tXv_long <- tibble::tibble(vertex_ = qXv$vertex_[rep(c(1, 2, 3, 1, 3, 4), n4) + rep(seq(1, length = n4, by = 4)-1, each = 6)])
tXv <- tibble::tibble(triangle_ = silicate::sc_uid(nrow(tXv_long)/3))

tXv[c(".vertex0", ".vertex1", ".vertex2")] <- as.data.frame(matrix(as.integer(tXv_long$vertex_), byrow = TRUE, ncol = 3))
    #tXv$triangle_ <- silicate::sc_uid(nrow(tXv)/3)[rep(seq(nrow(tXv)/3), each = 3)]
    tXv$visible <- TRUE
    uid <- silicate::sc_uid(nrow(v))
    v$vertex_ <- uid[v$vertex_]
    tXv$.vertex0 <- uid[tXv$.vertex0]
    tXv$.vertex1 <- uid[tXv$.vertex1]
    tXv$.vertex2 <- uid[tXv$.vertex2]
    
  x <- list(object = tibble::tibble(object_ = "1"), object_link_triangle = 
              tibble::tibble(triangle_ = tXv$triangle_, object_ = "1"), 
            triangle = tXv, 
            vertex = v, meta = meta)
  class(x) <- c("TRI", "sc")
  x
}


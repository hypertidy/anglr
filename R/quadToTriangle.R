#' TRI model extensions
#'
#' TRI model from silicate is extended by methods for QUAD. 
#' 
#' @param x QUAD
#' @param ... reserved
#' @name TRI
#' @export
#' @export TRI
#' @examples 
#' library(anglr)
#' library(raster)
#' r <- setExtent(raster(volcano), extent(0, nrow(volcano), 0, ncol(volcano)))
#' a <- QUAD(r)
#' x <- copy_down(TRI(a), r)
#' nrow(x$vertex)
#' sc <- silicate::SC(x)
#' mesh <- DEL(sc, max_area = .2)
#' mesh <- copy_down(mesh, r)
#' nrow(mesh$vertex)
#' @importFrom silicate TRI sc_uid
#' @export TRI
TRI.QUAD <- function(x, ...) {
  out <- quadToTriangle(x)
  ## try expanding object to pairs of triangles
  f <- factor(x$quad$value)
  
  out$object <- tibble::tibble(object_ = silicate::sc_uid(nlevels(f)), 
                               value = levels(f))
  out$triangle$object_ <- rep(out$object$object_, each = 2L)
  #out$object_link_triangle$object_ <- rep(out$object$object_[f], each = 2)
#  out$
  out
}



quadToTriangle <- function(x) {
  exy <- get_edges(x)
  v <- tibble(x_ = exy[,1], y_ = exy[,2])
  if (is.null(x$vertex)) {
   v$z_ <- 0
  } else {
    v$z_ <- x$vertex$z_
  }
  v$vertex_ <- seq(nrow(v))
  meta <- x$meta
  qXv <- get_qXv(x)
  n4 <- nrow(qXv) / 4L
  tXv_long <- tibble::tibble(vertex_ = qXv$vertex_[rep(c(1, 2, 3, 1, 3, 4), n4) + rep(seq(1, length = n4, by = 4)-1, each = 6)])
tXv <- tibble::tibble(triangle_ = silicate::sc_uid(nrow(tXv_long)/3))

tXv[c(".vx0", ".vx1", ".vx2")] <- as.data.frame(matrix(as.integer(tXv_long$vertex_), byrow = TRUE, ncol = 3))
    #tXv$triangle_ <- silicate::sc_uid(nrow(tXv)/3)[rep(seq(nrow(tXv)/3), each = 3)]
    tXv$visible <- TRUE
    uid <- silicate::sc_uid(nrow(v))
    v$vertex_ <- uid[v$vertex_]
    tXv$.vx0 <- uid[tXv$.vx0]
    tXv$.vx1 <- uid[tXv$.vx1]
    tXv$.vx2 <- uid[tXv$.vx2]
    tXv$object_ <- "1"  ## no link table any more
  x <- list(object = tibble::tibble(object_ = "1"),  
            triangle = tXv, 
            vertex = v, meta = meta)
  class(x) <- c("TRI", "sc")
  x
}


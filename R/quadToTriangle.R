#' TRI model extensions
#'
#' TRI model from silicate is extended by methods for QUAD.
#'
#' @param x QUAD
#' @param ... reserved
#' @export
#' @name TRI.QUAD
#' @aliases TRI
#' @return a TRI model, as per silicate package
#' @examples
#' library(anglr)
#' library(raster)
#' v <- volcano[1:10, 1:6]
#' r <- setExtent(raster(v), extent(0, nrow(v), 0, ncol(v)))
#' a <- QUAD(r)
#' x <- copy_down(TRI(a), r)
#' nrow(x$vertex)
#' sc <- silicate::SC(x)
#' #mesh <- DEL(sc, max_area = .2)
#' #mesh <- copy_down(mesh, r)
#' #nrow(mesh$vertex)
TRI.QUAD <- function(x, ...) {
  out <- quadToTriangle(x)
  ## explode the objects
  out$object <- dplyr::slice(out$object, rep(1L, nrow(out$triangle)/2))
  out$object$object_ <- as.character(seq_len(nrow(out$object)))
  out$object$color_ <- palr::image_pal(x$quad$value)
  out$triangle$object_ <- as.character(rep(out$object$object_, each = 2L))
  ## try expanding object to pairs of triangles
  #f <- factor(x$quad$value)

 # out$object <- tibble::tibble(object_ = silicate::sc_uid(nlevels(f)),
  #                             value = levels(f))
  #out$triangle$object_ <- out$object$object_[as.integer(out$triangle$object_)]
  #out$object_link_triangle$object_ <- rep(out$object$object_[f], each = 2)
#  out$
  out
}



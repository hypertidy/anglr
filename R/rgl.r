
#' @rdname anglr
#' @export
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @examples 
#' ## -----------------------------------------------
#' ## RGL TRIANGLES
#' library(rgl)
#' 
#' dod <- anglr(dodecahedron3d(col = "cyan"))
#' octo <- anglr(translate3d(octahedron3d(col = "blue"), 6, 0, 0))
#' plot(dod, col = viridis::viridis(5)[1], alpha = 0.3)
#' plot(octo, col = viridis::viridis(5)[5], alpha = 0.3)
#' bg3d("grey")
anglr.mesh3d <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
  stopifnot(x$primitivetype == "triangle")
  if ("ib" %in% names(x)) {
    warning("object has quad primitives as well as triangles,\n only the triangles will be carried through")
  }
  v <- setNames(as_tibble(t(x$vb[1:3, ])), c("x_", "y_", "z_"))
  v$vertex_ <- spbabel:::id_n(nrow(v))
  o <- tibble(object_ = spbabel:::id_n(1))
  tt <- tibble(triangle_ = spbabel:::id_n(ncol(x$it)), object_ = rep(o$object_[1L], ncol(x$it)))
  
  tXv <- tibble(vertex_ = v$vertex_[x$it], 
                triangle_ = tt$triangle_[rep(seq(ncol(x$it)), each = 3)])
  
  
  
  structure(list(o = o, t = tt, tXv = tXv, v = v), class = "trimesh")
}



anglr <- function(x, ...) {
  UseMethod("anglr")
}

#mesh.mesh3d
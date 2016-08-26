
#' @rdname tri_mesh
#' @export
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
tri_mesh.mesh3d <- function(x) {
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

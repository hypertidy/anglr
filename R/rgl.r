
#' @rdname tri_mesh
#' @export
#' @examples 
#' theta <- seq(0, 2*pi, len = 25)[-25]
#' theta <- c(theta, NA, theta, NA, theta, NA, theta, NA, theta)
#' r <- c(rep(1.5, 24), NA, rep(0.5, 24), NA, rep(0.5, 24), NA, rep(0.3, 24), NA, rep(0.1, 24))
#' dx <- c(rep(0, 24), NA, rep(0.6, 24), NA, rep(-0.6, 24), NA, rep(-0.6, 24), NA, rep(-0.6, 24))
#' x <- r*cos(theta) + dx
#' y <- r*sin(theta)
#' #plot(x, y, type = "n")
#' #polygon(x, y)
#' tr <- rangl:::th3d()
#' tr$vb <- t(cbind(x, y, 0, 1))
#' tr$it <- triangulate(x, y, plot = FALSE)
#' tre <- extrude3d(x, y, thickness = 1)
tri_mesh.mesh3d <- function(x) {
  stopifnot(x$primitivetype == "triangle")
  if ("ib" %in% names(x)) {
    warning("object has quad primitives as well as triangles,\n only the triangles will be carried through")
  }
  v <- setNames(as_tibble(t(x$vb[1:3, ])), c("x_", "y_", "z_"))
  v$vertex_ <- spbabel:::id_n(nrow(v))
  tt <- tibble(triangle_ = spbabel:::id_n(ncol(x$it)), object_ = "1")
  
  tXv <- tibble(vertex_ = v$vertex_[x$it], 
                triangle_ = tt$triangle_[rep(seq(ncol(x$it)), each = 3)])
  o <- tibble(object_ = "1")
  structure(list(o = o, t = tt, tXv = tXv, v = v), class = "trimesh")
}

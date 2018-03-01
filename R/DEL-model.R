#' DEL Delaunay model
#'
#' The Delaunary criterion forms the basis of this model, is its
#' definining characteristic without being compulsory. From Wikipedia: 

#' The "Delaunay triangulation (also known as a Delone triangulation) for a given
#' set P of discrete points in a plane is a triangulation DT(P) such that no
#' point in P is inside the circumcircle of any triangle in DT(P). Delaunay
#' triangulations maximize the minimum angle of all the angles of the triangles
#' in the triangulation; they tend to avoid sliver triangles. . . . The
#' Delaunay triangulation corresponds to the dual graph of the Voronoi
#' diagram of P" 
#' \url{https://en.wikipedia.org/wiki/Delaunay_triangulation}. 

#' The Delaunay model is a constrained triangulation with a variety 
#' of constraint and qualification types. The Delaunay model has the
#' odd but defining characterist of not being always consistent 
#' with the Delaunay criterion. Edge inclusion is 
#' non-negotiable, but other constraints include (limit, or avoid)
#' Steiner vertex insertion, a limit on the maximum area of a triangle, 
#' minimum triangle angle and strict adherence to the Delaunay criterion. 
#' 
#' The Delaunay model is the *mostly Delaunay* scheme used 
#' by the provable-quality meshers. 
#' @param x input model
#' @param ... passed to the underlying Triangle library
#' @param max area WIP and all the other args for Triangle
#' @return
#' @export
#'
#' @examples
#' plot3d.DEL(DEL(simpleworld))
#' rgl::rglwidget()
DEL <- function(x, ..., max_area = NULL) {
  UseMethod("DEL")
}
#' @export
DEL.SC <- function(x, ...) {
  v <- x$vertex
  a <- match(x$edge$.vertex0, v$vertex_)
  b <- match(x$edge$.vertex1, v$vertex_)
  p <- RTriangle::pslg(as.matrix(dplyr::select(v, .data$x_, .data$y_)), 
                       S = cbind(a, b))
  t <- RTriangle::triangulate(p, ...)
  ## need pfft to drop holes...
  meta <- tibble(proj = get_proj(x), ctime = Sys.time())
  
  structure(list(TRI = t$T, V = cbind(t$P, 0)), class = "TRI")
}

#' @export
DEL.default <- function(x, ...) {
  DEL(SC(x), ...)
}

#' @importFrom rgl plot3d
#' @export
plot3d.DEL <- function(x) {
  rgl::rgl.triangles(x$V[t(x$TRI), ])
}

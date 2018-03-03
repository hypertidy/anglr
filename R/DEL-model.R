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
## from pfft
edge_RTriangle <- function (x, ...) 
{
  ps <- RTriangle::pslg(P = as.matrix(x[["vertex"]][c("x_", 
                                                      "y_")]), S = matrix(match(silicate::sc_edge(x) %>% dplyr::select(.data$.vertex0, 
                                                                                                                       .data$.vertex1) %>% as.matrix() %>% t() %>% as.vector(), 
                                                                                x[["vertex"]][["vertex_"]]), ncol = 2, byrow = TRUE))
  RTriangle::triangulate(ps, ...)
}
## DEL for a PATH is a copy of pfft_polys that returns a DEL, TRI, sc
## TRI for a PATH returns a TRI, sc (just decido triangles)
DEL.PATH <- function(x, max_area = NULL,  ...) {
    dots <- list(...)
    dots[["a"]] <- max_area
    dots[["x"]] <- x
  
    ## TRIANGULATE with PATH-identity  
    RTri <- do.call(edge_RTriangle, dots)
    ## object/path_link_triangle (path_triangle_map)
    ptm <- pfft::path_triangle_map(x, RTri)
    
    ## unique triangles
    triangle <- tibble::tibble(triangle_ = silicate::sc_uid(nrow(RTri$T)))

    ## all triangle instances
    ptm[["triangle_"]] <- triangle[["triangle_"]][ptm[["triangle_idx"]]]
    ptm[["triangle_idx"]] <- NULL
    
    ## any triangle that occurs an even number of times in a path 
    ## per object is part of a hole
    ptm <- dplyr::inner_join(ptm, x[["path"]][c("path_", "object_")], "path_")
    object_link_triangle <- ptm %>% dplyr::group_by(.data$object_, .data$triangle_) %>% 
      dplyr::mutate(visible_ = !(n() %% 2 == 0)) %>%  ## see globalVariables declaration for "n"
      dplyr::ungroup()  
    vertex <- tibble::tibble(x_ = RTri$P[,1], y_ = RTri$P[,2], 
                             vertex_ = silicate::sc_uid(nrow(RTri$P)))

    triangle <- dplyr::mutate(triangle, .vertex0 = vertex$vertex_[RTri$T[,1]],
                              .vertex1 = vertex$vertex_[RTri$T[,2]],
                              .vertex2 = vertex$vertex_[RTri$T[,3]])
    
    tXv <- tibble::tibble(vertex_ = vertex[["vertex_"]][t(RTri$T)], 
                          triangle_ = rep(triangle[["triangle_"]], each = 3))
    
    meta <- tibble(proj = get_proj(x), ctime = Sys.time())
  
    structure(list(object = x$object, object_link_triangle = object_link_triangle, 
                   triangle = triangle, 
                   vertex = vertex, 
                   meta = meta), class = c("DEL", "TRI", "sc"))
    
  }


#' @importFrom rgl plot3d
#' @export
plot3d.DEL <- function(x) {
  nms <- intersect(c("x_", "y_", "z_"), names(x$vertex))
  if (length(nms) < 3) z <- 0 else z <- NULL
  V <- cbind(as.matrix(x$vertex[nms]), z)
  tXv <- dplyr::inner_join(x$triangle, x$object_link_triangle)
  TT <- rbind(match(tXv$.vertex0, x$vertex$vertex_), 
              match(tXv$.vertex1, x$vertex$vertex_),
                    match(tXv$.vertex2, x$vertex$vertex_))
  rgl::rgl.triangles(t(V[TT, ]))
}

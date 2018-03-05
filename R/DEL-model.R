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
#' @name DEL
#' @export
DEL.SC <- function(x, ..., max_area = NULL) {
  v <- x$vertex
  a <- match(x$edge$.vertex0, v$vertex_)
  b <- match(x$edge$.vertex1, v$vertex_)
 # for (i in seq_len(dim(x$object)[1L])) {
    p <- RTriangle::pslg(as.matrix(dplyr::select(v, .data$x_, .data$y_)), 
                       S = cbind(a, b))
    t <- RTriangle::triangulate(p, a = max_area, ...)
  
  ## need to identify segments that were input and are
  ## shared by two triangles, set to invisible
  
    vertex <- tibble::tibble(x_ = t$P[,1], 
                           y_ = t$P[,2], 
                           vertex_ = sc_uid(dim(t$P)[1L]))
    triangle <- tibble::tibble(.vertex0 = vertex$vertex_[t$T[,1L]],
                             .vertex1 = vertex$vertex_[t$T[,2L]], 
                             .vertex2 = vertex$vertex_[t$T[,3L]], 
                             triangle_ = sc_uid(dim(t$T)[1L])
                             )
  object_link_triangle <- tibble::tibble(triangle_ = triangle$triangle_, 
                                         object_ = x$object$object_[1])
    meta <- tibble(proj = get_proj(x), ctime = Sys.time())
  structure(list(object = x$object[1, ], 
                 object_link_triangle = object_link_triangle, 
                 triangle = triangle,
                 vertex = vertex, 
                 meta = meta), class = c("DEL", "TRI"))
  #structure(list(TRI = t$T, V = cbind(t$P, 0)), class = "TRI")
}
#' @name DEL
#' @export
DEL.default <- function(x, ..., max_area = NULL) {
  DEL(SC(x), ... , max_area = max_area)
}

## DEL for a PATH is a copy of pfft_polys that returns a DEL, TRI, sc
## TRI for a PATH returns a TRI, sc (just decido triangles)

#' @name DEL
#' @export
DEL.PATH <- function(x,  ..., max_area = NULL) {
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



## from pfft
edge_RTriangle <- function (x, ...) 
{
  ps <- RTriangle::pslg(P = as.matrix(x[["vertex"]][c("x_", 
                                                      "y_")]), S = matrix(match(silicate::sc_edge(x) %>% dplyr::select(.data$.vertex0, 
                                                                                                                       .data$.vertex1) %>% as.matrix() %>% t() %>% as.vector(), 
                                                                                x[["vertex"]][["vertex_"]]), ncol = 2, byrow = TRUE))
  RTriangle::triangulate(ps, ...)
}




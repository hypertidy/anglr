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
DEL.default <- function(x, ...) {
  DEL(SC(x), ...)
}

#' @name DEL
#' @export
DEL.SC <- function(x, max_area = NULL, ...)  {
 objs <- vector("list", nrow(x$object))
  for (i in seq_along(objs)) {
    x1 <- x
    x1$object <- x1$object[i, ]
    x1$object_link_edge <- x1$object_link_edge %>% dplyr::filter(object_ == x1$object$object_[1])
    x1$edge <- x$edge[x$edge$edge_ %in% x1$object_link_edge$edge_, ]
    ordered_verts <- t(apply(as.matrix(x1$edge[c(".vertex0", ".vertex1")]), 1, sort))
    x1$vertex <- x$vertex[x$vertex$vertex_ %in% c(ordered_verts), ]
    

    dots <- list(...)
    
    dots[["a"]] <- max_area
    dots[["x"]] <- x1
    ## TRIANGULATE
    RTri <- do.call(edge_RTriangle, dots)
    objs[[i]] <- RTri
  }


  ## unique triangles
 n_t <- unlist(lapply(objs, function(x) nrow(x$T)))
 nT <- sum(n_t)
  triangle <- tibble::tibble(triangle_ = silicate::sc_uid(nT))
  
  P <- do.call(rbind, lapply(objs, function(x) x$P))
  vertex <- tibble::tibble(x_ = P[,1], y_ = P[,2], 
                           vertex_ = silicate::sc_uid(nrow(P)))
  TRIS <- lapply(objs, function(x) x$T)
  count <- 0

  for (i in seq_along(TRIS)) {
    TRIStemp <- TRIS[[i]] + count
    count <- count + max(TRIS[[i]])
    TRIS[[i]] <- TRIStemp
  }
  ## need to identify segments that were input and are
  ## shared by two triangles, set to invisible
  
  TRIS <- do.call(rbind, TRIS)
  triangle <- dplyr::mutate(triangle, .vertex0 = vertex$vertex_[TRIS[,1]],
                            .vertex1 = vertex$vertex_[TRIS[,2]],
                            .vertex2 = vertex$vertex_[TRIS[,3]],  
                            object_ = rep(x$object$object_, n_t))
 object_link_triangle <- dplyr::distinct(triangle[c("object_", "triangle_")])
 triangle$visible <- TRUE
 triangle$object_ <- NULL
  tXv <- tibble::tibble(vertex_ = vertex[["vertex_"]][t(TRIS)], 
                        triangle_ = rep(triangle[["triangle_"]], each = 3))
  
  meta <- tibble(proj = get_proj(x), ctime = Sys.time())
  structure(list(object = x$object, 
                 object_link_triangle = object_link_triangle,
                 triangle = triangle, 
                 vertex = vertex, 
                 meta = meta), class = c("DEL", "TRI", "sc"))
  
}

 

## DEL for a PATH is a copy of pfft_polys that returns a DEL, TRI, sc
## TRI for a PATH returns a TRI, sc (just decido triangles)

#' @name DEL
#' @export
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

# DEL.PATH <- function(x,  ..., max_area = NULL) {
#     dots <- list(...)
#     dots[["a"]] <- max_area
#     dots[["x"]] <- x
#   
#     ## TRIANGULATE with PATH-identity  
#     RTri <- do.call(edge_RTriangle, dots)
#     ## object/path_link_triangle (path_triangle_map)
#     ptm <- pfft::path_triangle_map(x, RTri)
#     
#     ## unique triangles
#     triangle <- tibble::tibble(triangle_ = silicate::sc_uid(nrow(RTri$T)))
# 
#     ## all triangle instances
#     ptm[["triangle_"]] <- triangle[["triangle_"]][ptm[["triangle_idx"]]]
#     ptm[["triangle_idx"]] <- NULL
#     
#     ## any triangle that occurs an even number of times in a path 
#     ## per object is part of a hole
#     ptm <- dplyr::inner_join(ptm, x[["path"]][c("path_", "object_")], "path_")
#     object_link_triangle <- ptm %>% dplyr::group_by(.data$object_, .data$triangle_) %>% 
#       dplyr::mutate(visible_ = !(n() %% 2 == 0)) %>%  ## see globalVariables declaration for "n"
#       dplyr::ungroup()  
#     vertex <- tibble::tibble(x_ = RTri$P[,1], y_ = RTri$P[,2], 
#                              vertex_ = silicate::sc_uid(nrow(RTri$P)))
# 
#     triangle <- dplyr::mutate(triangle, .vertex0 = vertex$vertex_[RTri$T[,1]],
#                               .vertex1 = vertex$vertex_[RTri$T[,2]],
#                               .vertex2 = vertex$vertex_[RTri$T[,3]])
#     
#     tXv <- tibble::tibble(vertex_ = vertex[["vertex_"]][t(RTri$T)], 
#                           triangle_ = rep(triangle[["triangle_"]], each = 3))
#     
#     meta <- tibble(proj = get_proj(x), ctime = Sys.time())
#   
#     structure(list(object = x$object, object_link_triangle = object_link_triangle, 
#                    triangle = triangle, 
#                    vertex = vertex, 
#                    meta = meta), class = c("DEL", "TRI", "sc"))
#     
#   }


## from pfft
edge_RTriangle <- function (x, ...) 
{
  ps <- RTriangle::pslg(P = as.matrix(x[["vertex"]][c("x_", 
                                                      "y_")]), S = matrix(match(silicate::sc_edge(x) %>% dplyr::select(.data$.vertex0, 
                                                                                                                       .data$.vertex1) %>% as.matrix() %>% t() %>% as.vector(), 
                                                                                x[["vertex"]][["vertex_"]]), ncol = 2, byrow = TRUE))
  RTriangle::triangulate(ps, ...)
}


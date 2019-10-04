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
#' odd but defining characteristic of not being always consistent 
#' with the Delaunay criterion. Edge inclusion is 
#' non-negotiable, but other constraints include (limit, or avoid)
#' Steiner vertex insertion, a limit on the maximum area of a triangle, 
#' minimum triangle angle and strict adherence to the Delaunay criterion. 
#' 
#' The Delaunay model is the *mostly Delaunay* scheme used 
#' by the provable-quality meshers. 
#' @param x input model
#' @param ... passed to the underlying Triangle library
#' @param max_area the maximum area of a triangle
#' @return DEL model
#' @export
#'
#' @examples
#' plot3d(DEL(simpleworld))
#' rgl::rglwidget()
DEL <- function(x, ..., max_area = NULL) {
  UseMethod("DEL")
}
#' @name DEL
#' @importFrom silicate PATH
#' @importFrom rlang .data
#' @export
DEL.default <- function(x, ..., max_area = NULL) {
  p <- try(silicate::PATH(x), silent = TRUE)
  if (inherits(p, "try-error")) {
    stop("cannot convert 'x' to a PATH, try 'DEL(silicate::SC(x))' rather than 'DEL(x)'")
  }
  DEL(p, ..., max_area = max_area)
}
#' @name DEL
#' @importFrom silicate SC
#' @export
DEL.PATH0 <- function(x, ..., max_area = NULL) {
  DEL(PATH(x),  ..., max_area = max_area)
}

#' @name DEL
#' @export
DEL.TRI <- function(x, ..., max_area = NULL) {
  DEL(SC(x), ..., max_area = max_area)
}
#' @name DEL
#' @export 
DEL.TRI0 <- function(x, ..., max_area = NULL) {
  DEL(SC(x), ..., max_area = max_area)
}

#' @name DEL
#' @export
DEL.SC <- function(x, ..., max_area = NULL)  {
  ## find if any objects have < 3 edges
  edge_per_object_lt <- x$object["object_"] %>% 
    dplyr::inner_join(x$object_link_edge, "object_") %>% 
    dplyr::inner_join(x$object_link_edge, "object_") %>% 
    
    dplyr::group_by(.data$object_) %>% dplyr::tally(dplyr::n()) %>% dplyr::filter(n < 3)
  if (nrow(edge_per_object_lt) > 0) {
    message("dropping untriangulatable objects")
    ## need anti_join.sc
    x <- dplyr::filter(x, !.data$object_ %in% edge_per_object_lt$object_)
    drop <- x$vertex$vertex_ %in% c(x$edge$.vx0, x$edge$.vx1)
    if (any(drop)) x$vertex <- x$vertex[drop, ]
  }

  ## we need a pfft::edge_triangle_map
  ## https://github.com/hypertidy/silicate/issues/62#issuecomment-372898877
  objs <- vector("list", nrow(x$object))
  for (i in seq_along(objs)) {
    x1 <- x
    x1$object <- x1$object[i, ]
    x1$edge <- x1$object %>% dplyr::inner_join(x1$object_link_edge, "object_") %>% dplyr::inner_join(x1$edge, "edge_")
    ordered_verts <- t(apply(as.matrix(x1$edge[c(".vx0", ".vx1")]), 1, sort))
    x1$vertex <- x$vertex[x$vertex$vertex_ %in% c(ordered_verts), ]
    
    dots <- list()
    
    dots[["a"]] <- max_area
    dots[["x"]] <- x1
    RTri <- do.call(edge_RTriangle, dots)
    objs[[i]] <- RTri
  }
  
  
  ## unique triangles
  n_t <- unlist(lapply(objs, function(x) nrow(x$T)))
  bad <- n_t < 1
  if (all(bad)) stop("nothing triangulatable")
  objs <- objs[!bad]
  n_t <- n_t[!bad]
  nT <- sum(n_t)
  
  P <- do.call(rbind, lapply(objs, function(x) x$P))
  f <- as.integer(factor(paste(P[,1], P[,2], sep = "-")))
  P <- cbind(P, f)
  P_unique <- P[!duplicated(f), ]
  
  vertex <- tibble::tibble(x_ = P_unique[,1], y_ = P_unique[,2], 
                           vertex_ = silicate::sc_uid(nrow(P_unique)),
                           Pidx = P_unique[,3])
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
  
  v0 <- match(P[TRIS[,1],3], vertex$Pidx)
  v1 <- match(P[TRIS[,2],3], vertex$Pidx)
  v2 <- match(P[TRIS[,3],3], vertex$Pidx)
  
  
  triangle <- tibble::tibble(.vx0 = vertex$vertex_[v0],
                             .vx1 = vertex$vertex_[v1],
                             .vx2 = vertex$vertex_[v2],  
                             object_ = rep(x$object$object_[!bad], n_t))
  
  
  vertex$Pidx <- NULL
  meta <- tibble(proj = get_proj(x), ctime = Sys.time())
  structure(list(object = x$object[!bad, ],  ## this is tenuous, using !bad throughout 
                 
                 triangle = triangle, 
                 vertex = vertex, 
                 meta = meta), class = c("DEL", "TRI", "sc"))
  
}


#' @name DEL
#' @export
DEL.SC0 <- function(x, ..., max_area = NULL)  {
  ## FIXME:build off SC0 not SC
 DEL(SC(x), max_area = max_area, ...) 

}
## DEL for a PATH is a copy of pfft_polys that returns a DEL, TRI, sc
## TRI for a PATH returns a TRI, sc (just decido triangles)

#' @name DEL
#' @export
#' @importFrom tibble tibble
DEL.PATH <- function(x, ..., max_area = NULL) {

  dots <- list(...)
  dots[["a"]] <- max_area
  dots[["x"]] <- x
  
  ## TRIANGULATE with PATH-identity  
  RTri <- do.call(edge_RTriangle, dots)
  ## object/path_link_triangle (path_triangle_map)
  ptm <- pfft::path_triangle_map(x, RTri)
  #ptm$path_ <- as.integer(factor(ptm$path_))
  ## unique triangles
  triangle <- tibble::tibble(triangle_ = silicate::sc_uid(nrow(RTri$T)))
  
  ## all triangle instances
  ptm[["triangle_"]] <- triangle[["triangle_"]][ptm[["triangle_idx"]]]
  ptm[["triangle_idx"]] <- NULL
  
  ## any triangle that occurs an even number of times in a path 
  ## per object is part of a hole
  path <- x$path
  path$path_ <- as.character(path$path_)
  ptm <- dplyr::inner_join(ptm, path[c("path_", "object_")], "path_")
  object_link_triangle <- ptm %>% dplyr::group_by(.data$object_, .data$triangle_) %>% 
    dplyr::mutate(visible_ = !(dplyr::n() %% 2 == 0)) %>%  ## see globalVariables declaration for "n"
    dplyr::ungroup()  
  vertex <- tibble::tibble(x_ = RTri$P[,1], y_ = RTri$P[,2], 
                           vertex_ = silicate::sc_uid(nrow(RTri$P)))
  
  triangle <- dplyr::mutate(triangle, .vx0 = vertex$vertex_[RTri$T[,1]],
                            .vx1 = vertex$vertex_[RTri$T[,2]],
                            .vx2 = vertex$vertex_[RTri$T[,3]])
  triangle <- object_link_triangle[c("triangle_", "object_", "visible_")] %>% dplyr::inner_join(triangle, "triangle_")
  triangle$triangle_ <- NULL
#  tXv <- tibble::tibble(vertex_ = vertex[["vertex_"]][t(RTri$T)], 
#                        triangle_ = rep(triangle[["triangle_"]], each = 3))
  
  meta <- tibble(proj = get_proj(x), ctime = Sys.time())
  
  structure(list(object = x$object, #object_link_triangle = object_link_triangle, 
                 triangle = triangle, 
                 vertex = vertex, 
                 meta = meta), class = c("DEL", "TRI", "sc"))
  
}


## from pfft
edge_RTriangle <- function (x, ...) 
{
  ps <- RTriangle::pslg(P = as.matrix(x[["vertex"]][c("x_", 
                                                      "y_")]), S = matrix(match(silicate::sc_edge(x) %>% dplyr::select(.data$.vx0, 
                                                                                                                       .data$.vx1) %>% as.matrix() %>% t() %>% as.vector(), 
                                                                                x[["vertex"]][["vertex_"]]), ncol = 2, byrow = TRUE))
  RTriangle::triangulate(ps, ...)
}


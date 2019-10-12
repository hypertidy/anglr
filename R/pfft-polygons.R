## FROM https://github.com/hypertidy/pfft/blob/master/R/pfft.R
edge_RTriangle <- function(x, ...) {
  ps <- RTriangle::pslg(P = as.matrix(x[["vertex"]][c("x_", "y_")]),
                        S = matrix(match(silicate::sc_edge(x) %>%
                                           dplyr::select(.data$.vx0, .data$.vx1) %>%
                                           as.matrix() %>% t() %>% as.vector(), x[["vertex"]][["vertex_"]]), ncol = 2, byrow = TRUE))
  RTriangle::triangulate(ps, ...)
}

path_triangle_map <- function(x, RTri) {
  centroids <- matrix(unlist(lapply(split(RTri[["P"]][t(RTri[["T"]]), ], rep(seq(nrow(RTri$T)), each = 3)), .colMeans, 3, 2)),
                      ncol = 2, byrow = TRUE)
  ex <- extents(x)
  gm <- x[["path"]]
  ## map of which points to look up
  
  pipmap <- split(ex, ex$path_)[unique(ex$path_)] %>%
    purrr::map(~ (centroids[,1] >= .x[["xmn"]] &
                    centroids[,1] <= .x[["xmx"]] &
                    centroids[, 2] >= .x[["ymn"]] &
                    centroids[,2] <= .x[["ymx"]]))
  pipmap <- pipmap[gm$path_]
  len <- purrr::map_int(pipmap, sum)
  ## now the lookup
  lc <- split(silicate::sc_coord(x), rep(seq_len(nrow(gm)), gm$ncoords_))
  ## this is the result
  pip <- pipmap
  for (i in seq_along(pipmap)) {
    if (len[i] > 0) {
      ## replace this with a generic native function
      #      pip[[i]][pipmap[[i]]] <-
      ##sp::point.in.polygon(centroids[pipmap[[i]], 1], centroids[pipmap[[i]],2], lc[[i]][["x_"]], lc[[i]][["y_"]]) > 0
      pip[[i]][pipmap[[i]]] <-  abs(polyclip::pointinpolygon(list(x = centroids[pipmap[[i]], 1], y = centroids[pipmap[[i]],2]),
                                                             list(x = lc[[i]][["x_"]], y = lc[[i]][["y_"]]))) > 0L
    } else {
      pip[[i]][] <- FALSE
    }
  }
  ix <- lapply(pip, which)
  tibble::tibble(path_ = rep(names(ix), lengths(ix)),
                 triangle_idx = unlist(ix))
}


#' @noRd
#' @param x PATH
pfft_polys <- function(x, max_area = NULL,  ...) {
  dots <- list(...)
  dots[["a"]] <- max_area
  dots[["x"]] <- x

  RTri <- do.call(edge_RTriangle, dots)
  ptm <- path_triangle_map(x, RTri)
  #vertex <- x$v  ## for fun aRt
  vertex <- tibble::tibble(x_ = RTri$P[,1], y_ = RTri$P[,2], vertex_ = silicate::sc_uid(nrow(RTri$P)))
  ## unique triangles
  triangle <- tibble::tibble(triangle_ = silicate::sc_uid(nrow(RTri$T)))
  ## all triangle instances
  ptm[["triangle_"]] <- triangle[["triangle_"]][ptm[["triangle_idx"]]]
  ptm[["triangle_idx"]] <- NULL
  ## any triangle that occurs an even number of times in a path per object is part of a hole

  ptm <- dplyr::inner_join(ptm, x[["path"]][c("path_", "object_")], "path_")
  
 
  ptm <- ptm %>% dplyr::group_by(.data$object_, .data$triangle_) %>% 
    dplyr::mutate(n = dplyr::n()) %>%  ## see globalVariables declaration for "n"
    dplyr::ungroup()  #%>% 
  
  tt <- dplyr::select(ptm, .data$object_, .data$triangle_) %>% 
    dplyr::anti_join(ptm %>% dplyr::filter(.data$n %% 2 == 0) %>% 
                       dplyr::select(.data$object_, .data$triangle_), c("object_", "triangle_"))
  tXv <- tibble::tibble(vertex_ = vertex[["vertex_"]][t(RTri$T)], 
                        triangle_ = rep(triangle[["triangle_"]], each = 3))
  
  
  outlist <- list(o = x$o, t = tt, tXv = tXv, v = vertex, 
                  meta = x$meta)
  class(outlist) <- "trimesh"
  outlist
}

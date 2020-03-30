## FROM https://github.com/hypertidy/pfft/blob/master/R/pfft.R


extents <- function(x) {
  UseMethod("extents")
}
extents.default <- function(x) {
  extents(silicate::PATH(x))
}
#' @importFrom rlang .data
#' @importFrom dplyr %>%
extents.SC <- function(x) {
  x0 <- x[["edge"]] %>%
    dplyr::inner_join(x[["vertex"]], c(".vx0" = "vertex_"))  %>%
    dplyr::transmute(x0 = .data$x_, y0 = .data$y_)
  x1 <- x[["edge"]] %>%
    dplyr::inner_join(x[["vertex"]], c(".vx1" = "vertex_"))  %>%
    dplyr::transmute(x1 = .data$x_, y1 = .data$y_, edge_ = .data$edge_)

  edges <- dplyr::bind_cols(x0, x1)
  tibble::tibble(xmn = pmin(edges$x0, edges$x1), xmx = pmax(edges$x0, edges$x1),
                 ymn = pmin(edges$y0, edges$y1), ymx = pmax(edges$y0, edges$y1))
}



#' @importFrom rlang .data
#' @importFrom dplyr %>%
extents.PATH <- function(x) {
  x[["path"]] %>% dplyr::select(.data$path_) %>%
    dplyr::inner_join(x[["path_link_vertex"]], "path_") %>%
    dplyr::inner_join(x[["vertex"]], "vertex_") %>%
    dplyr::group_by(.data$path_) %>%
    dplyr::summarize(xmn = min(.data$x_), xmx = max(.data$x_),
                     ymn = min(.data$y_), ymx = max(.data$y_))
}


edge_RTriangle <- function(x, ...) {
  ps <- RTriangle::pslg(P = as.matrix(x[["vertex"]][c("x_", "y_")]),
                        S = matrix(match(silicate::sc_edge(x) %>%
                                           dplyr::select(.data$.vx0, .data$.vx1) %>%
                                           as.matrix() %>% t() %>% as.vector(), x[["vertex"]][["vertex_"]]), ncol = 2, byrow = TRUE))
  RTriangle::triangulate(ps, ...)
}

edge_RTriangle0 <- function(x, ...) {
  ps <- RTriangle::pslg(P = as.matrix(x[["vertex"]][c("x_", "y_")]),
                        S = as.matrix(silicate::sc_edge(x)[c(".vx0", ".vx1")]))
  RTriangle::triangulate(ps, ...)
}

path_triangle_map <- function(x, RTri) {
  centroids <- matrix(unlist(lapply(split(RTri[["P"]][t(RTri[["T"]]), ], rep(seq(nrow(RTri$T)), each = 3)), .colMeans, 3, 2)),
                      ncol = 2, byrow = TRUE)
  ex <- extents(x)
  gm <- silicate::sc_path(x) ## x[["path"]]
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

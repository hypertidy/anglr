
#' DEL0
#'
#' Structural form of Delaunay constrained triangulation
#'
#' More compact form of [DEL()] model.
#'
#' @param x object of class [PATH0] or understood by [PATH0()]
#' @param ... ignored
#' @inheritParams DEL
#'
#' @return DEL0 class
#' @export
#'
#' @examples
#' a <- DEL0(cad_tas)
#' plot(a)
DEL0 <- function(x, ..., max_area = NULL) {
  UseMethod("DEL0")
}
#' @name DEL0
#' @export
DEL0.DEL <- function(x, ..., max_area = NULL) {
  ## must redo this stuff to use DEL0 as the basis
  if (!is.null(max_area)) {
    warning("'max_area' ignored, cannot currently re-mesh a DEL or DEL0")
  }
  object <- silicate::sc_object(x)
  triangle <- x$triangle
  if ("visible_" %in% names(triangle)) {
    ## because DEL(SC()) doesn't have visible_
    triangle <- dplyr::filter(triangle, .data$visible_)
  }
  topol <- matrix(match(as.matrix(triangle[c(".vx0", ".vx1", ".vx2")]),
                        x$vertex$vertex_), ncol = 3L)
  colnames(topol) <- c(".vx0", ".vx1", ".vx2")
  object$topology_ <- split(tibble::as_tibble(topol), triangle$object_)[unique(triangle$object_)]
  object$object_ <- NULL
  meta <- x$meta
  row <- x$meta[1, ]
  row$ctime <- Sys.time()
  meta <- rbind(row, meta)
  structure(list(object = object,
                 vertex = x$vertex,
                 meta = meta), class = c("DEL0", "TRI0", "sc"))

}
#' @name DEL0
#' @export
DEL0.default <- function(x, ..., max_area = NULL) {
  DEL0(silicate::PATH0(x), ..., max_area)
}

#' @name DEL0
#' @export
DEL0.SC <- function(x, ..., max_area = NULL) {
  DEL0(DEL(x, max_area = max_area), ...)
}
#' @name DEL0
#' @export
DEL0.SC0 <- function(x, ..., max_area = NULL) {
  DEL0(DEL(x, max_area = max_area), ...)
}

#' @name DEL0
#' @export
DEL0.TRI <- function(x, ..., max_area = NULL) {
  DEL0(DEL(x, max_area = max_area), ...)
}
#' @name DEL0
#' @export
DEL0.TRI0 <- function(x, ..., max_area = NULL) {
  DEL0(TRI(x, max_area = max_area), ...)
}
#' @name DEL0
#' @export
DEL0.ARC <- function(x, ..., max_area = NULL) {
  DEL0(DEL(x, max_area = max_area), ...)
}
#' @name DEL0
#' @export
DEL0.PATH <- function(x, ..., max_area = NULL) {
  DEL(PATH0(x), max_area = NULL)
}
#' @name DEL0
#' @export
DEL0.PATH0 <- function(x, ..., max_area = NULL) {
  .check_area(x$vertex$x_, x$vertex$y_, max_area)
  dots <- list(...)
  dots[["a"]] <- max_area
  dots[["x"]] <- x

  ## TRIANGULATE with PATH-identity
  RTri <- do.call(edge_RTriangle0, dots)
 # x## object/path_link_triangle (path_triangle_map)
  ptm <- path_triangle_map(x, RTri)
  omap <- dplyr::bind_rows(x$object$path_)%>% dplyr::distinct(.data$object_, .data$path_)
  ptm$object_ <- omap$object_[match(ptm$path_, omap$path_)]
  ptm <- ptm %>% dplyr::group_by(.data$object_, .data$triangle_idx) %>%
    dplyr::filter(!(dplyr::n() %% 2 == 0)) %>% dplyr::ungroup()
  path <-  silicate::sc_path(x)
  vertex <- tibble::tibble(x_ = RTri$P[,1], y_ = RTri$P[,2])

  tridf <- setNames(as.data.frame(RTri$T), c(".vx0", ".vx1", ".vx2"))[ptm$triangle_idx, ]
  tridf$path_ <- as.integer(ptm$path_)
  tridf$object_ <- as.integer(ptm$object_)
 topology <- split(tridf, tridf$object_)
  meta <- tibble(proj = get_proj(x), ctime = Sys.time())
  object <- x$object
  object$topology_ <- topology
  structure(list(object = object,
                 vertex = vertex,
                 meta = meta), class = c("DEL0", "TRI0", "sc"))

}

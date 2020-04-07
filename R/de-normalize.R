## de-normalize

# For vector, we need to
#
# de-normalize the vertices (join vertex to thing_link_vertex) PATH. ARC use
# inner_join, primitives tables require .vx0, .vx1, .xv2 handling link object to
# path to link table, or object to link table and transfer z renormalize in x,
# y, z (unjoin)

## sequence/path-based works for ARC or PATH
#' @importFrom silicate sc_uid
denorm_SEQ_addZ <- function(x, z, ..., .id = "z_") {
  group <- if(inherits(x, "PATH")) "path" else "object_link_arc"

  coords <- vertex_link(x) %>% dplyr::inner_join(x$vertex, "vertex_") %>%
    dplyr::inner_join(x[[group]])
  coords[[.id]] <- z[match(coords$object_, x$object$object_)]
  coords[["vertex"]] <- NULL
  data <- unjoin::unjoin(coords, .data$x_, .data$y_, .id, key_col = "vertex")
  x$vertex <- data$vertex
  x$vertex$vertex_ <- sc_uid(nrow(x$vertex))
  data$data$vertex_ <- x$vertex$vertex_[match(data$data$vertex, x$vertex$vertex)]
  x$vertex$vertex <- NULL
  data$data$vertex <- NULL
  vertex_link(x) <- data$data
  x
}

#'  PRIM works for SC, TRI
#'
#' @noRd
denorm_PRIM_addZ <- function(x, z, ..., .id = "z_") {

  if (inherits(x, "SC")) {
    ## instances of primitives
    priminst <- x$edge %>% inner_join(x$object_link_edge, "edge_")
    ## note that silicate/SC doesn't have a labelled edge, only object_ and .vx0 ,.vx1
    verts <- c(as.matrix(priminst[c(".vx0", ".vx1")]))  ## avoid gather()
    priminst$.vx0 <- priminst$.vx1 <- priminst$path_ <- priminst$native_ <- NULL
    prim_long <- tibble::tibble(vertex = verts)
    prim_long <- dplyr::bind_cols(prim_long, tibble::as_tibble(priminst[rep(seq_len(nrow(priminst)), 2L), ]))
    prim_long$edge_vertex <- rep(c(".vx0", ".vx1"), each = nrow(priminst))

    z <- rep(z, nrow(silicate::sc_object(x)))
    
    prim_long[[.id]] <- z[match(prim_long$object_, x$object$object_)]

    
    prim_long <- prim_long %>% inner_join(x$vertex, c("vertex" = "vertex_"))
    prim_long$vertex <- NULL
    prim_long$vertex_ <- silicate::sc_uid(nrow(prim_long))

vx <- split(prim_long, prim_long[["edge_vertex"]])
#vx[[1]]$.vx0 <- vx[[1]][["vertex_"]]
#vx[[2]]$.vx1 <- vx[[2]][["vertex_"]]
i <- 1; j <- 2
if (vx[[1]]$edge_vertex[1] == ".vx1") {
  j <- 1; i <- 2
}
## avoid spread()
x$edge <- tibble(.vx0 = vx[[i]][["vertex_"]],
                 .vx1 = vx[[j]][["vertex_"]],
                 edge_ = vx[[1]][["edge_"]])

    # x$edge <- prim_long[c("edge_vertex", "vertex_", "edge_", "object_")] %>%
    #   tidyr::spread(.data$edge_vertex, .data$vertex_)
    #
    #x$edge$object_ <- NULL

    }
  if (inherits(x, "TRI")) {

    priminst <- x$triangle
    priminst[["triangle_"]] <- silicate::sc_uid(priminst)  ## FIXME: temporary triangle_ id not needed
    #prim_long <- priminst %>% tidyr::gather("tri_vertex", "vertex", -.data$object_, -.data$triangle_)

    verts <- c(as.matrix(priminst[c(".vx0", ".vx1", ".vx2")]))  ## avoid gather()
    priminst$.vx2 <- priminst$.vx0 <- priminst$.vx1 <- priminst$path_ <- priminst$native_ <- NULL
    prim_long <- tibble::tibble(vertex = verts)
    prim_long2 <- tibble::as_tibble(priminst[rep(seq_len(nrow(priminst)), 3L), ])
    prim_long <- dplyr::bind_cols(prim_long, prim_long2)
    prim_long$tri_vertex <- rep(c(".vx0", ".vx1", ".vx2"), each = nrow(priminst))

    z <- rep(z, nrow(silicate::sc_object(x)))
    prim_long[[.id]] <- z[match(prim_long$object_, x$object$object_)]
  
    prim_long <- prim_long %>% inner_join(x$vertex, c("vertex" = "vertex_"))
    prim_long$vertex <- NULL
    prim_long$vertex_ <- silicate::sc_uid(nrow(prim_long))
    # prim_wide <-  prim_long[c("tri_vertex", "vertex_", "triangle_", "object_")] %>%
    #   tidyr::spread("tri_vertex", "vertex_")
    ## avoid spread()
    vx <- split(prim_long, prim_long[["tri_vertex"]])

    prim_wide <- tibble(.vx0 = vx[[1]][["vertex_"]],
                     .vx1 = vx[[2]][["vertex_"]],
                     .vx2 = vx[[3]][["vertex_"]],
                     triangle_ = vx[[1]][["triangle_"]],
                     object_ = vx[[1]][["object_"]])

    x$triangle <- dplyr::distinct(prim_wide, .data$object_, .data$.vx0, .data$.vx1, .data$.vx2)

    x$triangle$triangle_ <- NULL ## FIXME: temporary triangle_ id not needed

  }
  if ("z_" %in% names(prim_long)) {
    x$vertex <- dplyr::distinct(prim_long, .data$x_, .data$y_, .data$z_, .data$vertex_)
  } else {
    x$vertex <- dplyr::distinct(prim_long, .data$x_, .data$y_, .data$vertex_)
  }
  x
}

## return name of think_link_vertex table
vertex_link <- function(x) {
  UseMethod("vertex_link")
}
# vertex_link.SC <- function(x) {
#   x[["edge_link_vertex"]]
# }
# vertex_link.TRI <- function(x) {
#   x[["triangle_link_vertex"]]
# }
vertex_link.ARC <- function(x) {
  x[["arc_link_vertex"]]
}
vertex_link.PATH <- function(x) {
  x[["path_link_vertex"]]
}
## return name of thing_link_vertex table

'vertex_link<-' <- function(x, value) {
  UseMethod("vertex_link<-")
}
'vertex_link<-.SC' <- function(x, value) {
  x[["edge_link_vertex"]] <- value
  x
}
'vertex_link<-.TRI' <- function(x, value) {
  x[["triangle_link_vertex"]] <- value
  x
}
'vertex_link<-.ARC' <- function(x, value) {
  x[["arc_link_vertex"]] <- value
  x
}
'vertex_link<-.PATH' <- function(x, value) {
  x[["path_link_vertex"]] <- value
  x
}

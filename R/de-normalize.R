## de-normalize

# For vector, we need to
#
# de-normalize the vertices (join vertex to thing_link_vertex) PATH. ARC use
# inner_join, primitives tables require .vx0, .vx1, .xv2 handling link object to
# path to link table, or object to link table and transfer z renormalize in x,
# y, z (unjoin)

## sequence/path-based works for ARC or PATH
denorm_SEQ_addZ <- function(x, z, ..., .id = "z_") {
  coords <- vertex_link(x) %>% dplyr::inner_join(x$vertex, "vertex_")
  coords[[.id]] <- z
  coords[["vertex"]] <- NULL
  data <- unjoin::unjoin(coords, x_, y_, .id, key_col = "vertex")
  x$vertex <- data$vertex
  x$vertex$vertex_ <- sc_uid(nrow(x$vertex))
  data$data$vertex_ <- x$vertex$vertex_[match(data$data$vertex, x$vertex$vertex)]
  x$vertex$vertex <- NULL
  data$data$vertex <- NULL
  vertex_link(x) <- data$data
  x
}

## PRIM works for SC, TRI
denorm_PRIM_addZ <- function(x, z, ..., .id = "z_") {
  link  <- vertex_link(x)  
}

## return name of think_link_vertex table
vertex_link <- function(x) {
 UseMethod("vertex_link") 
}
vertex_link.SC <- function(x) {
  x[["edge_link_vertex"]]
} 
vertex_link.TRI <- function(x) {
  x[["triangle_link_vertex"]]
} 
vertex_link.ARC <- function(x) {
  x[["arc_link_vertex"]]
} 
vertex_link.PATH <- function(x) {
  x[["path_link_vertex"]]
} 
## return name of think_link_vertex table
vertex_link<- function(x, value,) {
  UseMethod("vertex_link") 
}
vertex_link<-.SC <- function(x, value,) {
  x[["edge_link_vertex"]] <- value
  x
} 
vertex_link<-.TRI <- function(x, value,) {
  x[["triangle_link_vertex"]] <- value
  x
} 
vertex_link<-.ARC <- function(x, value,) {
  x[["arc_link_vertex"]] <- value
  x
} 
vertex_link<-.PATH <- function(x, value,) {
  x[["path_link_vertex"]] <- value
  x
} 

## de-normalize

# For vector, we need to
#
# de-normalize the vertices (join vertex to thing_link_vertex) PATH. ARC use
# inner_join, primitives tables require .vx0, .vx1, .xv2 handling link object to
# path to link table, or object to link table and transfer z renormalize in x,
# y, z (unjoin)

## sequence/path-based works for ARC or PATH
denorm_SEQ_addZ <- function(x, z, ..., .id = "z_") {
  group <- if(inherits(x, "PATH")) "path" else "object_link_arc"
  
  coords <- vertex_link(x) %>% dplyr::inner_join(x$vertex, "vertex_") %>% 
    dplyr::inner_join(x[[group]])
  coords[[.id]] <- z[match(coords$object_, x$object$object_)]
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
#' @noRD
#' @examples
##' fifty <- fiftystater::fifty_states %>%
##' dplyr::transmute(x_ = long, y_ = lat, branch_ = group, object_ = id, order_ = order, island_ = !hole) %>%
##'   spbabel::sp() %>% sf::st_as_sf()
##' #plot(PATH(fifty))
##' fifty$height <- rnorm(nrow(fifty))
##' x <- denorm_PRIM_addZ(silicate::SC(fifty), z = fifty$height)
##' 
##' plot3d(x)
##' rgl::rglwidget()
denorm_PRIM_addZ <- function(x, z, ..., .id = "z_") {

  if (inherits(x, "SC")) {
    ## instances of primitives
  priminst <- x$edge %>% inner_join(x$object_link_edge)
  priminst[["edge_"]] <- silicate::sc_uid(priminst)
  prim_long <- priminst %>% 
    tidyr::gather(edge_vertex, vertex, -object_, -edge_)
  prim_long[[.id]] <- z[match(prim_long$object_, x$object$object_)]
  prim_long <- prim_long %>% inner_join(x$vertex, c("vertex" = "vertex_"))
  prim_long$vertex <- NULL
  prim_long$vertex_ <- silicate::sc_uid(nrow(prim_long))
  x$edge <- prim_long[c("edge_vertex", "vertex_", "edge_")] %>% tidyr::spread(edge_vertex, vertex_)
  x$object_link_edge <- dplyr::distinct(prim_long, object_, edge_)
  x$vertex <- dplyr::distinct(prim_long, x_, y_, z_, vertex_)
  }
  if (inherits(x, "TRI")) {
    priminst <- x$triangle %>% inner_join(x$object)
    priminst[["triangle_"]] <- silicate::sc_uid(priminst)
    prim_long <- priminst %>% tidyr::gather(tri_vertex, vertex, -object_, -triangle_)
    prim_long[[.id]] <- z[match(prim_long$object_, x$object$object_)]
    
    prim_long <- prim_long %>% inner_join(x$vertex, c("vertex" = "vertex_"))
    prim_long$vertex <- NULL
    prim_long$vertex_ <- silicate::sc_uid(nrow(prim_long))
    x$triangle <- prim_long[c("tri_vertex", "vertex_", "triangle_", "object_")] %>% 
      tidyr::spread(tri_vertex, vertex_)
    x$vertex <- dplyr::distinct(prim_long, x_, y_, z_, vertex_)
    
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

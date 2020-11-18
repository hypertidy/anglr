#' Planar Straight Line Graph
#'
#' Create a 'pslg' which is a mesh of segments used in the RTriangle package.
#'
#' The pslg is constructed from from unique vertices x, y and their segments. In the context of
#' triangulating the functions [DEL0()] and [DEL()] do more to ensure that input
#' polygons have their holes culled out (or classified by invisible triangles.)
#'
#'
#' @param x data model (understood by [SC0()])
#'
#' @return object of class [RTriangle::pslg] from the RTriangle package
#' @export
#'
#' @examples
#' data("minimal_mesh", package = "silicate")
#' as_pslg(minimal_mesh)
as_pslg <- function(x) {
  UseMethod("as_pslg")
}
#' @name as_pslg
#' @export
as_pslg.default <- function(x) {
  x <- silicate::SC0(x)
  v <- silicate::sc_vertex(x)  ## we expecting only x_, y_
  segs <- do.call(rbind, silicate::sc_object(x)$topology_)
  RTriangle::pslg(P = as.matrix(v[c("x_", "y_")]), S = as.matrix(segs[c(".vx0", ".vx1")]))
}

#' #' @name as_pslg
#' #' @export
#' as_pslg.default <- function(x) {
#'   if (inherits(x, "grouped_df")) {
#'     ## use the groupings as linestring_id, polygon_id, sf_id?
#'   }
#'   ## try if sfheaders knows the columns
#'   if ("multipolygon_id" %in% names(x)) {
#'    sfx <-   sfheaders::sf_multipolygon(x)
#'   } else {
#'     if ("polygon_id" %in% names(x)) {
#'       sfx <-   sfheaders::sf_polygon(x)
#'    }
#'
#'   }
#'
#' ## last resort
#'   sfx <- sfheaders::sf_polygon(x)
#'   as.pslg(sfx)
#' }

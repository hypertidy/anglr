#' Tidy tables for topological spatial data structures.
#'
#' The 'rangl' package helps transcend the following general limitations:  
#' \itemize{
#'  \item coordinates beyond X and Y, or longitude and latitude
#'  \item storing attributes on vertices, primitives, branches (parts), or objects
#'  \item topology and geometry are properly separated
#'  \item spatial data can be properly represented as a graph of spatial primitives
#'  \item polygons as true surfaces, not just glorified lines with a path-filling rule
#'  \item TBD higher dimensional primitives are possible
#'  \item TBD n-dimensional rasters with curvilinear coordinates, and the discrete-continuous distinction 
#' }
#'
#' @name rangl-package
#' @docType package
#' @section I. Creation:
#' \tabular{ll}{
#'  \code{\link{rangl}} \tab create a rangl table set from various input types  \cr
#'  }
#'
#' @section II. Plotting:
#' \tabular{ll}{
#'  \code{\link{globe}} \tab convert X,Y planar or angular to 3D on the surface of a globe, based on the data in longitude-latitude form \cr
#'  \code{\link{plot.trimesh}} \tab plot 2D topology in 3D geometry space \cr
#'  \code{\link{plot.linemesh}} \tab plot 1D topology in 3D geometry space \cr
#'  \code{\link{plot.pointmesh}} \tab plot 0D topology in 3D geometry space \cr
#' }
#'
NULL


rangl.map_table <- function(x, ...) {
  
}

plot.map_table <- function(x, ...) {
  x <- x$o %>% inner_join(x$b) %>% inner_join(x$bXv) %>% inner_join(x$v)
  ggplot2::ggplot(x) + 
    aes(x = x_, y = y_, group = branch_, fill = object_) + 
    ggpolypath::geom_polypath() + guides(colour = FALSE)
}
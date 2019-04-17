#' Tidy tables for topological spatial data structures.
#'
#' The 'anglr' package show-cases getting past limitations in spatial data by
#' extending models in the `silicate`` package:  
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
#' @name anglr-package
#' @docType package
#' @section I. Creation:
#' \tabular{ll}{
#'   \code{\link{DEL}} \tab create a mostly-Delaunay shape-preserving constrained triangulation  \cr
#'  \code{\link{QUAD}} \tab create a (raster-like) quad mesh  \cr
#'  }
#'
#' @section II. Plotting:
#' \tabular{ll}{
#'  \code{\link{globe}} \tab convert X,Y planar or angular to 3D on the surface of a globe, based on the data in longitude-latitude form \cr
#'  \code{\link{plot3d.SC}} \tab plot 1D topology in 3D geometry space \cr
#'  \code{\link{plot3d.TRI}} \tab plot 2D topology in 3D geometry space (DEL or TRI) \cr
#'  \code{\link{plot3d.QUAD}} \tab plot quad-cell topology in 3D geometry space \cr
#' }
#'
NULL


#' sf data frame zoo. 
#' 
#' Each kind of geometry in an sf data frame, in a list. 
#' @name sf_data_zoo
#' @docType data 
NULL

#' Cadastre and Contour
#' 
#' Sympatric cadastre layer and line contour layer. 
#' @name cad_tas
#' @aliases cont_tas
#' @docType data 
NULL

#' simple world 
#' 
#' A simple polygon map of world sovereign countries, a modified copy of
#' the rnaturalearth counties110 (see data-raw/simpleworld.R for details). 
#' @name simpleworld
#' @docType data 
#' @examples
#' DEL(simpleworld[1:10, ])
NULL


#' simple world elevation raster
#' 
#' A simple raster map of world topography, elevation relative to sea level in metres. Source
#' data is Gebco 2014, converted to a much reduced 1 degree resolution global map.  
#' @name gebco1
#' @docType data 
#' @examples
#' data("gebco1", package = "anglr")
#' library(silicate)
#' laea <- "+proj=laea +lon_0=147 +lat_0=-42"
#' longlat <- "+init=epsg:4326"
#' x <- SC(simpleworld) %>% copy_down(gebco1 + 500)
#' plot3d(x); rgl::aspect3d(1, 1, 0.07);rgl::rglwidget()
#' 
#' ## WARNING: SC doesn't have meta yet ...
#' ## this is a lossless transformation of raster and vector to a projected
#' ## 3D scene
#' x$vertex[c("x_", "y_")] <- proj4::ptransform(as.matrix(x$vertex[c("x_", "y_")]) * pi/180, 
#'  longlat, laea)
#' if (requireNamespace("quadmesh", quietly = TRUE)) {
#'  qm <- quadmesh::quadmesh(raster::aggregate(gebco1, fact = 4)); 
#'  qm$vb[1:2, ] <- t(proj4::ptransform(t(qm$vb[1:2, ]) * pi/180, longlat, laea))[1:2, ]
#'  plot3d(x); shade3d(qm, col = "white");  rgl::aspect3d(1, 1, 0.4); rgl::rglwidget()
#' }
NULL
#' Copy down values to vertices
#'
#' Copy down provides ways to transfer object level data values to
#' vertex level. 
#' 
#' Various methods are used depending on the second argument `z`. 
#' 
#' If z is a raster (`BasicRaster`) a numeric value for each vertex is found by bilinear
#' interpolation using `raster::extract(raster, vertex, method = "bilinear")`. Vertices
#' are transformed into the space used by the raster if possible. (WIP ... Otherwise a warning is issued if 
#' there's not overlap ... WIP) 
#' 
#' If z is a character value, that column is taken from the object table. 
#' 
#' The `.id` argument must be character and exist as a column name in the object table. 
#' 
#' If z is a vector it's simply copied down. 
#' 
#' No checking is done on the type of the result, and so there's nothing to stop the use of the recyling rule
#' to expand out values, and nothing to stop the use of non numeric values being copied down. It's your model. 
#' @param x a silicate model
#' @param z object specifying values to copy down, a vector of values, a column name, a raster (see details)
#' @param ... currently ignored
#' @param .id character value, the name of the resulting column in the vertices, default is "z_"
#'
#' @return silicate model with vertex values copied to vertices
#' @export
#'
#' @examples
#' 
copy_down <- function(x, z = NULL, ..., .id = "z_") {
  stopifnot(is.character(.id))
  UseMethod("copy_down")
}
#' @name copy_down
copy_down.sc <- function(x, z = NULL, ..., .id = "z_") {
  
  if (inherits(z, "BasicRaster")) {
    
    xy <- as.matrix(x$vertex[c("x_", "y_")])
    p1 <- get_proj(x)
    p2 <- get_proj(z)
    if (!anyNA(c(p1, p2)) && !(p1 == p2)) {
      message("transforming model vertices to raster coordinate system")
      xy <- proj4::ptransform(xy * pi/180, p1, p2, silent = TRUE)
    }
    z <- raster::extract(z[[1L]], xy, method = "bilinear")
  }
  if (is.null(z)) {
    ## nothing required
  } else {
    x$vertex[[.id]] <- z
  }
  x  
}
#' @name copy_down
copy_down.PATH <- function(x, z = NULL, ..., .id = "z_") {
  stopifnot(.id %in% names(x$object))
  if (is.character(z)) {
    ## we need the join_ramp
    dmap <- x$object[c(z, "object_")] %>% 
      dplyr::inner_join(x$path[c("path_", "object_")], "object_") %>%
      dplyr::select(z, path_) %>% 
      dplyr::inner_join(x$path_link_vertex, "path_")  %>% 
      dplyr::select(z, vertex_) %>% dplyr::distinct(vertex_, .keep_all = TRUE)
    x$vertex[[.id]] <- dmap[[z]][match(dmap$vertex_, x$vertex$vertex_)]
  }
  x
}

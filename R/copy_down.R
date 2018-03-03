# v <- out$tXv %>% dplyr::inner_join(out$t) %>% 
#   dplyr::inner_join(out$o %>% dplyr::select(.data$object_, z), "object_") %>% 
#   dplyr::inner_join(out$v %>% dplyr::select(.data$vertex_, .data$x_, .data$y_)) %>% 
#   dplyr::select(.data$x_, .data$y_, z, .data$vertex_, .data$triangle_)
# 
# names(v)[names(v) == z] <- "z_"
# names(v)[names(v) == "vertex_"] <- "old"
# 
# gp <- dplyr::group_indices(v,  .data$x_, .data$y_, .data$z_)
# v$vertex_ <- silicate::sc_uid(length(unique(gp)))[gp]
# tXv <- v %>% dplyr::select(.data$vertex_, .data$triangle_)
# out$tXv <- tXv
# v$old <- NULL
# 
# 



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
#' library(raster)
#' r <- raster(volcano)
#' cl <- silicate::SC(rasterToContour(r))
#' plot3d(copy_down(cl, r)); rgl::rglwidget()
#' ## looks funny?
#' auto_3d(z = 15); rgl::rglwidget()
copy_down <- function(x, z = NULL, ..., .id = "z_") {
  stopifnot(is.character(.id))
  UseMethod("copy_down")
}

copy_downRaster<- function(x, z = NULL, ..., .id = "z_") {
    
    xy <- as.matrix(x$vertex[c("x_", "y_")])
    p1 <- get_proj(x)
    p2 <- get_proj(z)
    if (!anyNA(c(p1, p2)) && !(p1 == p2)) {
      if (grepl("longlat", p1) && grepl("longlat", p2)) {
        warning("both proj are different longlat, no transformation done")
        
      } else {
     message("transforming model vertices to raster coordinate system for copy down")
     xy <- proj4::ptransform(xy * pi/180, p1, p2, silent = TRUE)[, c(1, 2)]
      }
     }
    z <- raster::extract(z[[1L]], xy, method = "bilinear")

        x$vertex[[.id]] <- z
  
  
  x  
}



#' @name copy_down
#' @export
copy_down.SC <- function(x, z = NULL, ..., .id = "z_") {
  if (inherits(z, "BasicRaster")) {
   return(copy_downRaster(x, z = z, ..., .id = .id) )
  }
  denorm_PRIM_addZ(x, z = z, ..., .id = .id)
}

#' @name copy_down
#' @export
copy_down.TRI <- function(x, z = NULL, ..., .id = "z_") {
  if (inherits(z, "BasicRaster")) {
    return(copy_downRaster(x, z = z, ..., .id = .id) )
  }
  
  denorm_PRIM_addZ(x, z = z, ..., .id = .id)
}
#' @name copy_down
#' @export
copy_down.PATH <- function(x, z = NULL, ..., .id = "z_") {
  if (inherits(z, "BasicRaster")) {
    return(copy_downRaster(x, z = z, ..., .id = .id) ) 
  }
  
  denorm_SEQ_addZ(x, z = z, ..., .id = .id)
}

#' @name copy_down
#' @export
copy_down.ARC <- function(x, z = NULL, ..., .id = "z_") {
  if (inherits(z, "BasicRaster")) {
    return(copy_downRaster(x, z = z, ..., .id = .id) )
  }
  
  denorm_SEQ_addZ(x, z = z, ..., .id = .id)
}

#  #@name copy_down
#  #@export
# copy_down.SC <- function(x, z = NULL, ..., .id = "z_") {
#   dmap <- x$object[c(z, "object_")] %>% 
#     dplyr::inner_join(x$object_link_edge[c("edge_", "object_")], "object_") %>%
#     dplyr::select(z, edge_) %>% 
#     dplyr::inner_join(x$edge, "edge_")  %>% 
#     dplyr::select(z, vertex_) %>% dplyr::distinct(vertex_, .keep_all = TRUE)
#   x$vertex[[.id]] <- dmap[[z]][match(dmap$vertex_, x$vertex$vertex_)]
#   
# }
#  #@name copy_down
#  #@export
# copy_down.PATH <- function(x, z = NULL, ..., .id = "z_") {
#   stopifnot(.id %in% names(x$object))
#   if (is.character(z)) {
#     ## we need the join_ramp
#     dmap <- x$object[c(z, "object_")] %>% 
#       dplyr::inner_join(x$path[c("path_", "object_")], "object_") %>%
#       dplyr::select(z, path_) %>% 
#       dplyr::inner_join(x$path_link_vertex, "path_")  %>% 
#       dplyr::select(z, vertex_) %>% dplyr::distinct(vertex_, .keep_all = TRUE)
#     x$vertex[[.id]] <- dmap[[z]][match(dmap$vertex_, x$vertex$vertex_)]
#   }
#   x
# }
#  #@name copy_down
#  #@export
# copy_down.TRI <- function(x, z = NULL, ..., .id = "z_") {
#   stopifnot(.id %in% names(x$object))
#   if (is.character(z)) {
#     ## we need the join_ramp
#     dmap <- x$object[c(z, "object_")] %>% 
#       dplyr::inner_join(x$triangle)
# ##    WIP this is way trickier because we have to split the vertices again
#     }
#   x
# }
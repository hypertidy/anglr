## anglr generic will have a "type" - surface or segment
## use this to specify or override the inferred type, and that calls anglr_lines or anglr_polys

## generic will
## get_proj


get_proj <- function(x, ...) UseMethod("get_proj")
get_proj.default <- function(x, ...) {
  mt <- try(x[["meta"]], silent = TRUE)
  if (inherits(mt, "data.frame")) return(mt[["meta"]][["proj"]])
  op <- options(warn = -1)
  on.exit(op)
  rp <- try(raster::projection(x), silent = TRUE)
  if (inherits(rp, "try-error")) rp < - NA
  as.character(rp)
}
get_proj.sf <- function(x, ...) {
  attr(x[[attr(x, "sf_column")]], "crs")[["proj4string"]]
}
get_proj.sfc <- function(x, ...) {
  attr(x, "crs")[["proj4string"]]
}
## should be a sc method, but silicate needs meta everywhere
get_proj.PATH <- function(x, ...) {
  x$meta$proj
}



#' @rdname anglr
#' @importFrom dplyr %>%  arrange distinct mutate
#' @importFrom sp geometry  over SpatialPoints proj4string CRS SpatialPolygonsDataFrame
#' @importFrom dplyr inner_join
#' @importFrom RTriangle pslg triangulate
#' @importFrom spbabel map_table
#' @importFrom tibble tibble
#' @importFrom methods slotNames
#' @export

#' @name anglr
#' @export
anglr.default <- function(x, z = NULL, ..., type = NULL, max_area = NULL) {
  ## we don't need methods for sf, Spatial etc because PATH covers those
 anglr(PATH(x), z = z, ..., type = type, max_area = max_area)
}



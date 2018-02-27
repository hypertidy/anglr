## anglr generic will have a "type" - surface or segment
## use this to specify or override the inferred type, and that calls anglr_lines or anglr_polys

## generic will
## get_proj


get_proj <- function(x, ...) UseMethod("get_proj")
get_proj.default <- function(x, ...) {
  mt <- try(x[["meta"]], silent = TRUE)
  if (inherits(mt, "data.frame")) return(mt[["meta"]][["proj"]])
  raster::projection(x)
}
get_proj.sf <- function(x, ...) {
  attr(x[[attr(x, "sf_column")]], "crs")[["proj4string"]]
}
get_proj.sfc <- function(x, ...) {
  attr(x, "crs")[["proj4string"]]
}
get_proj.PATH <- function(x, ...) {
  x$meta$proj
}


#' @importFrom silicate PATH
#' @export
anglr.PATH <- function(x, z = NULL, ..., type = NULL, max_area = NULL) {
  pr4 <- get_proj(x)
  tabs <- x
  tabs$meta <- tibble::tibble(proj = pr4, ctime = format(Sys.time(), tz = "UTC"))
  thetype <- tabs[["path"]]$type[1]
 if (grepl("POLYGON", thetype, ignore.case = TRUE)) {
    out <- pfft_polys(tabs, ..., max_area = max_area)
  }
  if (grepl("LINE", thetype, ignore.case = TRUE)) {
    out <- anglr_lines(tabs)
  }
  
  if (inherits(z, "BasicRaster")) {
    ee <- raster::extract(z, as.matrix(out$v[, c("x_", "y_")]), method = "bilinear")
    rproj <- get_proj(z)
    vproj <- get_proj(out)
    if (is.na(rproj) || is.na(vproj) || !vproj == rproj) {
      warning(sprintf("model projection: %s\n\nz projection: %s", vproj, rproj))
    }
    if (all(is.na(ee))) warning("all raster values NA")
    out$v$z_ <- ee
    
    z <- NULL
  }
  if (!is.null(z)) {
    ## this line/poly thing is a mess, need to use SC as general case
    if (grepl("POLYGON", thetype, ignore.case = TRUE)) {
      v <- out$tXv %>% dplyr::inner_join(out$t) %>% 
        dplyr::inner_join(out$o %>% dplyr::select(.data$object_, z), "object_") %>% 
        dplyr::inner_join(out$v %>% dplyr::select(.data$vertex_, .data$x_, .data$y_)) %>% 
        dplyr::select(.data$x_, .data$y_, z, .data$vertex_, .data$triangle_)
      
      names(v)[names(v) == z] <- "z_"
      names(v)[names(v) == "vertex_"] <- "old"
      
      gp <- dplyr::group_indices(v,  .data$x_, .data$y_, .data$z_)
      v$vertex_ <- silicate::sc_uid(length(unique(gp)))[gp]
      tXv <- v %>% dplyr::select(.data$vertex_, .data$triangle_)
      out$tXv <- tXv
      v$old <- NULL
    } else {
    v <- out$lXv %>% dplyr::inner_join(out$l) %>% 
      dplyr::inner_join(out$o %>% dplyr::select(.data$object_, z), "object_") %>% 
      dplyr::inner_join(out$v %>% dplyr::select(.data$vertex_, .data$x_, .data$y_)) %>% 
      dplyr::select(.data$x_, .data$y_, z, .data$vertex_, .data$segment_)
    
    names(v)[names(v) == z] <- "z_"
    names(v)[names(v) == "vertex_"] <- "old"
    
    gp <- dplyr::group_indices(v,  .data$x_, .data$y_, .data$z_)
    v$vertex_ <- silicate::sc_uid(length(unique(gp)))[gp]
    lXv <- v %>% dplyr::select(.data$vertex_, .data$segment_)
    out$lXv <- lXv
    v$old <- NULL
    }

    out$v <- dplyr::distinct(v, .data$x_, .data$y_, .data$z_, .data$vertex_)
  }
  out
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



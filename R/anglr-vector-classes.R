## anglr generic will have a "type" - surface or segment
## use this to specify or override the inferred type, and that calls anglr_lines or anglr_polys

## generic will
## get_proj


get_proj <- function(x, ...) UseMethod("get_proj")
get_proj.default <- function(x, ...) {
  raster::projection(x)
}
get_proj.sf <- function(x, ...) {
  attr(x[[attr(x, "sf_column")]], "crs")[["proj4string"]]
}
get_proj.sfc <- function(x, ...) {
  attr(x, "crs")[["proj4string"]]
}
#' @importFrom silicate PATH
#' @export

anglr.sf <- function (x, z = NULL, ..., type = NULL, max_area = NULL) 
{
  pr4 <- get_proj(x)
  tabs <- silicate_to_gris_names(silicate::PATH(x))
  tabs$meta <- tibble::tibble(proj = pr4, ctime = format(Sys.time(), tz = "UTC"))
  thetype <- tabs[["b"]]$type[1]
  if (!is.null(type)) thetype <- type
  if (grepl("POLYGON", thetype)) {
 out <- anglr_polys(tabs, ..., max_area = max_area)
  if (!is.null(z)) {
    tmp <- out$tXv %>% 
      dplyr::inner_join(out$t, "triangle_") %>% 
      dplyr::inner_join(out$o %>% dplyr::select(object_, z), "object_") %>% 
      dplyr::select(vertex_,  z)
    out$v <- dplyr::inner_join(tmp, out$v, "vertex_") %>% 
      dplyr::select(vertex_, x_, y_, z)
    names(out$v)[names(out$v) == z] <- "z_"
    
    ## we now need to make new vertex_ ids based on uniqueness in x, y, z
    out$tXv$gp  <- out$v$gp <- dplyr::group_indices(out$v, x_, y_, z_)
    
    out$v <- dplyr::distinct(out$v, x_, y_, z_, gp)
    out$v$vertex_ <- silicate::sc_uid(nrow(out$v))
    out$tXv$vertex_ <- out$v$vertex_[match(out$tXv$gp, out$v$gp)]
    out$tXv$gp <- out$v$gp <- NULL
  }
       return(out)
}
  if (grepl("LINE", thetype)) {
    out <- anglr_lines(tabs)
    if (!is.null(z)) {
      
      ## this is totally broken now
      tmp <- out$lXv %>%  dplyr::distinct(segment_) %>% 
        dplyr::inner_join(out$l, "segment_") %>% 
        dplyr::inner_join(out$o %>% dplyr::select(object_, z), "object_") %>% 
        dplyr::select(segment_, z) %>% dplyr::distinct()
      

      out$v <- dplyr::inner_join(tmp, out$v,  "vertex_") %>% 
        dplyr::select(vertex_, x_, y_, z)
      names(out$v)[names(out$v) == z] <- "z_"
      
      out$lXv$gp  <- out$v$gp <- dplyr::group_indices(out$v, x_, y_, z_)
      
      out$v <- dplyr::distinct(out$v, x_, y_, z_, gp)
      out$v$vertex_ <- silicate::sc_uid(nrow(out$v))
      out$lXv$vertex_ <- out$v$vertex_[match(out$lXv$gp, out$v$gp)]
      out$lXv$gp <- out$v$gp <- NULL
    }
    return(out)
  }
  
  tabs
}
#' @export
anglr.PATH <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
  tabs <- silicate_to_gris_names(silicate::PATH(x))
  tabs$meta <- tibble::tibble(proj = pr4, ctime = format(Sys.time(), tz = "UTC"))
  thetype <- tabs[["b"]]$type[1]
  if (grepl("POLYGON", thetype)) {
    return(anglr_polys(tabs, ..., max_area = max_area))
  }
  if (grepl("LINE", thetype)) {
    return(anglr_lines(tabs))
  }
  tabs
  ## could be NULL
  #stop("woah, no type in this PATH - todo")
}

#' @rdname anglr
#' @importFrom dplyr %>%  arrange distinct mutate
#' @export
anglr.SpatialLines <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
  pr4 <- proj4string(x)
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialLinesDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  out <- anglr_lines(tabs)
  #tabs <- silicate::PATH(x)
  #tabs <- silicate_to_gris_names(tabs)
  out$meta <- tibble::tibble(proj = pr4,
                                 ctime = format(Sys.time(), tz = "UTC"))
  out
}



#' @rdname anglr
#' @export
#' @importFrom sp geometry  over SpatialPoints proj4string CRS SpatialPolygonsDataFrame
#' @importFrom dplyr inner_join
#' @importFrom RTriangle pslg triangulate
#' @importFrom spbabel map_table
#' @importFrom tibble tibble
#' @importFrom methods slotNames
anglr.SpatialPolygons <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
  pr4 <- proj4string(x)
  x0 <- x
  ## kludge for non DataFrames
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialPolygonsDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  out <- anglr_polys(tabs, max_area = max_area, ...)
  out$meta <- tibble::tibble(proj = pr4,
                             ctime = format(Sys.time(), tz = "UTC"))
  out
}


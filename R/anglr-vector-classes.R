#' @importFrom rlang .data
silicate_to_gris_names <- function(x) {
  names(x) <- c("o", "b", "v", "bXv")
  #x[["o"]] <- dplyr::rename(x[["o"]], object_  = .data$object)
  temp <- x[["b"]]
  temp$branch_ <- temp$path_
  temp$path_ <- NULL
  x[["b"]] <- temp
  #x[["b"]] <- dplyr::rename(x[["path"]], branch_ = .data$path_)
  
  thetype <- x[["b"]]$type[1]
  
  ## good grief, split order is a nightmare
  if (thetype == "MULTIPOLYGON") x[["b"]][["island_"]] <- unlist(lapply(split(x[["b"]], x[["b"]][["object_"]]), function(xa) !duplicated(xa[["subobject"]]))[unique(x[["b"]][["object_"]])])
  if (thetype == "POLYGON") x[["b"]][["island_"]] <- !duplicated(x[["b"]][["object_"]])
  #x[["bXv"]] <- dplyr::rename(x[["bXv"]], branch_ = .data$path_)
  temp <- x[["bXv"]]
  temp$branch_ <- temp$path_
  temp$path_ <- NULL
  x[["bXv"]] <- temp
  x
}

#' @importFrom silicate PATH
#' @export
anglr.sf <- function (x,  z = NULL, ..., max_area = NULL) 
{

  pr4 <- attr(x[[attr(x, "sf_column")]], "crs")[["proj4string"]]
  #tabs <- spbabel::map_table(x)
  tabs <- silicate::PATH(x)
  
  tabs <- silicate_to_gris_names(tabs)
  tabs$meta <- tibble::tibble(proj = pr4, ctime = format(Sys.time(), tz = "UTC"))
  
  thetype <- tabs[["b"]]$type[1]
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
 # tabs <- spbabel::map_table(x)
  ## otherwise M/POINT
  
  tabs

}
#' @export
anglr.PATH <- function(x, ...) {
  tabs <- silicate_to_gris_names(x)
  thetype <- tabs[["b"]]$type[1]
  if (grepl("POLYGON", thetype)) {
    return(anglr_polys(tabs, ...))
  }
  if (grepl("LINE", thetype)) {
    return(anglr_lines(tabs))
  }
  ## could be NULL
  stop("woah, no type in this PATH - todo")
}

#' @rdname anglr
#' @importFrom dplyr %>%  arrange distinct mutate
#' @export
anglr.SpatialLines <- function(x, ...) {
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
#' @section Warning:
#' anglr only checks for presence of triangle centres within
#' known holes, so this doesn't pick up examples of overlapping areas e.g. 
#' https://github.com/hypertidy/anglr/issues/39
#' @importFrom sp geometry  over SpatialPoints proj4string CRS SpatialPolygonsDataFrame
#' @importFrom dplyr inner_join
#' @importFrom RTriangle pslg triangulate
#' @importFrom spbabel map_table
#' @importFrom tibble tibble
#' @importFrom methods slotNames
anglr.SpatialPolygons <- function(x, max_area = NULL, ...) {
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


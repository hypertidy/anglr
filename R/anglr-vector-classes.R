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
anglr.sf <- function (x,  ..., max_area = NULL) 
{

  pr4 <- attr(x[[attr(x, "sf_column")]], "crs")[["proj4string"]]
  #tabs <- spbabel::map_table(x)
  tabs <- silicate::PATH(x)
  
  tabs <- silicate_to_gris_names(tabs)
  tabs$meta <- tibble::tibble(proj = pr4, ctime = format(Sys.time(), tz = "UTC"))
  
  thetype <- tabs[["b"]]$type[1]
  if (grepl("POLYGON", thetype)) {
   return(anglr_polys(tabs, ..., max_area = max_area))
  }
  if (grepl("LINE", thetype)) {
    return(anglr_lines(tabs))
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


#' @importFrom spbabel map_table
#' @export
rangl.sf <- function (x, max_area = NULL, ...) 
{
  pr4 <- attr(x[[attr(x, "sf_column")]], "crs")[["proj4string"]]
  #tabs <- spbabel::map_table(x)
  tabs <- silicate::PATH(x)
  names(tabs) <- c("o", "b", "v", "bXv")
  tabs[["o"]] <- dplyr::rename(tabs[["o"]], object_  = object)
  tabs[["b"]] <- dplyr::rename(tabs[["b"]], object_ = object, branch_ = path)
  #tabs[["b"]] <- dplyr::mutate(tabs[["b"]], island_ = subobject < 2)
  ## good grief, split order is a nightmare
  if (tabs[["b"]]$type[1] == "MULTIPOLYGON") tabs[["b"]][["island_"]] <- unlist(lapply(split(tabs[["b"]], tabs[["b"]][["object_"]]), function(xa) !duplicated(xa[["subobject"]]))[unique(tabs[["b"]][["object_"]])])
  if (tabs[["b"]]$type[1] == "POLYGON") tabs[["b"]][["island_"]] <- !duplicated(tabs[["b"]][["object_"]])
  tabs[["bXv"]] <- dplyr::rename(tabs[["bXv"]], branch_ = path)
  rangl_polys(tabs)

}


#' @rdname rangl
#' @importFrom dplyr %>%  arrange distinct mutate
#' @export
rangl.SpatialLines <- function(x, ...) {
  pr4 <- proj4string(x)
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialLinesDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  rangl_lines(tabs)
}



#' @rdname rangl
#' @export
#' @section Warning:
#' rangl only checks for presence of triangle centres within
#' known holes, so this doesn't pick up examples of overlapping areas e.g. 
#' https://github.com/r-gris/rangl/issues/39
#' @importFrom sp geometry  over SpatialPoints proj4string CRS SpatialPolygonsDataFrame
#' @importFrom dplyr inner_join
#' @importFrom RTriangle pslg triangulate
#' @importFrom spbabel map_table
#' @importFrom tibble tibble
#' @importFrom methods slotNames
rangl.SpatialPolygons <- function(x, max_area = NULL, ...) {
  pr4 <- proj4string(x)
  x0 <- x
  ## kludge for non DataFrames
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialPolygonsDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  rangl_polys(tabs, max_area = max_area, ...)
}


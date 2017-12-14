
#' @rdname anglr
#' @export
anglr.SpatialMultiPoints <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
  pr4 <- proj4string(x)
  if (! "data" %in% slotNames(x)) {
    dummy <- data.frame(row_number = seq_along(x))
    x <- sp::SpatialMultiPointsDataFrame(x, dummy, match.ID = FALSE)
  }
  tabs <- spbabel::map_table(x)
  tabs$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", ctime = format(Sys.time(), tz = "UTC"))
  
  class(tabs) <- "pointmesh"
  tabs
}


#' @rdname anglr
#' @export
anglr.SpatialPoints <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
   pr4 <- proj4string(x)
   if (! "data" %in% slotNames(x)) {
     dummy <- data.frame(row_number = seq_along(x))
     x <- sp::SpatialPointsDataFrame(x, dummy, match.ID = FALSE)
   }
   tabs <- spbabel::map_table(x)
  tabs$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", ctime = format(Sys.time(), tz = "UTC"))
  
  class(tabs) <- "pointmesh"
   tabs
}



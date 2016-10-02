
#' @rdname rangl
#' @export
rangl.SpatialMultiPoints <- function(x, ...) {
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


#' @rdname rangl
#' @export
rangl.SpatialPoints <- function(x, ...) {
  stop("you don't really need this function, just use `as.data.frame(x)`")
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



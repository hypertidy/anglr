
#' Convert map coordinates to Geocentric (XYZ) coordinates. 
#'
#' 
#' @param x list of tibbles, in \code{\link{anglr}} form
#' @param gproj Geocentric PROJ.4 string, defaults to WGS84
#' @param ... arguments to methods (none used)
#'
#' @return anglr object with vertices table modified
#' @export
#'
#' @examples
#' data(simpleworld)
#' g <- globe(anglr(as(simpleworld, "SpatialLinesDataFrame")))
#' if (interactive()) {
#'  plot(g, lwd = 3)
#'  }
globe <- function(x, ...) {
  UseMethod("globe")
}

#' @export
#' @rdname globe
globe.default <- function(x, gproj = "+proj=geocent +ellps=WGS84", ...) {
  p4 <- x$meta$proj[1]
  haveZ <- "z_" %in% names(x$v)
  
  ## need to handle if we already have a "z_"
  if (haveZ) {
    ll <- as.matrix(x$v[, c("x_", "y_", "z_")])
    
  } else { 
    ll <- cbind(as.matrix(x$v[, c("x_", "y_")]), 0)
  }
  
  if (grepl("longlat", p4)) ll <- ll * pi / 180
  xyz <- proj4::ptransform(ll, src.proj = p4, dst.proj = gproj)
  x$v$x_ <- xyz[,1]
  x$v$y_ <- xyz[,2]
  x$v$z_ <- xyz[,3]
  x$meta <- rbind(x$meta[1, ], x$meta)
  x$meta$proj[1] <- gproj
  x$meta$ctime[1] <- format(Sys.time(), tz = "UTC")
  x
}
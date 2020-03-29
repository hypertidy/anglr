
#' Convert map coordinates to Geocentric (XYZ) coordinates.
#'
#'
#' @param x list of tibbles, in silicate form
#' @param gproj Geocentric PROJ.4 string, defaults to WGS84
#' @param ... arguments to methods (none used)
#'
#' @return anglr object with vertices table modified
#' @export
#' @importFrom reproj reproj
#' @examples
#' data(simpleworld)
#' g <- globe(silicate::PATH(as(simpleworld, "SpatialLinesDataFrame")))
#' if (interactive()) {
#'  plot(g, lwd = 3)
#'  }
globe <- function(x, ...) {
  UseMethod("globe")
}

#' @export
#' @rdname globe
globe.default <- function(x, gproj = "+proj=geocent +datum=WGS84", ...) {
  p4 <- x$meta$proj[1]
  vertex <- x$vertex
  haveZ <- "z_" %in% names(vertex)

  ## zap towgs84 if present
  if (grepl("\\+towgs84=", p4, ignore.case = TRUE)) {
    warning("towgs84 element removed from source projection string")
    toks <- as.list(strsplit(p4, "\\s+")[[1L]])
    for (i in seq_along(toks)) {
      if (grepl("\\+towgs84=", toks[[i]])) {
        toks[[i]] <- NULL
        }
        p4 <- paste(unlist(toks), collapse = " ")
    }
  }
  ## need to handle if we already have a "z_"
  if (haveZ) {
    ll <- as.matrix(vertex[, c("x_", "y_", "z_")])

  } else {
    ll <- cbind(as.matrix(vertex[, c("x_", "y_")]), z_ = 0)
  }


  xyz <- reproj::reproj(ll, source = p4, target = gproj)
  x$vertex[c("x_", "y_", "z_")] <- as.data.frame(xyz)
  x$meta <- rbind(x$meta[1, ], x$meta)
  x$meta$proj[1] <- gproj
  x$meta$ctime[1] <- format(Sys.time(), tz = "UTC")
  x
}

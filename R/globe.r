
#' Geocentric (XYZ) coordinates
#'
#' Convert longitude/latitude coordinates to geocentric coordinates.
#'
#' With silicate data checks are made for the projection in use, but
#' not for mesh3d. In that case data are assumed to be
#' 'longitude,latitude,elevation'.
#' @param x a silicate model or mesh3d
#' @param gproj Geocentric PROJ.4 string, defaults to WGS84
#' @param ... arguments to methods (none used)
#'
#' @return object with vertices table modified
#' @export
#' @importFrom reproj reproj
#' @examples
#' data(simpleworld)
#' g <- globe(silicate::PATH(as(simpleworld, "SpatialLinesDataFrame")))
#' if (interactive()) {
#'  plot(g, lwd = 3)
#'  plot3d(g)
#'}
globe <- function(x, ...) {
  UseMethod("globe")
}


.ll_to_globe <- function(lonlatheight, rad = 6378137.0, exag = 1) {
  cosLat = cos(lonlatheight[,2] * pi / 180.0)
  sinLat = sin(lonlatheight[,2] * pi / 180.0)
  cosLon = cos(lonlatheight[,1] * pi / 180.0)
  sinLon = sin(lonlatheight[,1] * pi / 180.0)

  rad <- (exag * lonlatheight[,3] + rad)
  x = rad * cosLat * cosLon
  y = rad * cosLat * sinLon
  z = rad * sinLat

  cbind(x, y, z)
}

#' @export
#' @rdname globe
globe.mesh3d <- function(x, gproj = NULL, ...) {
  x$vb[1:3, ] <- .ll_to_globe(t(x$vb[1:3, ]))
  x
}
#' @export
#' @rdname globe
globe.default <- function(x, gproj = "+proj=geocent +datum=WGS84", ...) {
  p4 <- x$meta$proj[1]
  vertex <- get_vertex(x)
  if (inherits(x, "QUAD")) {
    if (is.null(vertex[["z_"]])) {
      v <- vxy(raster::as.matrix(raster::raster(matrix(x$quad$value, nrow = x$object$ncols))))
      vertex[["z_"]] <- v
    }
  }
  haveZ <- "z_" %in% names(vertex)

  ## zap towgs84 if present
  if ( !is.null(p4) && grepl("\\+towgs84=", p4, ignore.case = TRUE)) {
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

  if (!raster::isLonLat(p4)) {
    ll0 <- reproj::reproj(ll[,1:2], source = p4, target = "+proj=longlat +datum=WGS84")
    ll <- cbind(ll0, ll[,3])
  }
  #xyz <- .ll_to_globe(ll)
#print(head(xyz))
  ## BS, it doesn't work any more
  # xyz <- reproj::reproj(ll[,1:3], source = p4, target = gproj)
  # xyz <- do.call(cbind, PROJ::proj_trans_generic(ll[,1:2],
  #                                                 source = p4, target = gproj,
  #                                                z_ = ll[,3, drop = TRUE])[c("x_", "y_", "z_")])


  ## use proj4 as ever
  op <- options(reproj.mock.noproj6 = TRUE)
 suppressMessages( suppressWarnings(xyz <- reproj::reproj(ll[,1:3], source = p4, target = gproj)))
  options(op)

vertex[["x_"]] <- xyz[,1]
vertex[["y_"]] <- xyz[,2]
vertex[["z_"]] <- xyz[,3]
x$vertex <- vertex

x$meta <- rbind(x$meta[1, ], x$meta)
  x$meta$proj[1] <- gproj
  x$meta$ctime[1] <- format(Sys.time(), tz = "UTC")
  x
}

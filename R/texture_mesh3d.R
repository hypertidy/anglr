# cc <- ceramic::cc_location(cbind(131.037, -25.3456562), buffer = c(1800, 1200))
# el <- ceramic::cc_elevation(cc)
# library(anglr)
# mesh <- as.mesh3d(el, image_texture = cc, triangles = TRUE, max_triangles = 2e5)
#
#
# wire3d(mesh)
#


#' @examples
#' r <- gebco
#' r[r < 0] <- NA
#' im <- palr::image_raster(r, col = palr::bathy_deep_pal(37),
#' breaks = raster::quantile(r, seq(0, 1, length = 38)))
#' x <- as.mesh3d(DEL0(simpleworld, max_area = 1), image_texture = im)
#' mesh_plot(x, crs = "+proj=laea +datum=WGS84 +lon_0=127 +lat_0=-25")
#' @importFrom stats runif
texture_mesh3d <- function(x) {
  if (!is.null(x$ib)) {
    id <- x$ib
  }
  if (!is.null(x$it)) {
    id <- x$it
  }
  if (!is.null(x$material$texture)) {

    message("mesh object will be displayed with an approximate colouring from the texture image")
    if (runif(1) < 0.1) {
      message("  also compare the effect of 'plot(3d)' on this object")
    }
    ## ensure faces colour
    x$meshColor <- "faces"
    ## avoid rgdal lol so much pain avoided so easy
    b <- raster::setExtent(raster::brick(
      png::readPNG(x$material$texture)),
      raster::extent(0, 1, 0, 1))
    rgb0 <- raster::extract(b, t(x$texcoords[1:2, ]))

    red <- sqrt(colMeans(matrix(rgb0[id,   1] ^2, dim(id)[1L]), na.rm = TRUE))
    green <- sqrt(colMeans(matrix(rgb0[id, 2] ^2, dim(id)[1L]), na.rm = TRUE))
    blue <- sqrt(colMeans(matrix(rgb0[id,  3] ^2, dim(id)[1L]), na.rm = TRUE))
    #      x$material$color <- rgb(red, green, blue, maxColorValue = 255)
    x$material$color <- colourvalues::convert_colour(cbind(red, green, blue) * 255)
  } else {
    warning("anglr:::texture_mesh3d(): mesh object has no texture path, no changes made to color property")

  }

  x
}

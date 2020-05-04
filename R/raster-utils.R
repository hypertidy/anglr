# @importFrom raster crop
# @examples
# x <- crop(as.mesh3d(DEL0(gebco)), raster::extent(-179, 179, -88, 88))
# mesh_plot(x,
#          col = colourvalues::colour_values(x$vb[3, x$it[1, ]]))
# super dopey but there it is
# crop.mesh3d <- function(x, y, ...) {
#   xmin <- y@xmin
#   xmax <- y@xmax
#   ymin <- y@ymin
#   ymax <- y@ymax
#   ## don't at me, it's just for fun
#   idx <- (x$vb[1, ] >= xmin & x$vb[1, ] <= xmax &
#            x$vb[2, ] >= ymin & x$vb[2, ] <= ymax)
#   bad <- colSums(matrix(x$it %in%  which(idx), nrow = 3L) ) < 3
#   x$it <- x$it[,-which(bad)]
#   x
# }
# setOldClass("mesh3d")
# setMethod("crop", signature = c("mesh3d", "Extent"),
#           function(x, y, ...) {
#             crop.mesh3d(x, y, ...)
#           })
#
#

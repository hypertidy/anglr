# if there's a colour table emit a 3-layer RGB from it
.pal2rgb <- function(x, ...) {
  if (raster::nlayers(x) == 1 && length(x@legend@colortable) > 0) {
    x <- raster::setValues(raster::brick(x[[1]], x[[1]], x[[1]]),
                           t(grDevices::col2rgb(x@legend@colortable[raster::values(x) + 1])))
  }
  x
}
.zero_one <- function(x) {
  raster::setExtent(x, raster::extent(0, 1, 0, 1))
}
.texture_coordinates <- function (x, vertices) {
  raster::xyFromCell(.zero_one(x),
                     raster::cellFromXY(x, vertices))
}
# function is quadmesh::quadmesh(texture = ) logic
# @param img RGB raster
# @param source projection of the raster being mapped to
# @param exy the edge coordinates of each unique corner in the grid
# @return the texcoords array for mesh3d
.texture_map <- function(img, source, exy, ...) {
  errmsg <- "'img' must be a 3(or 4)-layer raster with RGB(A) values (in 0-255)"
  ## FIXME: could be an array, and so we would map it to [0, 1, 0, 1]
  if (!inherits(img, "BasicRaster")) {
    stop(errmsg)
  }
  ## only converts if needed
  img <- .pal2rgb(img)
  if (!raster::nlayers(img) %in% c(3, 4)) {
    stop(errmsg)
  }
 target_proj <- crsmeta::crs_proj(img)
  if (!is.na(target_proj)) {
    verts <- reproj::reproj(exy, target = target_proj,
                          source  = source)[,1:2, drop = FALSE]
  } else {
    verts <- exy
  }
  .texture_coordinates(img, vertices = verts)
}

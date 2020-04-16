
#' Convert to mesh object
#'
#' @description
#'
#' The `as.mesh3d()` generic function converts various objects to
#' [mesh3d][rgl::tmesh3d] objects. Methods are added to support a variety of
#' spatial formats, which will include triangles or quads according to their
#' inherent form. For quad-types the argument `triangles` can be specified to
#' generate triangles from quads. The majority of conversions are done by model
#' functions in the silicate package, and anglr adds models [DEL()], [DEL0()],
#' and [QUAD()].
#'
#' The [mesh3d][rgl::tmesh3d] format is the rgl workhorse behind
#' [plot3d()][rgl::plot3d], [wire3d()][rgl::wire3d], [persp3d()][rgl::persp3d]
#' and [dot3d()][rgl::dot3d].'
#'
#' A method for a numeric matrix is included, as are methods for sf, sp, raster,
#' RTriangle, and silicate types. and for a matrix.
#'
#' @details
#'
#' When converting a matrix to mesh3d it is considered to be quad-based (area
#' interpretation) within `xmin = 0, xmax = nrow(x), ymin = 0, ymax = ncol(x)`.
#' Note that this differs from the `[0, 1, 0, 1]` interpretation of [image()],
#' but shares its orientation. Raster-types from the raster package are
#' interpreted in the `t(ranspose), y-flip` orientation used by
#' `plot(raster::raster(matrix))`.
#'
#' The conversion function [anglr::as.mesh3d()] consolidates code from quadmesh
#' and angstroms packages where the basic facilities were developed. The
#' function [as.mesh3d()][rgl::as.mesh3d] is imported from rgl and re-exported,
#' and understands all of the surface types from sf, sp, raster, and silicate,
#' and can accept a raw matrix as input.
#'
#' When creating a surface mesh there is an optional `z` argument to extract
#' elevation values from a raster, and/or an `image_texture` argument to drape
#' an image from a raster RGB object onto the surface. Map projections are
#' automatically resolved to the coordinate system of the `x` argument.
#'
#' @section Implicit versus explicit topology:
#'
#' We support conversion to mesh for strictly linear types such as sf 'POLYGON',
#' 'MULTIPOLYGON', 'MULTILINESTRING', 'LINESTRING' and their sp counterparts
#' 'SpatialPolygons' and 'SpatialLines'. Even polygons are only *implicit
#' surfaces* and so conversion and plotting functions try to reflect this
#' inherent nature as much as possible. A mesh is inherently a surface, and so
#' the method for polygons or lines will first call a surface-generating
#' function, [silicate::TRI0()] or [DEL0()] in order to created the required
#' primitives, while [plot3d()][anglr::plot3d] will not do this. The key goal is
#' *flexibility*, and so we can call a meshing function
#' [as.mesh3d()][anglr::as.mesh3d] (does conversion) or
#' [persp3d()][anglr::persp3d] (a plot function, but requires conversion to
#' surface) and they will choose an interpretation. An non-formal guideline is
#' to use the cheapest method possible, i.e. [silicate::TRI0()].
#'
#' Much of the above is open for discussion, so please get in touch! Use the
#' [issues tab](https://github.com/hypertidy/anglr/) or [ping me on
#' twitter](https://twitter.com/mdsumner) to clarify or discuss anything.
#'
#' @section Elevation values with `z`:
#'
#' The 'z' argument can be a constant value or a vector of values to be used for
#' each vertex. Alternatively, it may be a spatial raster object from which 'z'
#' values are derived. If not set, the vertex 'z_' value from TRI/TRI0 is used,
#' otherwise z = 0' is assumed.
#'
#' @section Textures:
#'
#' Please see the documentation for rgl textures in
#' `vignette("rgl", package = "rgl")`.
#' The most important detail is that the `$material$color` property of
#' a `mesh3d` not be set to "black" ("#000000" or equivalent), or it will not be
#' visible at all. The only way to add a texture in mesh3d is as a PNG file
#' on-disk, so anglr functions take an in-memory object and create the file if
#' needed.
#'
#' @param x a surface-alike, a matrix, or spatial object from raster, sp, sf, trip, or silicate
#' @param z numeric vector or raster object (see details)
#' @inheritParams rgl::as.mesh3d.tri
#' @param ... arguments collected and passed to [rgl::tmesh3d()] as the `material` argument
#' @param image_texture an rgb object to texture the surface
#' @param meshColor how should colours be interpreted? 'vertices' or 'faces', for more
#' details see [rgl::tmesh3d].
#' @param keep_all whether to keep non-visible triangles
#' @param triangles for quad input types, the quads may optionally be split into triangles
#' @name as.mesh3d
#' @return a [mesh3d object][rgl::mesh3d]
#' @importFrom rgl as.mesh3d tmesh3d
#' @export as.mesh3d
#' @export
#' @seealso [dot3d] [wire3d] [persp3d] [plot3d]
#' @examples
#' sf <- silicate::minimal_mesh
#' #sf <- silicate::inlandwaters
#' x <- silicate::TRI(sf)
#' library(rgl)
#' clear3d(); plot3d(x); view3d(phi = -10)
#' ## simple convention to carry feature colours
#' sf$color_ <- c("firebrick", "dodgerblue")
#' clear3d(); plot3d(silicate::TRI(sf)); view3d(phi = -10)
#'
#' # material properties for $material are collected in ...
#' # and will override the 'color_' mech
#' x$object$color_ <- "black"
#' clear3d(); plot3d(as.mesh3d(x, color = rainbow(14)))
#'
#' ## we cannot assume TRI triangles relate to features simply
#' ##  but sometimes it does  (always does for TRI0)
#' cols <- c("black", "grey")[c(rep(1, 12), c(2, 2))]
#' clear3d(); plot3d(as.mesh3d(x, color = cols))
#'
#' ## smear by vertices meshColor
#' cols <- c("black", "grey")
#' clear3d(); plot3d(as.mesh3d(x, color = cols), meshColor = "vertices")
#'
#' ## other material properties
#' clear3d()
#' plot3d(as.mesh3d(x, color = cols, specular = "black"), meshColor = "vertices")
#' clear3d()
#' plot3d(as.mesh3d(x, color = cols, front = "lines", lwd = 5), meshColor = "vertices")
#' clear3d()
#' plot3d(as.mesh3d(x, color = viridis::viridis(20), alpha = 0.3), meshColor = "faces")
#' clear3d()
#' plot3d(as.mesh3d(x, color = viridis::viridis(5), alpha = 0.3), meshColor = "vertices")
#'
#' # TRI0 - index is stored structurally, not relations
#' x0 <- silicate::TRI0(sf)
#' clear3d(); plot3d(x0); view3d(phi = -10)
#'
#' # (TRI0 - it *is* guaranteed that triangle order is native)
#' clear3d(); plot3d(as.mesh3d(x0,  color = rainbow(14)))
#'
#' ## arbitrarily drape polygons over raster
#' r <- raster::setExtent(raster::raster(volcano), raster::extent(-0.1, 1.1, -0.1, 1.1))
#' clear3d();shade3d(as.mesh3d(DEL(silicate::minimal_mesh, max_area = 0.001), z =r))
#' aspect3d(1, 1, 0.5)
#'
#' \donttest{
#' library(rgl)
#' r1 <- raster::setExtent(raster::raster(volcano), raster::extent(silicate::inlandwaters))
#' clear3d();shade3d(as.mesh3d(DEL(silicate::inlandwaters, max_area = 1e9), z =r1))
#' aspect3d(1, 1, .2)
#'
#' ## fake news
#' rgl::wire3d(as.mesh3d(r1))
#' }
as.mesh3d.TRI <- function(x, z,  smooth = FALSE, normals = NULL, texcoords = NULL, ...,
                               keep_all = TRUE,
                               image_texture = NULL, meshColor = "faces") {
  as.mesh3d_internal(x, z = z, smooth = smooth, normals = normals, texcoords = texcoords, ...,
                               keep_all = keep_all, image_texture = image_texture,
                     meshColor = meshColor)
}
#' @name as.mesh3d
#' @export
as.mesh3d.TRI0 <- function(x, z,  smooth = FALSE, normals = NULL, texcoords = NULL, ...,
                          keep_all = TRUE,
                          image_texture = NULL, meshColor = "faces") {
  as.mesh3d_internal(x, z = z, smooth = smooth, normals = normals, texcoords = texcoords, ...,
                     keep_all = keep_all, image_texture = image_texture,
                     meshColor = meshColor)
}
#' @name as.mesh3d
#' @export
as.mesh3d.ARC <- function(x, triangles = FALSE,
                                  smooth = FALSE, normals = NULL, texcoords = NULL,
                                  ..., keep_all = TRUE, image_texture = NULL, meshColor = "faces") {
 as.mesh3d(DEL(x), triangles = triangles, smooth = smooth, normals = normals, texcoords = texcoords,
           keep_all = keep_all, image_texture = image_texture, meshColor = meshColor, ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.BasicRaster <- function(x, triangles = FALSE,
                                  smooth = FALSE, normals = NULL, texcoords = NULL,
                                  ..., keep_all = TRUE, image_texture = NULL, meshColor = "faces") {
  ## consider the case where x has 3 layers (xcrd, ycrd, zval) or we use
  ## arguments of the generic as.mesh3d(x, y, z) with 3 (or 2) separate rasters
  as.mesh3d(QUAD(x), triangles = triangles, smooth = smooth, normals = normals, texcoords = texcoords,
            keep_all = keep_all, image_texture = image_texture, meshColor = meshColor, ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.QUAD <- function(x, triangles = FALSE,
                           smooth = FALSE, normals = NULL, texcoords = NULL,
                           ..., keep_all = TRUE, image_texture = NULL, meshColor = "faces") {

  material <- list(...)  ## note that rgl has material <- .getMaterialArgs(...)
  ## for now we just warn if old-stlye material = list() was used
  if ("material" %in% names(material)) {
    warning("do not pass in 'material = list(<of properties>)' to as.mesh3d
             pass in 'rgl::material3d' arguments directly as part of '...'")
  }
  ## expensive, equivalent to TRI_add_shade
  if (!"color_" %in% names(x$quad)) {
    x$quad$color_ <- palr::image_pal(x$quad$value)
  }
  if (is.null(material$color) &&
      is.null(image_texture)) {
    material$color <- x$quad$color_

  }

  v <- get_vertex(x)
  if (is.null(v[["z_"]])) {  ## we might have already globe()d
    m <- matrix(x$quad$value, x$object$ncols)
    v$z_ <- vxy(m)
  }
  vb <- rbind(x = v$x_, y = v$y_, z = v$z_, h = 1)

  ## use the geometry to remap the texture if needed
  if (!is.null(image_texture)) {
    if (!is.null(texcoords)) {
      warning("must supply only one of 'texcoords' or 'image_texture' argument, 'image_texture' will be ignore")
    }
    texcoords <- .texture_map(image_texture,
                              crsmeta::crs_proj(x),
                              exy = t(vb[1:2, , drop = FALSE]))

    if (is.null(material$texture)) {
      material$texture <- tempfile(fileext = ".png")
      material$color <- "#FFFFFFFF"
    }
    if (!grepl("png$", material$texture)) {
      warning(sprintf("'texture = %s' does not look like a good PNG filename",
                      material$texture))
    }
    message(sprintf("writing texture image to %s", material$texture))
    png::writePNG(raster::as.array(image_texture) / 255, material$texture)
  }

 quad_common(vb, get_index(x), normals, texcoords, material, meshColor,  triangles, smooth)
}
#' @name as.mesh3d
#' @export
as.mesh3d.triangulation <- function(x, ...) {
  ## FIXME: no support here for textures etc.
  if (dim(x$T)[1L] < 1L) {
    stop("nothing to create a mesh from, no triangles in `x$T`")
  }
  z <- 0
  if (dim(x$PA)[2L] > 0) {
    z <- x$PA[,1L, drop = FALSE]
    if (dim(x$PA)[2L] > 1L) {
      message("'PA' array has more than one column, using the first as 'Z' coordinate")
    }
  }
  rgl::tmesh3d(t(cbind(x$P, z = z, h = 1)),
               t(x$T))
}

#' @name as.mesh3d
#' @export
as.mesh3d.sfc <-function(x, triangles = FALSE,
                        smooth = FALSE, normals = NULL, texcoords = NULL,
                        ..., keep_all = TRUE, image_texture = NULL, meshColor = "faces") {

  ## TRI or DEL or SC?
  as.mesh3d(DEL0(x), triangles = triangles, smooth = smooth, normals = normals, texcoords = texcoords,
            keep_all = keep_all, image_texture = image_texture, meshColor = meshColor, ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.sf <-function(x, triangles = FALSE,
                            smooth = FALSE, normals = NULL, texcoords = NULL,
                            ..., keep_all = TRUE, image_texture = NULL, meshColor = "faces") {

  ## TRI or DEL or SC?
  as.mesh3d(DEL0(x), triangles = triangles, smooth = smooth, normals = normals, texcoords = texcoords,
            keep_all = keep_all, image_texture = image_texture, meshColor = meshColor, ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.Spatial <-function(x, triangles = FALSE,
                        smooth = FALSE, normals = NULL, texcoords = NULL,
                        ..., keep_all = TRUE, image_texture = NULL, meshColor = "faces") {

  ## TRI or DEL or SC?
  as.mesh3d(DEL0(x), triangles = triangles, smooth = smooth, normals = normals, texcoords = texcoords,
            keep_all = keep_all, image_texture = image_texture, meshColor = meshColor, ...)
}

#' @name as.mesh3d
#' @export
as.mesh3d.matrix <-function(x, triangles = FALSE,
                            smooth = FALSE, normals = NULL, texcoords = NULL,
                            ..., keep_all = TRUE, image_texture = NULL, meshColor = "faces") {

  material <- list(...)  ## note that rgl has material <- .getMaterialArgs(...)
  ## for now we just warn if old-stlye material = list() was used
  if ("material" %in% names(material)) {
    warning("do not pass in 'material = list(<of properties>)' to as.mesh3d
             pass in 'rgl::material3d' arguments directly as part of '...'")
  }

  if (is.null(material$color) &&
      is.null(image_texture)) {
    material$color <-  palr::image_pal(x)

  }

  ## from https://github.com/hypertidy/quadmesh/blob/80380db26153615c365dc67b64465448beab2832/R/exy_values.R#L51-L72
  vals <- vxy(x)
  exy <- edges_xy(x)

  dm <- dim(x)
  ## this was developed against raster, so nc is nr ;)
  nc <- dm[1L]
  nr <- dm[2L]
  nc1 <- nc + 1
  aa <- t(prs(seq_len(nc1)))
  ind <- matrix(c(rbind(aa, aa[2:1, ])) + c(0, 0, nc1, nc1), 4)
  ind0 <- as.integer(as.vector(ind) +
                       rep(seq(0, length = nr, by = nc1), each = 4 * nc))

  vb <- rbind(t(exy), vals, 1)
  ind1 <- matrix(ind0, nrow = 4)

  ## use the geometry to remap the texture if needed
  if (!is.null(image_texture)) {
    if (!is.null(texcoords)) {
      warning("must supply only one of 'texcoords' or 'image_texture' argument, 'image_texture' will be ignore")
    }
    texcoords <- .texture_map(image_texture,
                              crsmeta::crs_proj(x),
                              exy = vb[, 1:2, drop = FALSE])
    if (is.null(material$texture)) {
      material$texture <- tempfile(fileext = ".png")
      material$color <- "#FFFFFFFF"
    }
    if (!grepl("png$", material$texture)) {
      warning(sprintf("'texture = %s' does not look like a good PNG filename",
                      material$texture))
    }
    message(sprintf("writing texture image to %s", material$texture))
    png::writePNG(raster::as.array(image_texture) / 255, material$texture)
  }
  quad_common(vb, ind1, normals, texcoords, material, meshColor, triangles, smooth)

}

#' @name as.mesh3d
#' @export
as.mesh3d.sc <- function(x, ...) {
  ## fallback for anything that can be triangulated
  tst <- try(DEL0(x), silent = TRUE)
  if (inherits(tst, "try-error")) {
    stop("not able to generate a surface from 'x'")
  }
  as.mesh3d(tst, ...)
}

TRI_xyz <- function(x, z) {
  if (missing(z)) z <- 0
  haveZ <- "z_" %in% names(x$vertex)
  if (haveZ) {
    xyz <- as.matrix(x$vertex[c("x_", "y_", "z_")])
  } else {
    xyz <- cbind(as.matrix(x$vertex[c("x_", "y_")]), z)
  }
  xyz
}
TRI_add_shade <- function(x) {
  if (!"color_" %in% names(x$object)) {
    x$object$color_ <- grDevices::grey(seq(0.1, 0.9, length.out = nrow(x$object)))
  }
  x
}
TRI_primitives_index <- function(x, keep_all = FALSE) {
  if (inherits(x, "TRI")) {
    pindex <- x$triangle
    if (!keep_all && !is.null(pindex[["visible_"]])) {
      pindex <- dplyr::filter(pindex, .data$visible_)
    }
    index <- matrix(match(c(t(as.matrix(pindex[c(".vx0", ".vx1", ".vx2")]))), x$vertex$vertex_),
                    nrow = 3L)
  }
  if (inherits(x, "TRI0")) {
    index <- t(do.call(rbind, lapply(x$object$topology_, function(ix) as.matrix(ix[c(".vx0", ".vx1", ".vx2")]))))
  }
  index
}

# internal function shared by as.mesh3d.TRI and as.mesh3d.TRI0
as.mesh3d_internal <- function(x, z,  smooth = FALSE, normals = NULL, texcoords = NULL, ...,
                               keep_all = TRUE,
                               image_texture = NULL, meshColor = "faces") {
  material <- list(...)  ## note that rgl has material <- .getMaterialArgs(...)
  ## for now we just warn if old-stlye material = list() was used
  if ("material" %in% names(material)) {
    warning("do not pass in 'material = list(<of properties>)' to as.mesh3d
             pass in 'rgl::material3d' arguments directly as part of '...'")
  }
  x <- TRI_add_shade(x)  ## sets color_ if not present

  if (is.null(material$color) &&
      is.null(image_texture)) {
    if (inherits(x, "TRI")) {
      material$color  <- x$object$color_[match(x$triangle$object_, x$object$object_)]
    }
    if (inherits(x, "TRI0")) {
      ## we need the number of rows of each nested index df
      ## but below we smash that when getting vindex
      material$color <- rep(x$object$color_, unlist(lapply(x$object$topology_, function(ix) dim(ix)[1L])))
    }
  }


  if (!missing(z) && inherits(z, "BasicRaster")) {
    if (!is.finite(crsmeta::crs_proj(z))) {
      z <- raster::extract(z[[1]], cbind(x$vertex$x_, x$vertex$y_), method = "bilinear")
    }  else {
      z <- raster::extract(z[[1]], reproj::reproj(cbind(x$vertex$x_, x$vertex$y_), crsmeta::crs_proj(z),
                                                  source = crsmeta::crs_proj(x))[, 1:2, drop = FALSE], method = "bilinear")
    }
  }

  ## FIXME: check for sanity in visible triangles

  ## the geometry
  vb <- TRI_xyz(x, z)
  ## the topology
  vindex <- TRI_primitives_index(x, keep_all = keep_all)

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

    }
    if (!grepl("png$", material$texture)) {
      warning(sprintf("'texture = %s' does not look like a good PNG filename",
                      material$texture))
    }
    message(sprintf("writing texture image to %s", material$texture))
    png::writePNG(raster::as.array(image_texture) / 255, material$texture)
  }

  ## NOTE: no partial naming allowed ($col for example cause heisenbugs so it's not negotiable)
  if (is.null(material$color))   material$color <- "#FFFFFFFF"  ## not black, else texture is invisible

  out <- tmesh3d(rbind(t(vb), h = 1),
                 vindex,
                 normals = normals, texcoords = texcoords,
                 material= material, meshColor = meshColor)
  if (smooth) {
    out <- rgl::addNormals(out)
  }

  out
}


#' Mesh3d objects
#'
#' Methods for the mesh3d type from package rgl
#'
#'
#' The anglr package adds methods for the [rgl::as.mesh3d()] generic for
#' sf, sp, raster, RTriangle, silicate, and for a matrix. This function
#' is the rgl counterpart to [rgl::plot3d()], [rgl::wire3d()], [rgl::persp3d()] and
#' [rgl::dot3d()].
#'
#'
#' The 'z' argument can be a constant value or a vector of values to be
#' used for each vertex. Alternatively, it may be a spatial raster object
#' from which 'z' values are derived. If not set, the vertex 'z_' value
#' from TRI/TRI0 is used, otherwise z = 0' is assumed.
#'
#' @section Textures:
#'
#' Please see the documentation for rgl textures in `vignette("rgl", package = "rgl")`.
#' The most important detail is that the `$material$color` property of a `mesh3d` not
#' be set to "black" ("#000000" or equivalent), or it will not be visible at all.
#' The only way to add a texture in mesh3d is as a PNG file on-disk, so anglr
#' functions take an in-memory object and create the file if needed.
#' @param x An object of class `TRI` or `TRI0`
#' @param z numeric vector or raster object (see details)
#' @inheritParams rgl::as.mesh3d.tri
#' @param ... arguments collected and passed to [rgl::tmesh3d()] as the `material` argument
#' @param image_texture an rgb object to texture the surface
#' @param meshColor how should colours be interpreted? 'vertices' or 'faces', for more
#' details see [rgl::tmesh3d].
#' @param keep_all whether to keep non-visible triangles
#' @param triangles for quad input types, the quads may optionally be split into triangles
#' @name as.mesh3d
#' @return a [mesh3d object](rgl::mesh3d)
#' @importFrom rgl as.mesh3d tmesh3d
#' @export as.mesh3d
#' @export
#' @seealso dot3d wire3d persp3d plot3d
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
#' clear3d(); plot3d(as.mesh3d(x, color = cols, specular = "black"), meshColor = "vertices")
#' clear3d(); plot3d(as.mesh3d(x, color = cols, front = "lines", lwd = 5), meshColor = "vertices")
#' clear3d(); plot3d(as.mesh3d(x, color = viridis::viridis(20), alpha = 0.3), meshColor = "faces")
#' clear3d(); plot3d(as.mesh3d(x, color = viridis::viridis(5), alpha = 0.3), meshColor = "vertices")
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


quad_common <- function(vb, index, normals, texcoords, material, meshColor, triangles, smooth) {
  ## deal with triangles = TRUE
  if (!triangles) {
    out <- do.call(rgl::qmesh3d, list(vertices = vb, indices = index,
                                        normals = normals, texcoords = texcoords,
                                        material = material,
                                        meshColor = "faces"))
  } else {
    out <- do.call(rgl::tmesh3d, list(vertices = vb, indices = .quad2tri(index),
                                        normals = normals, texcoords = texcoords,
                                        material = material,
                                        meshColor = "faces"))
  }
  if (smooth) {
    out <- rgl::addNormals(out)
  }
  out
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
as.mesh3d.BasicRaster <- function(x, triangles = FALSE,
                                  smooth = FALSE, normals = NULL, texcoords = NULL,
                                  ..., keep_all = TRUE, image_texture = NULL, meshColor = "faces") {
  ## consider the case where x has 3 layers (xcrd, ycrd, zval) or we use
  ## arguments of the generic as.mesh3d(x, y, z) with 3 (or 2) separate rasters
  as.mesh3d(QUAD(x), triangles = triangles, ...)
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
  m <- matrix(x$quad$value, x$object$ncols)
  zz <- vxy(m)
  vb <- rbind(v$x_, v$y_, zz, 1)


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




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


## consider args: z (as in as.mesh3d.tri)
##                triangles (can bust quads into tri)
##                col  (interleave with material = list()?)
##                normals, texcoords (but what for ...,
##                -- we can add texture, smooth for addNormals()
##  smooth - addNormals?
##                texture = RGBraster


#' Mesh3d objects
#'
#' Methods for the mesh3d type from package rgl
#'
#'
#' The 'z' argument can be a constant value or a vector of values to be
#' used for each vertex. Alternatively, it may be a spatial raster object
#' from which 'z' values are derived. If not set, the vertex 'z_' value
#' from TRI/TRI0 is used, otherwise z = 0' is assumed.
#'
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
#' @examples
#' sf <- silicate::minimal_mesh
#' #sf <- silicate::inlandwaters
#' x <- silicate::TRI(sf)
#' library(rgl)
#' clear3d(); plot3d(x); view3d(phi = -10)
#'
#' # manual face colours (it's not guaranteed that triangle order is native
#' # within original objects)
#'
#' clear3d(); plot3d(as.mesh3d(x, material = list(color = rainbow(14))))
#'
#' mts <- list(color = c("black", "grey")[c(rep(1, 12), c(2, 2))])
#' clear3d(); plot3d(as.mesh3d(x, material = mts))
#'
#' ## smear by vertices meshColor
#' mts1 <- list(color = c("black", "grey"))
#' clear3d(); plot3d(as.mesh3d(x, material = mts1), meshColor = "vertices")
#'
#' x0 <- silicate::TRI0(sf)
#' clear3d(); plot3d(x0); view3d(phi = -10)
#'
#' # (TRI0 - it *is* guaranteed that triangle order is native)
#' clear3d(); plot3d(as.mesh3d(x0,  material = list(color = rainbow(14))))
#'
#' ## arbitrarily drape polygons over raster
#' r <- raster::setExtent(raster::raster(volcano), raster::extent(-0.1, 1.1, -0.1, 1.1))
#' clear3d();shade3d(as.mesh3d(DEL(silicate::minimal_mesh, max_area = 0.001), z =r))
#'
#'
#'   aspect3d(1, 1, 0.5)
#' r1 <- raster::setExtent(raster::raster(volcano), raster::extent(silicate::inlandwaters))
#' clear3d();shade3d(as.mesh3d(DEL(silicate::inlandwaters, max_area = 0.001), z =r1))
#' aspect3d(1, 1, 0.5)
#'
as.mesh3d.TRI <- function(x, z,  smooth = FALSE, normals = NULL, texcoords = NULL, ...,
                          keep_all = TRUE,
                          image_texture = NULL, meshColor = "faces") {
  material <- list(...)
  x <- TRI_add_shade(x)  ## sets color_ if not present
  if (!missing(z) && inherits(z, "BasicRaster")) {
    if (!is.finite(crsmeta::crs_proj(z))) {
      z <- raster::extract(z[[1]], cbind(x$vertex$x_, x$vertex$y_))
    }  else {
        z <- raster::extract(z[[1]], reproj::reproj(cbind(x$vertex$x_, x$vertex$y_), crsmeta::crs_proj(z),
                                                source = crsmeta::crs_proj(x))[, 1:2, drop = FALSE], method = "bilinear")
    }
  }

  vb <- TRI_xyz(x, z)

  ## primitives
  pindex <- x$triangle
  if (!is.null(pindex[["visible_"]])) pindex <- dplyr::filter(pindex, .data$visible_)
  material <- list(...)$material
  set_color <- is.null(material) && is.null(material$color) && is.null(image_texture)

  if (set_color) {
    meshColor <- "faces"

    object_colors <- x$object$color_[match(pindex$object_, x$object$object_)]
    if (!keep_all && "visible_" %in% names(pindex)) {
      pindex <- pindex[pindex$visible_, ]
      if (nrow(pindex) < 1) stop("all 'visible_' property on '$triangle' are set to 'FALSE', nothing to plot\n try 'keep_all = TRUE'")
    }
  }

  vindex <- match(c(t(as.matrix(pindex[c(".vx0", ".vx1", ".vx2")]))), x$vertex$vertex_)

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
  if (is.null(material$color))   material$color <- "#FFFFFF00"  ## not black, else texture is invisible

  out <- tmesh3d(rbind(t(vb), h = 1),
                 matrix(vindex, nrow = 3L),
                 normals = normals, texcoords = texcoords,
                 material= material, meshColor = meshColor)
  if (smooth) {
    out <- rgl::addNormals(out)
  }
  ## override properties for color?
  if (set_color) out$material$color <- object_colors
  out
}
# as.mesh3d.default <- function(x,  ...) {
#   ## deal with sf, sp, PATH, PATH0
#   as.mesh3d(TRI0(x), ...)
# }
#' @name as.mesh3d
#' @export
as.mesh3d.TRI0 <- function(x, ..., meshColor = "faces") {
  x <- TRI_add_shade(x)  ## sets color_ if not present

  vb <- TRI_xyz(x)

  material <- list(...)$material
  set_color <- is.null(material) && is.null(material$color)

  if (set_color) {
    meshColor <- "faces"
    object_colors <- rep(x$object$color_, unlist(lapply(x$object$topology_, function(ix) dim(ix)[1L])))
  }

  out <- tmesh3d(rbind(t(vb), h = 1),
                 t(do.call(rbind, lapply(x$object$topology_, function(ix) as.matrix(ix[c(".vx0", ".vx1", ".vx2")])))),
                 ..., meshColor = meshColor)
  ## override properties for color?
  if (set_color) out$material$color <- object_colors
  out
}


#' @name as.mesh3d
#' @export
as.mesh3d.matrix <- function(x, triangles = FALSE,
                             smooth = FALSE, normals = NULL, texcoords = NULL, ...) {
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
  ind1 <- matrix(ind0, nrow = 4)
  ## for a matrix, we are done
  ## for raster, we have to apply the extent transformation
  cols <- viridis::viridis(100)
  ## deal with triangles = TRUE
  if (!triangles) {
  out <- rgl::qmesh3d(rbind(t(exy), vals, 1),
               ind1,
               normals = normals, texcoords = texcoords,
               material = list(color = cols[scales::rescale(vals, to = c(1, 100))])
  )
  } else {
    vals <- rep(vals, each = 2L)
    out <- rgl::tmesh3d(rbind(t(exy), vals, 1),
                 .quad2tri( ind1),
                 normals = normals, texcoords = texcoords,
                 material = list(color = cols[scales::rescale(vals, to = c(1, 100))])
    )
  }
  if (smooth) {
    out <- rgl::addNormals(out)
  }
  out
}
#' @name as.mesh3d
#' @export
as.mesh3d.BasicRaster <- function(x, triangles = FALSE, ...) {
  ## consider the case where x has 3 layers (xcrd, ycrd, zval) or we use
  ## arguments of the generic as.mesh3d(x, y, z) with 3 (or 2) separate rasters
  as.mesh3d(QUAD(x), triangles = triangles, ...)
}
#' @name as.mesh3d
#' @export
as.mesh3d.QUAD <- function(x, triangles = FALSE,
                           smooth = FALSE, normals = NULL, texcoords = NULL,
                           ...) {
  scl <- function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE))

  v <- get_vertex(x)
  m <- matrix(x$quad$value, x$object$ncols)
  zz <- vxy(m)
  vb <- rbind(v$x_, v$y_, zz, 1)

  cols <- viridis::viridis(84)
  if ("material" %in% names(list(...))) {
    material <- list(...)$material
    dots <- list(...)
    dots$material <- NULL
  } else {
    vals <- x$quad$value
    if (triangles) vals <- rep(vals, each = 2)
    material <- list(color = cols[scl(vals) * length(cols) + 1])
    dots <- NULL
  }


  ## deal with triangles = TRUE
  if (!triangles) {
  out <- do.call(rgl::qmesh3d, c(list(vertices = vb, indices = get_index(x),
                                      normals = normals, texcoords = texcoords,
        material = material,
         meshColor = "faces"), dots))
  } else {
    out <- do.call(rgl::tmesh3d, c(list(vertices = vb, indices = .quad2tri(get_index(x)),
                                        normals = normals, texcoords = texcoords,
                                        material = material,
                                        meshColor = "faces"), dots))
  }
  if (smooth) {
    out <- rgl::addNormals(out)
  }
  out
}





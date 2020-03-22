TRI_xyz <- function(x) {
  haveZ <- "z_" %in% names(x$vertex)
  if (haveZ) {
    xyz <- as.matrix(x$vertex[c("x_", "y_", "z_")])
  } else {
    xyz <- cbind(as.matrix(x$vertex[c("x_", "y_")]), 0)
  }
  xyz
}
TRI_add_shade <- function(x) {
  if (!"color_" %in% names(x$object)) {
    x$object$color_ <- grDevices::grey(seq(0.1, 0.9, length.out = nrow(x$object)))
  }
  x
}


#' Mesh3d objects
#'
#' Methods for the mesh3d type from package rgl
#'
#'
#' @param x An object of class `TRI` or `TRI0`
#' @param keep_all whether to keep non-visible triangles
#' @param ... arguments passed to [rgl::tmesh3d()]
#' @param meshColor rule for material properties used for colours (see [rgl::tmesh3d])
#'
#' @name as.mesh3d
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
as.mesh3d.TRI <- function(x, keep_all = TRUE, ..., meshColor = "faces") {
  x <- TRI_add_shade(x)  ## sets color_ if not present
  vb <- TRI_xyz(x)

  ## primitives
  pindex <- x$triangle
  if (!is.null(pindex[["visible_"]])) pindex <- dplyr::filter(pindex, .data$visible_)
  material <- list(...)$material
  set_color <- is.null(material) && is.null(material$color)

  if (set_color) {
    meshColor <- "faces"

    object_colors <- x$object$color_[match(pindex$object_, x$object$object_)]
    if (!keep_all && "visible_" %in% names(pindex)) {
      pindex <- pindex[pindex$visible_, ]
      if (nrow(pindex) < 1) stop("all 'visible_' property on '$triangle' are set to 'FALSE', nothing to plot\n try 'keep_all = TRUE'")
    }
  }

  vindex <- match(c(t(as.matrix(pindex[c(".vx0", ".vx1", ".vx2")]))), x$vertex$vertex_)

  out <- tmesh3d(rbind(t(vb), h = 1),
                 matrix(vindex, nrow = 3L), ..., meshColor = meshColor)
  ## override properties for color?
  if (set_color) out$material$color <- object_colors
  out
}
#' @name as.mesh3d
#' @export
as.mesh3d.default <- function(x,  ...) {
  ## deal with sf, sp, PATH, PATH0
  as.mesh3d(TRI0(x), ...)
}
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
as.mesh3d.matrix <- function(x,...) {
  ## from https://github.com/hypertidy/quadmesh/blob/80380db26153615c365dc67b64465448beab2832/R/exy_values.R#L51-L72
  v <- vxy(x)
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
  rgl::qmesh3d(rbind(t(exy), v, 1),
               ind1,
               material = list(color = cols[scales::rescale(v, to = c(1, 100))])
  )
}
#' @name
#' @export
as.mesh3d.BasicRaster <- function(x, ...) {
  as.mesh3d(QUAD(x), ..)
}
#' @name
#' @export
as.mesh3d.QUAD <- function(x, ...) {
  scl <- function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE))

  v <- get_vertex(x)
  m <- matrix(x$quad$value, x$object$ncols)
  zz <- vxy(m)
  vb <- rbind(v$x_, v$y_, zz, 1)

  cols <- viridis::viridis(84)
  rgl::qmesh3d(vb, get_index(x),
        material = list(color = cols[scl(x$quad$value) * length(cols) + 1]),
         meshColor = "faces")

}





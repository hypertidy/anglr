## --- silicate

.n_geometry <- function(x) {
  length(intersect(c("x_", "y_", "z_", "t_", "m_"), names(x$vertex)))
}

## --- as.mesh3d

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
    if (!keep_all && !is.null(pindex[["visible"]])) {
      pindex <- dplyr::filter(pindex, .data$visible)
    }
    index <- matrix(match(c(t(as.matrix(pindex[c(".vx0", ".vx1", ".vx2")]))), x$vertex$vertex_),
                    nrow = 3L)
  }
  if (inherits(x, "TRI0")) {
  ## flush out duds
    topology_ <- x$object$topology_
    tst <- unlist(lapply(topology_, is.null))

    if (any(tst)) topology_ <- topology_[!tst]
    index <- t(do.call(rbind, lapply(topology_, function(ix) as.matrix(ix[c(".vx0", ".vx1", ".vx2")]))))
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
  if (!is.null(x$triangle) && "visible" %in% names(x$triangle)) {
    x$triangle <- dplyr::filter(x$triangle, .data$visible)
  }


  if (is.null(material[["color"]]) &&
      is.null(image_texture)) {
    if (inherits(x, "TRI")) {
      material$color  <- x$object$color_[match(x$triangle$object_, x$object$object_)]
    }
    if (inherits(x, "TRI0")) {
      ## we need the number of rows of each nested index df
      ## but below we smash that when getting vindex

      nrs <- lapply(x$object$topology_, function(ix) dim(ix)[1L])
      tst <- unlist(lapply(nrs, is.null))
      #if (any(tst)) nrs[tst] <- 0
      color_ <- x$object$color_
      if (any(tst)) {
        color_ <- color_[!tst]
      }
      material$color <- rep(color_, unlist(nrs))
    }
  }
  ## workaround for https://github.com/hypertidy/anglr/issues/121
  if (length(material[["color"]]) < 1 || is.null(material[["color"]])) {
    material$color <- "#BBBBBBFF"
  }

  if (all(is.na(material$color))) {
    material$color[] <- "#BBBBBBFF"
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

## --- mesh_plot
#' @importFrom utils head tail
prs <- function(x) {
  cbind(head(x, -1), tail(x, -1))
}
expand_xy <- function(x, y) {
  ## fast matrix version of expand.grid
  cbind(x, rep(y, each = length(x)))
}

edges_xy <- function(x, ...) {
  ## corner coordinates from a matrix in 0,nrow 0,ncol
  dm <- dim(x)
  xx <- seq(0, dm[1L], length = dm[1L] + 1L)
  #yy <- seq(dm[2L], 0, length = dm[2L] + 1L)
  ## image() orientation
  yy <- seq(0, dm[2L], length = dm[2L] + 1L)

  expand_xy(x = xx, y = yy)
}

## from a matrix, calculate values at cell corner
vxy <- function(x, ...) {
  dm <- dim(x)
  nr <- dm[1L]
  nc <- dm[2L]
  ## top left
  tl <- cbind(NA_integer_, rbind(NA_integer_, x))
  ## top right
  tr <- cbind(NA_integer_, rbind(x, NA_integer_))
  ## bottom left
  bl <- cbind(rbind(NA_integer_, x), NA_integer_)
  ## bottom right
  br <- cbind(rbind(x, NA_integer_), NA_integer_)

  .colMeans(matrix(c(tl, tr, bl, br), 4L, byrow = TRUE),
            m = 4L, n = (nr + 1L) * (nc + 1L),
            na.rm = TRUE)
}

## --- copy_down


find_z <- function(x, z) {
  if (is.character(z)) {
    if (length(z) > 1 || !z[1] %in% names(x$object)) stop("z must be a named column on object")
    z <- x$object[[z]]
    if (!is.numeric(z)) warning("z is not numeric")
  }
  z
}
copy_downRaster<- function(x, z = NULL, ..., .id = "z_") {
  z <- find_z(x, z)
  xy <- as.matrix(x$vertex[c("x_", "y_")])
  p1 <- get_proj(x)
  p2 <- get_proj(z)

  if (!anyNA(c(p1, p2)) && !(p1 == p2)) {
    if (grepl("longlat", p1) && grepl("longlat", p2)) {
      warning("both proj are different longlat, no transformation done")

    } else {
      message("transforming model vertices to raster coordinate system for copy down")
      xy <- reproj::reproj(xy, source = p1, target = p2)[,c(1L, 2L)]
    }
  }
  z <- raster::extract(z[[1L]], xy, method = "bilinear")

  x$vertex[[.id]] <- z


  x
}

## --- QUAD

p_4 <- function(xp, nc) {
  (xp + c(0, 0, rep(nc, 2)))[c(1, 2, 4, 3)]
}
#' @importFrom utils tail head
prs0 <- function(x) {
  cbind(head(x, -1), tail(x, -1))
}
edges0 <- function(x) {
  as.matrix(expand.grid(seq(xmin(x), xmax(x), length = ncol(x) + 1),
                        seq(ymax(x), ymin(x), length = nrow(x) + 1)
  ))
}

## --- quadToTriangle


quadToTriangle <- function(x) {
  exy <- get_edges(x)
  v <- tibble(x_ = exy[,1], y_ = exy[,2])
  ## this originally from quadmesh
  v$z_ <- vxy(matrix(x$quad$value, x$object$ncols[1]))

  v$vertex_ <- seq(nrow(v))


  meta <- x$meta
  qXv <- get_qXv(x)
  n4 <- nrow(qXv) / 4L
  tXv_long <- tibble::tibble(vertex_ = qXv$vertex_[rep(c(1, 2, 3, 1, 3, 4), n4) + rep(seq(1, length = n4, by = 4)-1, each = 6)])
  tXv <- tibble::tibble(triangle_ = silicate::sc_uid(nrow(tXv_long)/3))

  tXv[c(".vx0", ".vx1", ".vx2")] <- as.data.frame(matrix(as.integer(tXv_long$vertex_), byrow = TRUE, ncol = 3))
  #tXv$triangle_ <- silicate::sc_uid(nrow(tXv)/3)[rep(seq(nrow(tXv)/3), each = 3)]
  tXv$visible <- TRUE
  uid <- silicate::sc_uid(nrow(v))
  v$vertex_ <- uid[v$vertex_]
  tXv$.vx0 <- uid[tXv$.vx0]
  tXv$.vx1 <- uid[tXv$.vx1]
  tXv$.vx2 <- uid[tXv$.vx2]
  tXv$object_ <- "1"  ## no link table any more
  x <- list(object = tibble::tibble(object_ = "1"),
            triangle = tXv,
            vertex = v, meta = meta)
  class(x) <- c("TRI", "sc")
  x
}


## --- texture-mapping
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
#' @importFrom raster xyFromCell
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

## --- miscellaneous (not organized):

.check_area <- function(x_, y_, max_area) {
  if (!is.null(max_area)) {
    check_x <- diff(range(x_, na.rm = TRUE))/sqrt(max_area)
    check_y <- diff(range(y_, na.rm = TRUE))/sqrt(max_area)

    max_triangles <- getOption("anglr.max.triangles")
    if ((check_x * check_y) > max_triangles && interactive()) {
      yes <- utils::askYesNo(sprintf("'max_area = %s' implies ~%i triangles, are you sure?",
                                     format(max_area), as.integer((check_x * check_y))))
      if (!yes) {
        stop("'getOption(\"anglr.max.triangles\")' exceeded, to avoid this check set a higher limit")
      }

    }
  }
  TRUE
}



# is there a screen device
screen_device <- function() {
  caps <- capabilities()[c("X11", "aqua")]
  ## can't use stats::na.omit() in .onLoad()
  caps <- caps[!is.na(caps)]
 length(caps) > 0 &&  (sum(caps) > 0 || exists("windows"))
}

#' @importFrom utils head
path2seg <- function(x) {
  ## this is a trick of array logic to generate paired indexes from a sequence
  head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)
}

trimesh_cols <- function(n) {
  viridis::viridis(n)
}

## from quadmesh::triangulate_quads
.quad2tri <- function (quad_index, clockwise = FALSE)
{
  if (clockwise) {
    matrix(rbind(quad_index[c(1L, 2L, 4L), ], quad_index[c(2L,
                                                           3L, 4L), ]), 3L)
  }
  else {
    matrix(rbind(quad_index[c(1L, 4L, 2L), ], quad_index[c(4L,
                                                           3L, 2L), ]), 3L)
  }
}

#' @importFrom raster projection setValues brick
.raster_to_coords <- function(x, crs = NULL) {
  xy <- sp::coordinates(x)
  if (!is.null(crs)) {
    xy <- reproj::reproj(xy, target  = crs,
                         source= raster::projection(x))[, 1:2]
  }
  raster::setValues(raster::brick(x, x),
                    xy)
}


#' Plot a mesh surface in 2D
#'
#' Draw a 2D interpretation of a mesh object, or a mesh-alike object.
#' This is very fast and can be used to created _approximately_ continuously
#' varying surface plots.
#'
#' The input is treated as a mesh and plotted in vectorized form using
#' 'grid'.
#'
#' The mesh may be reprojected prior to plotting using the 'crs' argument to
#' define the target map projection in 'PROJ string' format. (There is no
#' "reproject" function for quadmesh, this is performed directly on the x-y
#' coordinates of the 'quadmesh' output). The 'col' argument are mapped to the
#' inputdata as in [graphics::image()], and applied relative to 'zlim' if
#' also supplied.
#'
#' The `coords` argument only applies to a raster object. The `crs` argument
#' only applies to a spatial object that has a crs projection metadata string
#' understood by anglr (works, but still work in progress). There is a
#' change from the previous `quadmesh::mesh_plot()` function that requires
#' both crs and coords to be named. In quadmesh, crs was the second argument
#' to the `mesh_plot()` function and so in usage was normally not named.
#'
#' If `coords` is supplied, it is currently assumed to be a 2-layer
#' `RasterBrick` with longitude and latitude as the *cell values*. These are
#' used to geographically locate the resulting mesh, and will be transformed to
#' the `crs` if that is supplied. This is modelled on the approach to
#' curvilinear grid data used in the `angstroms` package. There the function
#' `angstroms::romsmap()` and `angstroms::romscoords()`` are used to separate
#' the complicated grid geometry from the grid data itself.
#'
#' @param x object to convert to mesh and plot
#' @param crs target map projection
#' @param col colours to use, defaults to that used by [graphics::image()]
#' @param add add to existing plot or start a new one
#' @param zlim absolute range of data to use for colour scaling (if `NULL` the
#'   data range is used)
#' @param ... passed through to `base::plot`
#' @param coords optional input raster of coordinates of each cell, see details
#' @return nothing, used for the side-effect of creating or adding to a plot
#' @name mesh_plot
#' @aliases [plot3d] [as.mesh3d] [persp3d] [dot3d] [wire3d] [shade3d]
#' @export
mesh_plot <- function(x,  col = NULL, add = FALSE, zlim = NULL, ..., coords = NULL, crs = NULL) {
  UseMethod("mesh_plot")
}
#' @name mesh_plot
#' @export
mesh_plot.mesh3d <-
  function (x,
            col = NULL,
            add = FALSE, zlim = NULL,
            ..., coords = NULL, crs = NULL) {

    if (!is.null(coords)) {
      warning("argument 'coords' is only used for 'mesh_plot(Raster)', ignoring")
    }
    if (!is.null(x$material$texture)) {
      warning("mesh object has a texture path, but cannot be displayed in 2D graphics (try plot3d)")
    }
    if (!is.null(x$ib)) {
      id <- x$ib
    }
    if (!is.null(x$it)) {
      id <- x$it
    }
    xx <- x$vb[1L, id]
    yy <- x$vb[2L, id]

    ID <- rep(seq_len(ncol(id)), each = nrow(id))

    ## TODO: determine face or vertex colouring and
    ## expand to face (with a message that vertex not supported)
    ## if colours present, otherwise build colours from z
    if (is.null(col))  {
      if (is.null(x$material$color)) {
        cols <- viridis::viridis(100)[scales::rescale(x$vb[3L, id[1L, ]], c(1, 100))]
      } else {
        cols <- x$material$color
      }
    } else {
      cols <- col
    }
    xx <- list(x = xx, y = yy, id = ID, col = cols)

    ## if (isLL) 1/cos(mean(xx$y, na.rm = TRUE) * pi/180) else 1
    if (!add) {
      graphics::plot.new()
      graphics::plot.window(xlim = range(xx$x, finite = TRUE), ylim = range(xx$y, finite = TRUE),
                            ...)
    }
    vps <- gridBase::baseViewports()

    grid::pushViewport(vps$inner, vps$figure, vps$plot)


    grid::grid.polygon(xx$x, xx$y, xx$id, gp = grid::gpar(col = NA, fill = xx$col),
                       default.units = "native")


    grid::popViewport(3)
    #if (debug) return(xx)

    invisible(NULL)

  }

#' @name mesh_plot
#' @export
#' @importFrom raster nlayers
mesh_plot.BasicRaster  <- function(x,  col = NULL, add = FALSE, zlim = NULL, ...,
                                   coords = NULL, crs = NULL) {

  mesh <- as.mesh3d(x, triangles = FALSE,  ...)

  if (is.null(coords) && !is.null(crs)) {
    coords <- .raster_to_coords(x)
 }

  if (!is.null(coords)) {
    nl <- raster::nlayers(coords)
    dm <- dim(x)[1:2]
    if (!nl >= 2L) {
      stop("coords must be a 2-layer raster matching the dimensions of 'x'")
    }
    if (nl > 2) warning("'coords' has more than 2 layers, ignoring layers 3+")
   if (!all(dim(coords)[1:2] == dm)) {
     stop("dimensions of 'x' and 'coords' do not match")
   }
    xy <- cbind(vxy(t(raster::as.matrix(coords[[1]]))),
                vxy(t(raster::as.matrix(coords[[2]]))))

    if (!is.null(crs)) {
      ## we were given coords, and a crs so we assume they are longlat
      xy <- reproj::reproj(xy,
                           target = crs, source = "+proj=longlat +datum=WGS84")[, 1:2]
    }
    ## drop primitives with missing vertices
  bad <- (!is.finite(xy[,1]) | !is.finite(xy[,2]))
  ## generalize to triangles also/either
  atst <- mesh$ib
  atst[] <- atst %in% which(bad)
  mesh$ib <- mesh$ib[, colSums(atst) < 1]

  mesh$vb[1:2, ] <- t(xy)

  }
 mesh_plot(mesh, crs = NULL, col = col, add = add, zlim = zlim)
}

#' @name mesh_plot
#' @export
mesh_plot.sc <- function(x,  col = NULL, add = FALSE, zlim = NULL, ...,
                         coords = NULL, crs = NULL) {
  if (!is.null(coords)) {
    warning("argument 'coords' is only used for 'mesh_plot(Raster)', ignoring")
  }
  if (!is.null(crs)) {
    x <- reproj::reproj(x, target = crs)  ## not sure this is even worth doing
  }
  mesh_plot(as.mesh3d(x, ...),
            crs = crs,
            col = col,
            add = add,
            zlim = zlim)
}

#' @name mesh_plot
#' @export
mesh_plot.default <- function(x,  col = NULL, add = FALSE, zlim = NULL, ...,
                              coords = NULL, crs = NULL) {
  if (!is.null(coords)) {
    warning("argument 'coords' is only used for 'mesh_plot(Raster)', ignoring")
  }
  if (!is.null(crs)) {
    xx <- try(reproj::reproj(silicate::PATH0(x), target = crs), silent  = TRUE)
    if (inherits(xx, "try-error")) {
      stop("unable to reproject 'x' with 'crs'")
    }
    x <- xx
  }
  mesh_plot(as.mesh3d(x, ...),
            crs = crs,
            col = col,
            add = add,
            zlim = zlim)
}

#' @name mesh_plot
#' @export
mesh_plot.triangulation <- function(x,  col = NULL, add = FALSE, zlim = NULL, ...,
                                    coords = NULL, crs = NULL) {
  mesh_plot(as.mesh3d(x), col = col, add = add )
}


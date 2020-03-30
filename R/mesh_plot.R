
#' Plot a mesh
#'
#' The input is treated as a mesh and plotted vectorized form using 'grid'.
#'
#' The mesh may be reprojected prior to plotting using the 'crs' argument to
#' define the target map projection in 'PROJ string' format. (There is no
#' "reproject" function for quadmesh, this is performed directly on the x-y
#' coordinates of the 'quadmesh' output). The 'col' argument are mapped to the
#' input applied object data as in 'image', and applied relative to 'zlim' if
#' su.
#'
#' If `coords` is supplied, it is currently assumed to be a 2-layer
#' `RasterBrick` with longitude and latitude as the *cell values*. These are
#' used to geographically locate the resulting mesh, and will be transformed to
#' the `crs` if that is supplied. This is modelled on the approach to
#' curvilinear grid data used in the `angstroms` package. There the function
#' `angstroms::romsmap()` and `angstroms::romscoords()`` are used to separate
#' the complicated grid geometry from the grid data itself. A small fudge is
#' applied to extend the coordinates by 1 cell to avoid losing any data due to
#' the half cell outer margin (get in touch if this causes problems!).
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
#' @export
mesh_plot <- function(x, crs = NULL, col = NULL, add = FALSE, zlim = NULL, ..., coords = NULL) {
  UseMethod("mesh_plot")
}
#' @name mesh_plot
#' @export
mesh_plot.mesh3d <-
  function (x,
            crs = NULL, col = NULL,
            add = FALSE, zlim = NULL,
            ..., coords = NULL) {

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
mesh_plot.default  <- function (x,
                                crs = NULL, col = NULL,
                                add = FALSE, zlim = NULL,
                                ..., coords = NULL) {
  mesh_plot(as.mesh3d(x, ...),
            crs = crs,
            col = col,
            add = add,
            zlim = zlim,
            coords = coords)
}




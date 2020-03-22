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
  yy <- seq(dm[2L], 0, length = dm[2L] + 1L)
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

#' Plot a mesh
#'
#'  The input is treated as a mesh and plotted vectorized form using 'grid'.
#'
#' The mesh may be reprojected prior to plotting using the 'crs' argument to
#' define the target map projection in 'PROJ string' format. (There is no
#' "reproject" function for quadmesh, this is performed directly on the x-y
#' coordinates of the 'quadmesh' output). The 'col' argument are mapped to the input pplied
#' object data as in 'image', and applied relative to 'zlim' if su.
#'
#' If `coords` is supplied, it is currently assumed to be a 2-layer `RasterBrick` with
#' longitude and latitude as the *cell values*. These are used to geographically locate
#' the resulting mesh, and will be transformed to the `crs` if that is supplied. This is
#' modelled on the approach to curvilinear grid data used in the `angstroms` package. There
#' the function `angstroms::romsmap()` and `angstroms::romscoords()`` are used to separate the complicated
#' grid geometry from the grid data itself. A small fudge is applied to extend the coordinates
#' by 1 cell to avoid losing any data due to the half cell outer margin (get in touch if this causes problems!).
#'
#' @param x object to convert to mesh and plot
#' @param crs target map projection
#' @param col colours to use, defaults to that used by [graphics::image()]
#' @param add add to existing plot or start a new one
#' @param zlim absolute range of data to use for colour scaling (if `NULL` the data range is used)
#' @param ... passed through to `base::plot`
#' @param coords optional input raster of coordinates of each cell, see details
#' @return nothing, used for the side-effect of creating or adding to a plot
#' @name mesh_plot
#' @export
mesh_plot <- function(x, crs = NULL, col = NULL, add = FALSE, zlim = NULL, ..., coords = NULL) {
  if ("colfun" %in% names(list(...))) {
    warning("argument colfun is deprecated, please use 'col' as per base plot")
  }
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
    cols <- viridis::viridis(100)[scales::rescale(x$vb[3L, id[1L, ]], c(1, 100))]
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




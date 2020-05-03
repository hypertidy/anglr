## to make lazy workflows
## we need to abstract away the details of what's
## actually stored

# Get vertex
#
# For explicit storage the default method simply returns
# "vertex" data frame, otherwise create it.
#
# The API goes something like
# # v <- get_vertex(x)  ## get just x/y from the grid spec
# # if (!is.null(x$vertex$z_)) v$z_ <- x$vertex$z_ ## because $vertex is there, but not $quad


get_vertex <- function(x, ...) {
  UseMethod("get_vertex")
}

get_vertex.default <- function(x, ...) {
  x[["vertex"]]
}

get_vertex.QUAD <- function(x, ...) {
  if (!is.null(x[["vertex"]])) return(x[["vertex"]])
  exy <- get_edges(x)
  tibble(x_ = exy[,1],
         y_ = exy[,2])
}


get_proj <- function(x, ...) {
  UseMethod("get_proj")
}

get_proj.default <- function(x, ...) {
  x_na <- NA_character_
  proj <- crsmeta::crs_proj(x)
  if (is.na(proj) || is.null(proj) || nchar(proj) < 1) {
    proj <- crsmeta::crs_input(x)
  }
  if (is.na(proj) || is.null(proj) || nchar(proj) < 1) {
    return(x_na)
  }
  proj
}

## --- QUAD
#' @importFrom raster raster
get_edges <- function(x, ...) {
  ## assuming a QUAD
  edges0( do.call(raster::raster, x$object[c("xmx", "xmn", "ymn", "ymx", "nrows", "ncols")]))
}
get_index <- function(x, ...) {
  x <- do.call(raster::raster, as.list(x$object[c("xmx", "xmn", "ymn", "ymx", "nrows", "ncols")]))
  ind <- apply(prs0(seq(ncol(x) + 1)), 1, p_4, nc = ncol(x) + 1)
  ## all face indexes
  ind0 <- as.vector(ind) +
    rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x))
  matrix(ind0, nrow = 4L)

}
get_qXv <- function(x, ...) {
  ind1 <- get_index(x)
  tibble(vertex_ = as.vector(ind1), quad_ = rep(seq(ncol(ind1)), each = 4))
}


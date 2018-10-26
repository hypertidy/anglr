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

get_meta <- function(x, all = FALSE, ...) {
  UseMethod("get_meta")
}
get_meta.default <- function(x, all = FALSE, ...) {
  meta <- x[["meta"]]
  if (!all) meta <- meta[1L, ] 
  meta
  
}
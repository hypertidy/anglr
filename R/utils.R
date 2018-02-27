maybe_geom_column <- function(x, ...) {
  names(x)[purrr::map_lgl(x, ~ inherits(.x, "list"))]
}

check_is_geom_column <- function(x, ...) {
  any(class(x[[1]]) == "sfg")
}

find_geom_column <- function(x) {
  purrr::map(maybe_geom_column(x), check_is_geom_column)
}
# 
#     i <- which(lgl)
#   nms <- names(x)
#   if (length(i) > 1) {
#     message(sprintf("found these geom columns %s using %s", 
#                     paste(nms[i], collapse = ", "), 
#                     nms[i[1]]))
#     i <- i[1]
#   }
#   if (length(i) == 1) {
#     nm <- nms[i]
#     return(x[[nm]])
#   }
#   message("no geom column found")
#   NULL
# }


#' @importFrom utils head
path2seg <- function(x) {
  ## this is a trick of array logic to generate paired indexes from a sequence
  head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)
}

th3d <- function() {
  structure(list(vb = NULL, it = NULL, primitivetype = "triangle",
                 material = list(), normals = NULL, texcoords = NULL), .Names = c("vb",
                                                                                  "it", "primitivetype", "material", "normals", "texcoords"), class = c("mesh3d",
                                                                                                                                                        "shape3d"))
}
trimesh_cols <- function(n) {
  viridis::viridis(n)
}
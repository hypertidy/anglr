#' @importFrom rlang .data
silicate_to_gris_names <- function(x) {
  names(x) <- c("o", "b", "v", "bXv")
  #x[["o"]] <- dplyr::rename(x[["o"]], object_  = .data$object)
  temp <- x[["b"]]
  temp$branch_ <- temp$path_
  temp$path_ <- NULL
  x[["b"]] <- temp
  #x[["b"]] <- dplyr::rename(x[["path"]], branch_ = .data$path_)
  
  thetype <- x[["b"]]$type[1]
  
  ## good grief, split order is a nightmare
  if (thetype == "MULTIPOLYGON") x[["b"]][["island_"]] <- unlist(lapply(split(x[["b"]], x[["b"]][["object_"]]), function(xa) !duplicated(xa[["subobject"]]))[unique(x[["b"]][["object_"]])])
  if (thetype == "POLYGON") x[["b"]][["island_"]] <- !duplicated(x[["b"]][["object_"]])
  #x[["bXv"]] <- dplyr::rename(x[["bXv"]], branch_ = .data$path_)
  temp <- x[["bXv"]]
  temp$branch_ <- temp$path_
  temp$path_ <- NULL
  x[["bXv"]] <- temp
  x
}


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
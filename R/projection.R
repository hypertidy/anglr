
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

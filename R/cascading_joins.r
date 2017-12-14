semi_join_be_quiet_if_there_is_only_1 <- function(x, y, by = NULL, copy = FALSE, ...) {
  comm <- intersect(names(x), names(y))
  if (length(comm) == 1L) {
    by <- comm
  }
  semi_join(x, y, by = by, copy = copy, ...)
}


#' Cascading subset on object for \code{map_table}. 
#'
#' @param x 
#' @param ... 
#' @importFrom dplyr semi_join
#' @noRd
semi_cascade <- function(x, ..., tables = c("o", "b", "bXv", "v")) {
  itab <- tables[1L]
  first <- dplyr::filter(x[[itab]], ...)
  x[[itab]] <- last <- first 
  tables <- tables[-1]
  for (itab in tables) {
    x[[itab]] <- last <- semi_join_be_quiet_if_there_is_only_1(x[[itab]], last)
  }
  x
}

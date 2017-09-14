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

# semi_cascade2 <- function(x, ..., tables = c("o", "b", "bXv", "v")) {
#   #qs <- rlang::quos(...)
#   first <- dplyr::filter(x[[tables[1]]], g == 1)
#   x[[tables[1]]] <- last <- first 
#   tables <- tables[-1]
#   for (itab in tables) {
#     print(names(last))
#     print(itab)
#     print(names(x[[itab]]))
#     last <- semi_join_be_quiet_if_there_is_only_1(x[[itab]], last)
#     x[[itab]] <- last
#   }
#   x
# }

# inner_cascade <- function(x, ..., tables = c("o", "b", "bXv", "v")) {
#   first <- dplyr::filter(x[[tables[1]]], ...)
#   #x[[1]] <- last <- first 
#   tables <- tables[-1]
#   for (itab in tables) {
#     first <-  dplyr::inner_join(x[[itab]], first)
#   }
#   first
# }
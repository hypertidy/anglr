#' @importFrom spbabel map_table
#' @export
rangl.sf <- function (x, max_area = NULL, ...) 
{
  pr4 <- sf::st_crs(x)$proj4string
  
  tabs <- spbabel::map_table(x)
 # tabs <- sc::PRIMITIVE(x)
  return(tabs)
  ll <- vector("list", nrow(tabs$object))
  for (i_obj in seq(nrow(tabs$object))) {
    tabs_i <- tabs
    tabs_i$object <- tabs_i$object[i_obj, ]
    
    tabs_i <- semi_cascade(tabs_i, tables = attr(tabs_i, "join_ramp"))
    tt_i <- tri_mesh_map_table1(tabs_i, max_area = max_area)
    ll[[i_obj]] <- tt_i
  }
  outlist <- vector("list", length(ll[[1]]))
  nms <- names(ll[[1]])
  names(outlist) <- nms
  for (i in seq_along(outlist)) {
    outlist[[i]] <- dplyr::bind_rows(lapply(ll, "[[", nms[i]))
  }
  allverts <- dplyr::inner_join(outlist$tXv, outlist$v, "vertex_")
  allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, 
                                            sep = "_")))
  allverts$vertex_ <- spbabel:::id_n(length(unique(allverts$uvert)))[allverts$uvert]
  outlist$tXv <- allverts[, c("triangle_", "vertex_")]
  outlist$v <- dplyr::distinct_(allverts, "vertex_", .keep_all = TRUE)[, 
                                                                       c("x_", "y_", "vertex_")]
  outlist$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", 
                                 ctime = format(Sys.time(), tz = "UTC"))
  class(outlist) <- "trimesh"
  outlist
}
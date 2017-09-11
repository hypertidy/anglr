#' @importFrom spbabel map_table
#' @export
rangl.sf <- function (x, max_area = NULL, ...) 
{
  pr4 <- attr(x[[attr(x, "sf_column")]], "crs")[["proj4string"]]
  #tabs <- spbabel::map_table(x)
  tabs <- silicate::PATH(x)
  names(tabs) <- c("o", "b", "v", "bXv")
  tabs[["o"]] <- dplyr::rename(tabs[["o"]], object_  = object)
  tabs[["b"]] <- dplyr::rename(tabs[["b"]], object_ = object, branch_ = path)
  #tabs[["b"]] <- dplyr::mutate(tabs[["b"]], island_ = subobject < 2)
  ## good grief, split order is a nightmare
  if (tabs[["b"]]$type[1] == "MULTIPOLYGON") tabs[["b"]][["island_"]] <- unlist(lapply(split(tabs[["b"]], tabs[["b"]][["object_"]]), function(xa) !duplicated(xa[["subobject"]]))[unique(tabs[["b"]][["object_"]])])
  if (tabs[["b"]]$type[1] == "POLYGON") tabs[["b"]][["island_"]] <- !duplicated(tabs[["b"]][["object_"]])
  tabs[["bXv"]] <- dplyr::rename(tabs[["bXv"]], branch_ = path)
  ll <- vector("list", nrow(tabs$o))
  for (i_obj in seq(nrow(tabs[["o"]]))) {
    tabs_i <- tabs
    tabs_i[["o"]] <- tabs_i[["o"]][i_obj, ]
    
    tabs_i <- semi_cascade(tabs_i)
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
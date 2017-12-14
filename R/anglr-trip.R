#' @name anglr
#' @export
anglr.trip <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
    ## this is just a copy of the lines version for now
    ## next step is to put all the attributes on the v table
    pr4 <- proj4string(x)
    tabs <- spbabel::map_table(x)
    ll <- vector("list", nrow(tabs$o))
    for (i_obj in seq(nrow(tabs$o))) {
      tabs_i <- tabs; tabs_i$o <- tabs_i$o[i_obj, ]
      tabs_i <- semi_cascade(tabs_i)
      tt_i <- line_mesh_map_table1(tabs_i)
      # plot.trimesh(tt_i)
      # scan("", 1L)
      # rgl::rgl.clear()
      ll[[i_obj]] <- tt_i
    }
    
    outlist <- vector("list", length(ll[[1]]))
    nms <- names(ll[[1]])
    names(outlist) <- nms
    for (i in seq_along(outlist)) {
      outlist[[i]] <- dplyr::bind_rows(lapply(ll, "[[", nms[i]))
    }
    
    ## renormalize the vertices
    allverts <- dplyr::inner_join(outlist$lXv, outlist$v, "vertex_")
    allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, sep = "_")))
    allverts$vertex_ <- spbabel:::id_n(length(unique(allverts$uvert)))[allverts$uvert]
    outlist$lXv <- allverts[, c("segment_", "vertex_")]
    outlist$v <- dplyr::distinct_(allverts, "x_", "y_", "vertex_")
    ## finally add longitude and latitude
    outlist$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", ctime = format(Sys.time(), tz = "UTC"))
    class(outlist) <- "linemesh"
    outlist
  }
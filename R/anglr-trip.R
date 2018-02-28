#' @name anglr
#' @export
anglr.trip <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
  x <- silicate::PATH(x)
  ## denormalize
  
  if (!is.null(z)) {
    x$vertex$z_ <- 0
  v <- x$path_link_vertex %>% inner_join(x$vertex)
  
  if (inherits(z, "BasicRaster")) {
    xy <- as.matrix(v[c("x_", "y_")])
    if (!grepl("longlat", x$meta$proj)) xy <- rgdal::project(xy, x$meta$proj, inv = TRUE)
    v$z_ <- raster::extract(z, xy)
  } 
  if (is.character(z)) {
    if (z %in% names(v)) {
      message(sprintf("using vertex %s as z_", z))
      v$z_ <- v[[z]]
    } else if (z %in% names(x$object)){
      message("object z_ not supported yet")
      
    }
    
  }
  ## renormalize
  vv <- unjoin::unjoin(v, x_, y_, z_)
  vertex <- vv$.idx0
  path_link_vertex <- vv$data
  vertex$vertex_ <- silicate::sc_uid(nrow(vertex))
  path_link_vertex$vertex_ <- vertex$vertex_[match(path_link_vertex$.idx0, vertex$.idx0)]
  vertex$.idx0 <- NULL
  path_link_vertex$.idx0 <- NULL
  x$vertex <- vertex
  x$path_link_vertex <- path_link_vertex
  }
  x$path$type <- "line"
  
  anglr(x)
}
anglr_oldtrip <- function (x, z = NULL, ..., type = NULL, max_area = NULL) {
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



line_mesh_map_table1 <- function(tabs) {
  tabs$v$countingIndex <- seq_len(nrow(tabs$v))
  nonuq <- dplyr::inner_join(tabs$bXv, tabs$v, "vertex_")

  tabs$b$object_id <- as.integer(factor(tabs$b$object_))
  nonuq <- dplyr::inner_join(nonuq, tabs$b, "branch_")

  S = do.call(rbind, lapply(split(nonuq, nonuq$branch_),
                            function(x) cbind(path2seg(x$countingIndex), x$object_id[1])))
  tabs$v$countingIndex <- NULL
  tabs$v$vertex_ <- silicate::sc_uid(nrow(tabs$v))
  #tabs$v <- tibble::tibble(x_ = pl$P[,1], y_ = pl$P[,2], vertex_ = spbabel:::id_n(nrow(pl$P)))

  tabs$b <- tabs$bXv <- NULL
  #tabs$l <- tibble::tibble(segment_ = spbabel:::id_n(nrow(pl$S)), object_ = tabs$o$object_[1])
  tabs$l <- tibble::tibble(segment_ = silicate::sc_uid(nrow(S)), object_ = tabs$o$object_[S[,3]])

  tabs$lXv <- tibble::tibble(segment_ = rep(tabs$l$segment_, each = 2),
                             vertex_ = tabs$v$vertex_[as.vector(t(S[,1:2]))])

  tabs
}
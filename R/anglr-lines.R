anglr_lines <- function(tabs,   ...) {
  tabs$vertex$countingIndex <- seq_len(nrow(tabs$vertex))
  nonuq <- dplyr::inner_join(tabs$path_link_vertex, tabs$vertex, "vertex_")
  
  tabs$path$object_id <- as.integer(factor(tabs$path$object_))
  nonuq <- dplyr::inner_join(nonuq, tabs$path, "path_")
  
  S = do.call(rbind, lapply(split(nonuq, nonuq$path_),
                            function(x) cbind(path2seg(x$countingIndex), x$object_id[1])))
  tabs$vertex$countingIndex <- NULL
  tabs$vertex$vertex_ <- silicate::sc_uid(nrow(tabs$vertex))
  tabs$l <- tibble::tibble(segment_ = silicate::sc_uid(nrow(S)), object_ = tabs$o$object_[S[,3]])
  
  tabs$lXv <- tibble::tibble(segment_ = rep(tabs$l$segment_, each = 2), 
                             vertex_ = tabs$v$vertex_[as.vector(t(S[,1:2]))])
  
  ## renormalize the vertices
  allverts <- dplyr::inner_join(tabs$lXv, tabs$vertex, "vertex_")
  #browser()
  allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, sep = "_")))
  allverts$vertex_ <- silicate::sc_uid(length(unique(allverts$uvert)))[allverts$uvert]
  tabs$lXv <- allverts[, c("segment_", "vertex_")]
  tabs$v <- dplyr::distinct(allverts, .data$uvert, .keep_all = TRUE)
  tabs$o <- tabs$object
  tabs$object <- tabs$path <- tabs$vertex <- tabs$path_link_vertex <- NULL
  class(tabs) <- "linemesh"
  tabs
}


#anglr_lines <- function(tabs, ...) {
  # outlist <- line_mesh_map_table1(tabs)
  # ## renormalize the vertices
  # allverts <- dplyr::inner_join(outlist$lXv, outlist$v, "vertex_")
  # #browser()
  # allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, sep = "_")))
  # allverts$vertex_ <- spbabel:::id_n(length(unique(allverts$uvert)))[allverts$uvert]
  # outlist$lXv <- allverts[, c("segment_", "vertex_")]
  # outlist$v <- dplyr::distinct(allverts, .data$uvert, .keep_all = TRUE)
  # class(outlist) <- "linemesh"
  # outlist
#}




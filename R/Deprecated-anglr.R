anglr_lines <- function(tabs,   ...) {
  .Deprecated(new = "silicate::SC or silicate::TRI, and plot3d.SC", 
              package = "anglr", 
              old = "anglr(<line-topology>)")
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

  tabs <- tabs[c("o", "l", "lXv", "v", "meta")]
  attr(tabs, "join_ramp") <- c("o", "l", "lXv", "v")
  class(tabs) <- "linemesh"
  tabs
}

#' @importFrom silicate PATH
#' @importFrom rlang .data
#' @export
anglr.PATH <- function(x, z = NULL, ..., type = NULL, max_area = NULL) {
  .Deprecated("use silicate::SC(x) or silicate::TRI(x)", "anglr", old = "anglr(x)")
  pr4 <- get_proj(x)
  tabs <- x
  tabs$meta <- tibble::tibble(proj = pr4, ctime = format(Sys.time(), tz = "UTC"))
  thetype <- tabs[["path"]]$type[1]
  if (grepl("POLYGON", thetype, ignore.case = TRUE)) {
    out <- pfft_polys(tabs, ..., max_area = max_area)
  }
  if (grepl("LINE", thetype, ignore.case = TRUE)) {
    out <- anglr_lines(tabs)
  }
  
  if (inherits(z, "BasicRaster")) {
    ee <- raster::extract(z, as.matrix(out$v[, c("x_", "y_")]), method = "bilinear")
    rproj <- get_proj(z)
    vproj <- get_proj(out)
    if (is.na(rproj) || is.na(vproj) || !vproj == rproj) {
      warning(sprintf("model projection: %s\n\nz projection: %s", vproj, rproj))
    }
    if (all(is.na(ee))) warning("all raster values NA")
    out$v$z_ <- ee
    
    z <- NULL
  }
  if (!is.null(z)) {
    ## this line/poly thing is a mess, need to use SC as general case
    if (grepl("POLYGON", thetype, ignore.case = TRUE)) {
      v <- out$tXv %>% dplyr::inner_join(out$t) %>% 
        dplyr::inner_join(out$o %>% dplyr::select(.data$object_, z), "object_") %>% 
        dplyr::inner_join(out$v %>% dplyr::select(.data$vertex_, .data$x_, .data$y_)) %>% 
        dplyr::select(.data$x_, .data$y_, z, .data$vertex_, .data$triangle_)
      
      names(v)[names(v) == z] <- "z_"
      names(v)[names(v) == "vertex_"] <- "old"
      
      gp <- dplyr::group_indices(v,  .data$x_, .data$y_, .data$z_)
      v$vertex_ <- silicate::sc_uid(length(unique(gp)))[gp]
      tXv <- v %>% dplyr::select(.data$vertex_, .data$triangle_)
      out$tXv <- tXv
      v$old <- NULL
    } else {
      v <- out$lXv %>% dplyr::inner_join(out$l) %>% 
        dplyr::inner_join(out$o %>% dplyr::select(.data$object_, z), "object_") %>% 
        dplyr::inner_join(out$v %>% dplyr::select(.data$vertex_, .data$x_, .data$y_)) %>% 
        dplyr::select(.data$x_, .data$y_, z, .data$vertex_, .data$segment_)
      
      names(v)[names(v) == z] <- "z_"
      names(v)[names(v) == "vertex_"] <- "old"
      
      gp <- dplyr::group_indices(v,  .data$x_, .data$y_, .data$z_)
      v$vertex_ <- silicate::sc_uid(length(unique(gp)))[gp]
      lXv <- v %>% dplyr::select(.data$vertex_, .data$segment_)
      out$lXv <- lXv
      v$old <- NULL
    }
    
    out$v <- dplyr::distinct(v, .data$x_, .data$y_, .data$z_, .data$vertex_)
  }
  out
}



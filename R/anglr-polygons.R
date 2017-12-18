#' 
#' 
#' ## this could replace tri_mesh_map_table1
#' ## by input of a simpler object, not so many tables
#' #tri_mesh_PRIMITIVE <- function(x, max_area = NULL) {}
#' 
#' 
#' ## this internal function does the decomposition to primitives of a 
#' ##  single Spatial object, i.e. a "multipolygon"
#' ## we need to do it one object at a time otherwise keeping track
#' ## of the input versus add vertices is harder (but maybe possible later)
#' #' @importFrom dplyr row_number inner_join
#' #' @importFrom RTriangle pslg triangulate
#' tri_mesh_map_table1 <- function(tabs, max_area = NULL) {
#'   ## the row index of the vertices
#'   ## we need this in the triangulation
#'   tabs$v$countingIndex <- seq(nrow(tabs$v))
#'   ## join the vertex-instances to the vertices table
#'   ## so, i.e. expand out the duplicated x/y coordinates
#'   nonuq <- dplyr::inner_join(tabs$bXv, tabs$v, "vertex_")
#'   
#'   ## create Triangle's Planar Straight Line Graph
#'   ## which is an index matrix S of every vertex pair P
#'   ps <- RTriangle::pslg(P = as.matrix(tabs$v[, c("x_", "y_")]),
#'                         S = do.call(rbind, lapply(split(nonuq, nonuq$branch_),
#'                                                   function(x) path2seg(x$countingIndex))))
#'   
#'   ## build the triangulation, with input max_area (defaults to NULL)
#'   tr <- RTriangle::triangulate(ps, a = max_area)
#'   
#'  #  ## NOTE: the following only checks for presence of triangle centres within
#'  #  ## known holes, so this doesn't pick up examples of overlapping areas e.g. 
#'  #  ## https://github.com/hypertidy/anglr/issues/39
#'  #  ## centroid of every triangle
#'  #  
#'  #  ## process the holes if present
#'  #  if ("island_" %in% names(tabs$b) && any(!tabs$b$island_)) {
#'  #    ## filter out all the hole geometry and build an sp polygon object with it
#'  #    ## this 
#'  #    ##   filters all the branches that are holes
#'  #    ##   joins on the vertex instance index
#'  #    ##   joins on the vertex values
#'  #    ##   recomposes a SpatialPolygonsDataFrame using the spbabel::sp convention
#'  #    holes <- dplyr::inner_join(dplyr::inner_join(dplyr::filter_(tabs$b, quote(!island_)), tabs$bXv, "branch_"), 
#'  #                                           tabs$v, "vertex_")
#'  #    holes[["order_"]] <- seq_len(nrow(holes))
#'  #    holes <- spbabel::sp(holes)
#'  #    
#'  #    stopifnot(inherits(holes, "SpatialPolygons"))
#'  #    ## centroid of every triangle
#'  #    centroids <- matrix(unlist(lapply(split(tr$P[t(tr$T), ], rep(seq(nrow(tr$T)), each = 3)), .colMeans, 3, 2)), 
#'  #                        ncol = 2, byrow = TRUE)
#'  #    ## sp::over() is very efficient, but has to use high-level objects as input
#'  #    badtris <- !is.na(over(SpatialPoints(centroids), sp::geometry(holes)))
#'  #    ## presumably this will always be true inside this block (but should check some time)
#'  #    if (any(badtris)) tr$T <- tr$T[!badtris, ]
#'  #  }
#'  # 
#'  #  centroids <- matrix(unlist(lapply(split(tr$P[t(tr$T), ], rep(seq(nrow(tr$T)), each = 3)), .colMeans, 3, 2)), 
#'  #                      ncol = 2, byrow = TRUE)
#'  #  
#'  #  ## actually we are missing concavities of some kinds, so hole removal is not enough
#'  #  polys <- dplyr::inner_join(dplyr::inner_join(tabs$b, tabs$bXv, "branch_"), 
#'  #                             tabs$v, "vertex_")
#'  #  polys[["order_"]] <- seq_len(nrow(polys))
#'  #  polys <- spbabel::sp(as.data.frame(polys))## duh avoid sp seppuku
#'  # # holes <- spbabel::sp(holes)
#'  #   
#'  #  ## sp::over() is very efficient, but has to use high-level objects as input
#'  #  badtris <- is.na(over(SpatialPoints(centroids), sp::geometry(polys)))
#'  #  ## presumably this will always be true inside this block (but should check some time)
#'  #  if (any(badtris)) tr$T <- tr$T[!badtris, ]
#'  #  
#'  #  
#'   
#'   
#'    
#'   ## trace and remove any unused triangles
#'   ## the raw vertices with a unique vertex_ id
#'   tabs$v <- tibble::tibble(x_ = tr$P[,1], y_ = tr$P[,2], vertex_ = spbabel:::id_n(nrow(tr$P)))
#'   ## drop the path topology
#'   tabs$b <- tabs$bXv <- NULL
#'   ## add triangle topology
#'   tabs$t <- tibble::tibble(triangle_ = spbabel:::id_n(nrow(tr$T)), object_ = tabs$o$object_[1])
#'   tabs$tXv <- tibble::tibble(triangle_ = rep(tabs$t$triangle_, each = 3), 
#'                              vertex_ = tabs$v$vertex_[as.vector(t(tr$T))])
#'   
#'   tabs
#' }
#' 
#' 
#' anglr_polys <-  function(tabs, max_area = NULL, ...){
#'   ll <- vector("list", nrow(tabs$o))
#'   for (i_obj in seq(nrow(tabs[["o"]]))) {
#'     tabs_i <- tabs
#'     tabs_i[["o"]] <- tabs_i[["o"]][i_obj, ]
#'     
#'     tabs_i <- semi_cascade(tabs_i)
#'     tt_i <- tri_mesh_map_table1(tabs_i, max_area = max_area)
#'     ll[[i_obj]] <- tt_i
#'   }
#'   outlist <- vector("list", length(ll[[1]]))
#'   nms <- names(ll[[1]])
#'   names(outlist) <- nms
#'   for (i in seq_along(outlist)) {
#'     outlist[[i]] <- dplyr::bind_rows(lapply(ll, "[[", nms[i]))
#'   }
#'   allverts <- dplyr::inner_join(outlist$tXv, outlist$v, "vertex_")
#'   allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, 
#'                                             sep = "_")))
#'   allverts$vertex_ <- spbabel:::id_n(length(unique(allverts$uvert)))[allverts$uvert]
#'   outlist$tXv <- allverts[, c("triangle_", "vertex_")]
#'   outlist$v <- dplyr::distinct_(allverts, "vertex_", .keep_all = TRUE)[, 
#'                                                                        c("x_", "y_", "vertex_")]
#'  class(outlist) <- "trimesh"
#'   outlist
#'   
#' }


#' @param x PATH
pfft_polys <- function(x, max_area = NULL,  ...) {
  dots <- list(...)
  dots[["a"]] <- max_area
  dots[["x"]] <- x
  RTri <- do.call(pfft::edge_RTriangle, dots)
  ptm <- pfft::path_triangle_map(x, RTri)
  
  triangle <- tibble::tibble(triangle_ = silicate::sc_uid(nrow(RTri$T)), 
                             triangle_idx = 1:nrow(RTri$T))
  ptm[["triangle_"]] <- triangle[["triangle_"]][ptm[["triangle_idx"]]]
  ptm <- dplyr::inner_join(ptm, x[["path"]][c("path_", "object_", "subobject")])
  #> Joining, by = "path_"
  ptm <- ptm %>% 
    dplyr::group_by(.data$object_,  .data$subobject, .data$triangle_idx) %>% 
    dplyr::mutate(n = n())  %>% dplyr::ungroup()  %>% 
    dplyr::filter(.data$n < 2)
  
  
  o <- x[["object"]]
  tt <- ptm %>%    
    dplyr::select(.data$object_, .data$triangle_)
  remove_idx <-  setdiff(1:nrow(RTri$T), ptm$triangle_idx)
  ptm <- ptm %>% dplyr::arrange(.data$triangle_idx)
  TT <- if (length(remove_idx) > 0) RTri$T[-remove_idx, ] else RTri$T
  
  v <- tibble::tibble(x_ = RTri$P[,1], y_ = RTri$P[,2], vertex_ = silicate::sc_uid(nrow(RTri$P)))
  tXv <- tibble::tibble(vertex_ = v[["vertex_"]][t(TT)], 
                        triangle_ = rep(ptm[["triangle_"]], each = 3))
  
  #v <- x[["vertex"]]
  outlist <- list(o = o, t = tt, tXv = tXv, v = v)
  class(outlist) <- "trimesh"
  outlist
}

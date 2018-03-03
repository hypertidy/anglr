## we need this else
#Error in mutate_impl(.data, dots) :
#Evaluation error: This function should not be called directly.
#https://github.com/hypertidy/anglr/issues/64
globalVariables("n")


#' @noRd
#' @param x PATH
pfft_polys <- function(x, max_area = NULL,  ...) {
  dots <- list(...)
  dots[["a"]] <- max_area
  dots[["x"]] <- x

  RTri <- do.call(pfft::edge_RTriangle, dots)
  ptm <- pfft::path_triangle_map(x, RTri)
  #vertex <- x$v  ## for fun aRt
  vertex <- tibble::tibble(x_ = RTri$P[,1], y_ = RTri$P[,2], vertex_ = silicate::sc_uid(nrow(RTri$P)))
  ## unique triangles
  triangle <- tibble::tibble(triangle_ = silicate::sc_uid(nrow(RTri$T)))
  ## all triangle instances
  ptm[["triangle_"]] <- triangle[["triangle_"]][ptm[["triangle_idx"]]]
  ptm[["triangle_idx"]] <- NULL
  ## any triangle that occurs an even number of times in a path per object is part of a hole

  ptm <- dplyr::inner_join(ptm, x[["path"]][c("path_", "object_")], "path_")
  
 
  ptm <- ptm %>% dplyr::group_by(.data$object_, .data$triangle_) %>% 
    dplyr::mutate(n = n()) %>%  ## see globalVariables declaration for "n"
    dplyr::ungroup()  #%>% 
  
  tt <- dplyr::select(ptm, .data$object_, .data$triangle_) %>% 
    dplyr::anti_join(ptm %>% dplyr::filter(.data$n %% 2 == 0) %>% 
                       dplyr::select(.data$object_, .data$triangle_), c("object_", "triangle_"))
  tXv <- tibble::tibble(vertex_ = vertex[["vertex_"]][t(RTri$T)], 
                        triangle_ = rep(triangle[["triangle_"]], each = 3))
  
  
  outlist <- list(o = x$o, t = tt, tXv = tXv, v = vertex, 
                  meta = x$meta)
  class(outlist) <- "trimesh"
  outlist
}

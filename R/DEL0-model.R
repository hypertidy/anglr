
DEL0 <- function(x, ..., max_area = NULL) {
  UseMethod("DEL0")
}


DEL0.PATH0 <- function(x, ..., max_area = NULL) {
  dots <- list(...)
  dots[["a"]] <- max_area
  dots[["x"]] <- x
  
  ## TRIANGULATE with PATH-identity  
  RTri <- do.call(edge_RTriangle0, dots)
  ## object/path_link_triangle (path_triangle_map)
  ptm <- path_triangle_map(x, RTri) 
  omap <- dplyr::bind_rows(x$object$path_)%>% dplyr::distinct(.data$object_, .data$path_) 
  ptm$object_ <- omap$object_[match(ptm$path_, omap$path_)]
  ptm <- ptm %>% dplyr::group_by(.data$object_, .data$triangle_idx) %>% 
    dplyr::filter(!(dplyr::n() %% 2 == 0)) %>% dplyr::ungroup()
  path <-  silicate::sc_path(x)
  vertex <- tibble::tibble(x_ = RTri$P[,1], y_ = RTri$P[,2]) 

  tridf <- setNames(as.data.frame(RTri$T), c(".vx0", ".vx1", ".vx2"))[ptm$triangle_idx, ]
  tridf$path_ <- as.integer(ptm$path_)
  tridf$object_ <- as.integer(ptm$object_)
 topology <- split(tridf, tridf$object_)
  meta <- tibble(proj = get_proj(x), ctime = Sys.time())
  object <- x$object
  object$topology_ <- topology
  structure(list(object = object,
                 vertex = vertex, 
                 meta = meta), class = c("DEL0", "TRI0", "sc"))
  
}

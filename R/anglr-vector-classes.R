## anglr generic will have a "type" - surface or segment
## use this to specify or override the inferred type, and that calls anglr_lines or anglr_polys

## generic will
## get_proj


get_proj <- function(x, ...) UseMethod("get_proj")
get_proj.default <- function(x, ...) {
  raster::projection(x)
}
get_proj.sf <- function(x, ...) {
  attr(x[[attr(x, "sf_column")]], "crs")[["proj4string"]]
}
get_proj.sfc <- function(x, ...) {
  attr(x, "crs")[["proj4string"]]
}
get_proj.PATH <- function(x, ...) {
  NA_character_  ## silicate needs to record this
}
#' @importFrom silicate PATH
#' @export

anglr.sf <- function(x, z = NULL, ..., type = NULL, max_area = NULL) {
  pr4 <- get_proj(x)
  tabs <- silicate_to_gris_names(silicate::PATH(x))
  tabs$meta <- tibble::tibble(proj = pr4, ctime = format(Sys.time(), tz = "UTC"))
  thetype <- tabs[["b"]]$type[1]
  if (!is.null(type)) thetype <- type
  if (grepl("POLYGON", thetype)) {
    out <- anglr_polys(tabs, ..., max_area = max_area)
    PP <- as.matrix(out$v[c("x_", "y_")])
    TT <- matrix(match(out$tXv$vertex_, out$v$vertex_), ncol = 3, byrow = TRUE)
    centroids <- matrix(unlist(lapply(split(PP[t(TT), ], rep(seq(nrow(TT)), each = 3)), .colMeans, 3, 2)), 
                          ncol = 2, byrow = TRUE)
    #badtris <- is.na(sp::over(SpatialPoints(centroids), sp::geometry(holes)))
    a <- purrr::map_int(sf::st_intersects(sf::st_as_sf(setNames(as.data.frame(centroids), c("x", "y")), coords = c("x", "y")), 
                      sf::st_set_crs(x, NA)), length)
    
    
    badtris <- which(a == 0)
    if (length(badtris) > 0) {
      out$t <- out$t[-badtris, ]
      out$tXv <- out$tXv %>% dplyr::inner_join(out$t, "triangle_") %>% dplyr::select(.data$triangle_, .data$vertex_)
    }
    if (inherits(z, "BasicRaster")) {
      ee <- raster::extract(z, as.matrix(out$v[, c("x_", "y_")]), method = "bilinear")
      if (all(is.na(ee))) warning("all raster values NA, mixed projections not supported yet")
      out$v$z_ <- ee
      
      z <- NULL
    }
    if (!is.null(z)) {
      
      v <- out$tXv %>% dplyr::inner_join(out$t) %>% 
        dplyr::inner_join(out$o %>% dplyr::select(.data$object_, z), "object_") %>% 
        dplyr::inner_join(out$v) %>% 
        dplyr::select(.data$x_, .data$y_, z, .data$vertex_, .data$triangle_)
      names(v)[names(v) == z] <- "z_"
      names(v)[names(v) == "vertex_"] <- "old"
    
      gp <- dplyr::group_indices(v,  .data$x_, .data$y_, .data$z_)
      v$vertex_ <- silicate::sc_uid(length(unique(gp)))[gp]
      tXv <- v %>% dplyr::select(.data$vertex_, .data$triangle_)
      out$tXv <- tXv
      v$old <- NULL
      out$v <- dplyr::distinct(v, .data$x_, .data$y_, .data$z_, .data$vertex_)
    }
    return(out)
  }
  if (grepl("LINE", thetype)) {
    out <- anglr_lines(tabs)
    if (inherits(z, "BasicRaster")) {
      ee <- raster::extract(z, as.matrix(out$v[, c("x_", "y_")]), method = "bilinear")
      if (all(is.na(ee))) warning("all raster values NA, mixed projections not supported yet")
      out$v$z_ <- ee
      
      z <- NULL
    }
    if (!is.null(z)) {
      
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
      out$v <- dplyr::distinct(v, .data$x_, .data$y_, .data$z_, .data$vertex_)
    }
    return(out)
  }
  
  tabs
}
#' @export
anglr.PATH <- function(x, z = NULL, ..., type = NULL, max_area = NULL) {
  tabs <- silicate_to_gris_names(x)
  pr4 <- get_proj(x)
  tabs$meta <- tibble::tibble(proj = pr4, ctime = format(Sys.time(), tz = "UTC"))
  thetype <- tabs[["b"]]$type[1]
  if (grepl("POLYGON", thetype)) {
    return(anglr_polys(tabs, ..., max_area = max_area))
  }
  if (grepl("LINE", thetype)) {
    return(anglr_lines(tabs))
  }
  tabs
  ## could be NULL
  #stop("woah, no type in this PATH - todo")
}

#' @rdname anglr
#' @importFrom dplyr %>%  arrange distinct mutate
#' @export
anglr.SpatialLines <- function(x, z = NULL, ..., type = NULL, max_area = NULL) {
  return(anglr(sf::st_as_sf(x), z = z, type = type, max_area = max_area, ...))
  # pr4 <- proj4string(x)
  # if (! "data" %in% slotNames(x)) {
  #   dummy <- data.frame(row_number = seq_along(x))
  #   x <- sp::SpatialLinesDataFrame(x, dummy, match.ID = FALSE)
  # }
  # tabs <- spbabel::map_table(x)
  # out <- anglr_lines(tabs)
  # #tabs <- silicate::PATH(x)
  # #tabs <- silicate_to_gris_names(tabs)
  # out$meta <- tibble::tibble(proj = pr4,
  #                            ctime = format(Sys.time(), tz = "UTC"))
  # out
}



#' @rdname anglr
#' @export
#' @importFrom sp geometry  over SpatialPoints proj4string CRS SpatialPolygonsDataFrame
#' @importFrom dplyr inner_join
#' @importFrom RTriangle pslg triangulate
#' @importFrom spbabel map_table
#' @importFrom tibble tibble
#' @importFrom methods slotNames
anglr.SpatialPolygons <- function(x, z = NULL, ..., type = NULL, max_area = NULL) {
  return(anglr(sf::st_as_sf(x), z = z, type = type, max_area = max_area, ...))
  # pr4 <- proj4string(x)
  # x0 <- x
  # ## kludge for non DataFrames
  # if (! "data" %in% slotNames(x)) {
  #   dummy <- data.frame(row_number = seq_along(x))
  #   x <- sp::SpatialPolygonsDataFrame(x, dummy, match.ID = FALSE)
  # }
  # tabs <- spbabel::map_table(x)
  # out <- anglr_polys(tabs, max_area = max_area, ...)
  # out$meta <- tibble::tibble(proj = pr4,
  #                            ctime = format(Sys.time(), tz = "UTC"))
  # out
}


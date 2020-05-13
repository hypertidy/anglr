.add_zmt_dots <- function(vv, dots)  {
  for (i in seq_along(names(vv))) {
    if (!names(vv)[i] %in% c("x_", "y_", "z_", "m_", "t_")) {
      vv[[names(vv)[i]]] <- NULL
    }
  }
  #  vv <- vv %>% dplyr::distinct(.data$x_, .data$y_, .keep_all = TRUE)  ## we can't keep unique z, so for the guiding principles this needs mention
  vv <- vv[!duplicated(vv[c("x_", "y_")]), ]
  dots$p <- RTriangle::pslg(as.matrix(vv[c("x_", "y_")]))
  cnames <- c()
  if ("z_" %in% names(vv)) {
    ## better document this, pretty powerful
    dots$p$PA <- cbind(NULL, z_ = vv$z_)
    cnames <- c(cnames, "z_")
  }
  if ("m_" %in% names(vv)) {
    ## better document this, pretty powerful
    dots$p$PA <- cbind(dots$p$PA, m_ = vv$m_)
    cnames <- c(cnames, "m_")
  }
  if ("t_" %in% names(vv)) {
    ## better document this, pretty powerful
    dots$p$PA <- cbind(dots$p$PA, t_ = vv$t_)
    cnames <- c(cnames, "t_")
  }
  list(vv = vv, dots = dots, cnames = cnames)
}

#' Convert object to a constrained-Delaunay triangulation
#'
#' This *structural-form* Delaunay-based triangulation model is analogous to the
#' [TRI()][silicate::TRI] model in the silicate package and formally extends the
#' class of that model. A primitives-based shape-constrained triangulation. The
#' Delaunay model is the *mostly Delaunay* scheme used by the provable-quality
#' meshers.
#'
#' This is a more compact form of the *relational-form* [DEL()] model.
#'
#' @section Topology:
#'
#' Note that for explicitly linear features, these still use a post-meshing
#' identification for which triangles belong in which feature. This can't make
#' sense for many line layers, but we leave it for now.
#'
#' For point features, the mesher unproblematically creates a triangulation in
#' the convex hull of the points, any attributes names `z_`, `m_`, or `t_` are
#' automatically interpolated and include in the output. See the help for
#' [RTriangle::triangulate()] for how this works via the `$PA` element.
#'
#' Note that for a raster input the terrainmeshr package is used to determine
#' a sensible number of triangles based on local curvature. To avoid creating
#' this adative mesh and use `as.mesh3d(QUAD(raster))` to get quad primitives or
#' `as.mesh3d(QUAD(raster), triangles = TRUE)` to get triangle primitives directly
#' from raster cells.
#' @param x object of class [PATH0] or understood by [PATH0()]
#' @param ... ignored
#' @inheritParams DEL
#' @param max_triangles limit on triangles to create, passed to terrainmeshr
#' @return [DEL0 class][DEL0]
#' @export
#'
#' @inheritSection anglr-package Licensing
#'
#' @seealso [DEL]
#' @examples
#' a <- DEL0(cad_tas)
#' plot(a)
#'
#' ## ---- intepolate via triangulation, sample points from volcano
#' rgl::clear3d()
#' n <- 150
#' max_area <- .005 ## we working in x 0,1 y 0,1
#' library(anglr)
#' library(dplyr)
#' d <-
#'   data.frame(x = runif(n), y = runif(n), multipoint_id = 1) %>%
#'   dplyr::mutate(
#'     z = raster::extract(raster::raster(volcano), cbind(x, y)),
#'     multipoint_id = 1
#'   )
#' \donttest{
#' mesh <- DEL0(
#'   sfheaders::sf_multipoint(d, x = "x", y = "y", z = "z",
#'     multipoint_id = "multipoint_id"), max_area = max_area)
#'
#' plot3d(mesh , color = "darkgrey", specular = "darkgrey") #sample(grey.colors(5)))
#' }
DEL0 <- function(x, ..., max_area = NULL) {
  UseMethod("DEL0")
}
#' @name DEL0
#' @export
DEL0.DEL <- function(x, ..., max_area = NULL) {
  ## must redo this stuff to use DEL0 as the basis
  if (!is.null(max_area)) {
    warning("'max_area' ignored, cannot currently re-mesh a DEL or DEL0")
  }
  object <- silicate::sc_object(x)
  triangle <- x$triangle
  if ("visible" %in% names(triangle)) {
    ## because DEL(SC()) doesn't have visible
    triangle <- dplyr::filter(triangle, .data$visible)
  }
  topol <- matrix(match(as.matrix(triangle[c(".vx0", ".vx1", ".vx2")]),
                        x$vertex$vertex_), ncol = 3L)
  colnames(topol) <- c(".vx0", ".vx1", ".vx2")
  object$topology_ <- split(tibble::as_tibble(topol), triangle$object_)[unique(triangle$object_)]
  object$object_ <- NULL
  meta <- x$meta
  row <- x$meta[1, ]
  row$ctime <- Sys.time()
  meta <- rbind(row, meta)
  structure(list(object = object,
                 vertex = x$vertex,
                 meta = meta), class = c("DEL0", "TRI0", "sc"))

}
#' @name DEL0
#' @export
DEL0.default <- function(x, ..., max_area = NULL) {
  DEL0(silicate::PATH0(x), ..., max_area = max_area)
}

#' @name DEL0
#' @export
DEL0.SC <- function(x, ..., max_area = NULL) {
  DEL0(DEL(x, max_area = max_area), ...)
}
#' @name DEL0
#' @export
DEL0.SC0 <- function(x, ..., max_area = NULL) {
  DEL0(DEL(x, max_area = max_area), ...)
}

#' @name DEL0
#' @export
DEL0.TRI <- function(x, ..., max_area = NULL) {
  DEL0(DEL(x, max_area = max_area), ...)
}
#' @name DEL0
#' @export
DEL0.TRI0 <- function(x, ..., max_area = NULL) {
  DEL0(TRI(x, max_area = max_area), ...)
}
#' @name DEL0
#' @export
DEL0.ARC <- function(x, ..., max_area = NULL) {
  DEL0(DEL(x, max_area = max_area), ...)
}
#' @name DEL0
#' @export
DEL0.PATH <- function(x, ..., max_area = NULL) {
  DEL(silicate::PATH0(x), max_area = max_area)
}
#' @name DEL0
#' @export
DEL0.PATH0 <- function(x, ..., max_area = NULL) {
  .check_area(x$vertex$x_, x$vertex$y_, max_area)
  dots <- list(...)

  dots[["a"]] <- max_area

  #-------------------------------
  if ( all(unlist(lapply(x$object$path_, function(xa) dim(xa)[1L] == length(unique(xa$path_))), use.names = FALSE))) {
   # print("NO")
    ## bail out with a point-triangulation
    vv <- silicate::sc_vertex(x)

    zmt <- .add_zmt_dots(vv, dots)
    vv <- zmt[["vv"]]
    dots <- zmt[["dots"]]
   cnames <- zmt[["cnames"]]
    tri <- do.call(RTriangle::triangulate, dots)

    object <- tibble::tibble(del0 = 1L,
                             color_ = "#111111FF",
                             path0 = list(dplyr::select(x$object, -.data$path_)),
                             topology_ = list(tibble::tibble(.vx0 = tri$T[,1L,drop = TRUE],
                                                             .vx1 = tri$T[,2L, drop = TRUE],
                                                             .vx2 = tri$T[,3L, drop = TRUE])))
    meta <- tibble(proj = get_proj(x), ctime = Sys.time())

   out <-  structure(list(object = object,
                   vertex = tibble::tibble(x_ = tri$P[,1L, drop = TRUE],
                                           y_ = tri$P[,2L, drop = TRUE]),
                   meta = meta), class = c("DEL0", "TRI0", "sc"))
   cn <- dim(tri$PA)[2L]
   ## must do this above cnames <- colnames(tri$PA)
   if (cn > 0) {
     for (i in seq_len(cn)) {
       out$vertex[[cnames[i]]] <- tri$PA[,i, drop = TRUE]
     }
   }
    return(out)
  }

  vv <- silicate::sc_vertex(x)
  zmt <- .add_zmt_dots(vv, dots)
 # browser()

 vv <- zmt[["vv"]]
  dots <- zmt[["dots"]]
  cnames <- zmt[["cnames"]]
  #----------------
  ## TRIANGULATE with PATH-identity

  ## get the edges (doh!!) https://github.com/hypertidy/anglr/issues/138
  dots$p$S <- do.call(rbind, lapply(SC0(x)$object$topology_, function(a) as.matrix(a)[, c(".vx0", ".vx1"), drop = FALSE]))
  RTri <- do.call(RTriangle::triangulate, dots)
  # x## object/path_link_triangle (path_triangle_map)
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
 topology <- split(tridf, tridf$object_)[unique(tridf$object_)]
 ## really we should just get one object, the pfft stuff is not relevant for lines
 ids <- unique(tridf$object_)
 ## we don't always get triangles for a line
 object <- x$object[ids, ]
 object$topology_ <- topology

  meta <- tibble(proj = get_proj(x), ctime = Sys.time())

  out <- structure(list(object = object,
                 vertex = vertex,
                 meta = meta), class = c("DEL0", "TRI0", "sc"))


  cn <- dim(RTri$PA)[2L]
  ## must do this above cnames <- colnames(tri$PA)
  if (cn > 0) {
    for (i in seq_len(cn)) {
      out$vertex[[cnames[i]]] <- RTri$PA[,i, drop = TRUE]
    }
  }
  out
}


#' @name DEL0
#' @export
#' @importFrom raster as.matrix cellFromRowCol xFromCell yFromCell
DEL0.BasicRaster <- function(x, ..., max_triangles = NULL) {
  ## use [[]] to avoid the as.matrix crazy with RasterBrick
  heightmap <- t(raster::as.matrix(x[[1L]]))[,nrow(x):1]
  if (is.null(max_triangles)) max_triangles <- prod(dim(heightmap))/20
  ## if missing data you have to sentinelize them
  dosentinel <- FALSE
  if (anyNA(heightmap)) {
    dosentinel <- TRUE
    sentinel <- as.integer(min(heightmap, na.rm  = TRUE) -100)
    heightmap[is.na(heightmap)] <- sentinel
  }
  hmm <- terrainmeshr::triangulate_matrix(heightmap, maxTriangles = max_triangles)
  if (dosentinel) {
    #browser()
    hmm <- hmm[hmm[,2] > sentinel, ]
    bad <- table(hmm[,4]) < 3
    if (any(bad)) {
      hmm <- hmm[!hmm[,4] %in% as.integer(names(bad[which(bad)])), ]
    }
  }
  ## remember rgl.surface is x, z, y despite names
  cell <- raster::cellFromRowCol(x,hmm[,"z"], hmm[,"x"])
  xyz <- cbind(x = raster::xFromCell(x, cell),
               y = raster::yFromCell(x, cell),
               z = hmm[, "y"])
  topology <- tibble::tibble(.vx0 = seq(1, dim(hmm)[1], by = 3L),
                             .vx1 = .data$.vx0 + 1L,
                             .vx2 = .data$.vx1 + 1L)
  meta <- tibble::tibble(proj = crsmeta::crs_proj(x), ctime = Sys.time())
  structure(list(object = tibble(a = 1, topology_ = list(topology)),
                 vertex = tibble::tibble(x_ = xyz[,1], y_ = xyz[,2], z_ = xyz[,3]),
                 meta = meta),
            class = c("TRI0", "sc"))
}

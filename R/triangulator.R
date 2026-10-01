#' Triangulation backend
#'
#' anglr builds a planar straight line graph (vertices, segments, optional
#' per-vertex attributes) and hands it to a constrained Delaunay triangulator
#' with refinement. The backend is chosen by `options(anglr.triangulator = )`:
#'
#' - `"RTriangle"` (the default): Shewchuk's Triangle via the RTriangle package.
#' - `"cdtr"`: the artem-ogre/CDT library via the cdtr package (MPL-2.0, no
#'   non-commercial restriction). Honours `max_area` where Triangle may give up,
#'   reports what it could not refine, and classifies triangles topologically.
#'
#' Both return the RTriangle shape anglr consumes: `P` (vertices), `T`
#' (triangle indices), `PA` (per-vertex attributes carried onto Steiner
#' points), and for cdtr also `depth` and `unrefined`.
#'
#' @param dots list of arguments as built for [RTriangle::triangulate()]:
#'   `p` (a pslg with `P`, `S`, `PA`), `a` (max area), `q` (min angle), and
#'   others. Triangle flags with no cdtr equivalent (`Y`, `j`, `V`, `Q`, `D` is
#'   mapped to conforming) are dropped with a message.
#' @param backend "RTriangle" or "cdtr"
#' @return list with `P`, `T`, `PA`, and backend-specific extras
#' @export
#' @examples
#' p <- RTriangle::pslg(P = cbind(c(0, 1, 1, 0), c(0, 0, 1, 1)),
#'                      S = rbind(c(1, 2), c(2, 3), c(3, 4), c(4, 1)))
#' tr <- anglr_triangulate(list(p = p, a = 0.05))
#' nrow(tr$T)
anglr_triangulate <- function(dots, backend = getOption("anglr.triangulator", "RTriangle")) {
  backend <- match.arg(backend, c("RTriangle", "cdtr"))
  if (backend == "RTriangle") {
    return(do.call(RTriangle::triangulate, dots))
  }
  if (!requireNamespace("cdtr", quietly = TRUE)) {
    stop("options(anglr.triangulator = \"cdtr\") needs the cdtr package: remotes::install_github(\"hypertidy/cdtr\")")
  }
  p <- dots[["p"]]
  S <- p[["S"]]
  if (is.null(S) || length(S) == 0L) S <- matrix(integer(0), 0L, 2L)
  q <- dots[["q"]]
  min_angle <- if (isTRUE(q)) 20 else if (is.numeric(q)) q else NULL
  max_area <- dots[["a"]]
  if (!is.null(max_area) && !is.finite(max_area)) max_area <- NULL
  max_steiner <- if (is.numeric(dots[["S"]])) dots[["S"]] else Inf
  unsupported <- intersect(names(dots), c("Y", "j", "V", "Q"))
  if (length(unsupported)) {
    message("triangulate arguments not supported by the cdtr backend, ignored: ",
            paste(unsupported, collapse = ", "))
  }
  r <- cdtr::cdt_triangulate_attr(p[["P"]][, 1L], p[["P"]][, 2L], S[, 1L], S[, 2L],
                                  PA = p[["PA"]],
                                  max_area = max_area, min_angle = min_angle,
                                  max_steiner = max_steiner,
                                  conforming = isTRUE(dots[["D"]]))
  if (is.null(r[["PA"]])) r[["PA"]] <- matrix(0, nrow(r[["P"]]), 0L)
  r
}

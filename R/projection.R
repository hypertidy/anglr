
reproject <- function(x, target = NULL, ..., source) {
   UseMethod("reproject")   
}

reproject.sc <- function(x, target = NULL, ..., family = "laea", source) {
  if (is.null(target)) {
    target <- make_local(x, family = family)
  }
  source <- get_proj(x)
  fac <- if (grepl("longlat", source)) pi/180 else 1
  z <- if ("z_" %in% names(x$vertex)) z<- x$vertex[["z_"]] else 0
  verts <- cbind(as.matrix(x$vertex[c("x_", "y_")]), 0)
  
  x$vertex[c("x_", "y_", "z_")] <- tibble::as_tibble(proj4::ptransform(verts * fac, 
                                               src.proj = source, 
                                               dst.proj = target))
  x
                                                  
}
defaults <- function(x) {
  x[intersect(c("proj", "lon_0", "lat_0", "datum"), names(x))]
}
tokenize <- function(x) {
  l <- strsplit(gsub("^\\+", "", unlist(strsplit(x, " "))), "=")
  nc <- lengths(l)
  if (any(nc < 2)) {
    for (i in which(nc < 2)) l[[i]] <- c(l[[i]], "")
  }
  m <- matrix(unlist(l), 2)
  setNames(as.list(m[2, ]), m[1, ])
  
}
make_local <- function(x, family = "laea") {
  UseMethod("make_local")
}
make_local.sc <- function(x, family = "laea", ...) {
  proj <- get_proj(x)
  if (grepl("longlat", proj)) {
    xy <- as.matrix(x$vertex[c("x_", "y_")])
    if ("z_" %in% names(x$vertex)) Z <- x$vertex[["z_"]] else 0
    lon <- round(mean(range(xy[,1], na.rm = TRUE)), digits = 4)
    lat <- round(mean(range(xy[,2], na.rm = TRUE)), digits = 4)
    target <- sprintf("+proj=%s +lon_=%f +lat_0=%s +datum=WGS84", family, lon, lat)
  } else {
    ## we ignoring conic families
    tokens <- defaults(tokenize(proj))
 tokens$proj <- family
    target <- paste(paste("+", names(tokens), "=", tokens, sep = ""), collapse = " ")
  }
  target
}
get_proj <- function(x, ...) UseMethod("get_proj")
get_proj.default <- function(x, ...) {
  mt <- try(x[["meta"]], silent = TRUE)
  if (inherits(mt, "data.frame")) return(mt[["proj"]])
  op <- options(warn = -1)
  on.exit(op)
  rp <- try(raster::projection(x), silent = TRUE)
  if (inherits(rp, "try-error")) rp < - NA
  as.character(rp)
}
get_proj.sf <- function(x, ...) {
  attr(x[[attr(x, "sf_column")]], "crs")[["proj4string"]]
}
get_proj.sfc <- function(x, ...) {
  attr(x, "crs")[["proj4string"]]
}
## should be a sc method, but silicate needs meta everywhere
get_proj.PATH <- function(x, ...) {
  x$meta$proj
}


reproject <- function(x, target = NULL, ..., source) {
   UseMethod("reproject")   
}

reproject.sc <- function(x, target = NULL, ..., family = "laea", source = NULL) {
  if (is.null(target)) {
    target <- make_local(x, family = family)
  }
  if (is.null(source)) source <- get_proj(x)

  verts <- get_vertex(x)
  verts$z_ <- if (is.null(x$vertex$z_)) 0 else x$vertex$z_
  if (inherits(x, "QUAD") && is.null(x$vertex)) {
    x$vertex <- verts
    x$quad <- NULL
  }
  x$vertex[c("x_", "y_", "z_")] <- tibble::as_tibble(reproj::reproj(as.matrix(verts[c("x_", "y_", "z_")]), 
                                               source = source, 
                                               target = target))
  meta <- get_meta(x)
  meta["ctime"] <- Sys.time()
  meta["proj"] <- target
  x$meta <- rbind(meta, x$meta)
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
make_local.QUAD <- function(x, family = "laea", ...) {
  proj <- get_proj(x)
  if (grepl("longlat", proj)) {
#    xy <- as.matrix(x$vertex[c("x_", "y_")])
    lon <- unlist(x$object[c("xmn", "xmx")])
    lat <- unlist(x$object[c("ymn", "ymx")])
    
    #if ("z_" %in% names(x$vertex)) Z <- x$vertex[["z_"]] else 0
    lon <- round(mean(lon), digits = 4)
    lat <- round(mean(lat), digits = 4)
    target <- sprintf("+proj=%s +lon_=%f +lat_0=%s +datum=WGS84", family, lon, lat)
  } else {
    ## we ignoring conic families
    tokens <- defaults(tokenize(proj))
    tokens$proj <- family
    target <- paste(paste("+", names(tokens), "=", tokens, sep = ""), collapse = " ")
  }
  target
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

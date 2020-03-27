#
#
# defaults <- function(x) {
#   x[intersect(c("proj", "lon_0", "lat_0", "datum"), names(x))]
# }
# tokenize <- function(x) {
#   l <- strsplit(gsub("^\\+", "", unlist(strsplit(x, " "))), "=")
#   nc <- lengths(l)
#   if (any(nc < 2)) {
#     for (i in which(nc < 2)) l[[i]] <- c(l[[i]], "")
#   }
#   m <- matrix(unlist(l), 2)
#   setNames(as.list(m[2, ]), m[1, ])
#
# }
# make_local <- function(x, family = "laea") {
#   UseMethod("make_local")
# }
# make_local.QUAD <- function(x, family = "laea", ...) {
#   proj <- get_proj(x)
#   if (grepl("longlat", proj)) {
#     #    xy <- as.matrix(x$vertex[c("x_", "y_")])
#     lon <- unlist(x$object[c("xmn", "xmx")])
#     lat <- unlist(x$object[c("ymn", "ymx")])
#
#     #if ("z_" %in% names(x$vertex)) Z <- x$vertex[["z_"]] else 0
#     lon <- round(mean(lon), digits = 4)
#     lat <- round(mean(lat), digits = 4)
#     target <- sprintf("+proj=%s +lon_=%f +lat_0=%s +datum=WGS84", family, lon, lat)
#   } else {
#     ## we ignoring conic families
#     tokens <- defaults(tokenize(proj))
#     tokens$proj <- family
#     target <- paste(paste("+", names(tokens), "=", tokens, sep = ""), collapse = " ")
#   }
#   target
# }
# make_local.sc <- function(x, family = "laea", ...) {
#   proj <- get_proj(x)
#   if (grepl("longlat", proj)) {
#     xy <- as.matrix(x$vertex[c("x_", "y_")])
#     if ("z_" %in% names(x$vertex)) Z <- x$vertex[["z_"]] else 0
#     lon <- round(mean(range(xy[,1], na.rm = TRUE)), digits = 4)
#     lat <- round(mean(range(xy[,2], na.rm = TRUE)), digits = 4)
#     target <- sprintf("+proj=%s +lon_=%f +lat_0=%s +datum=WGS84", family, lon, lat)
#   } else {
#     ## we ignoring conic families
#     tokens <- defaults(tokenize(proj))
#     tokens$proj <- family
#     target <- paste(paste("+", names(tokens), "=", tokens, sep = ""), collapse = " ")
#   }
#   target
# }
#
#


library(raster)
library(marmap) ## has topo data
data(hawaii)
marToRaster <- function(x) {
  x <- setExtent(raster(unclass(x)), extent(unlist(lapply(attr(x, "dimnames"), function(x) as.numeric(c(head(x, 1), tail(x, 1)))))))
  if (raster:::.couldBeLonLat(x)) {
    projection(x) <- "+proj=longlat +ellps=WGS84 +no_defs"
  }
  x
}

r <- marToRaster(hawaii)
library(rangl)  ## devtools::install_github("r-gris/rangl")
r1 <- rangl(r)
library(rgl)


m1 <- plot(rangl(aggregate(r, fact = 2, method = "bilinear"))); aspect3d(1, 1, 1e-1)
rgl.close()
shade3d(m1); aspect3d(1, 1, 1e-1); box3d()
writeWebGL(dir = sprintf("mesh_%i", ncol(m1$ib)))
?writeWebGL
r2 <- rangl(disaggregate(r, fact = 4, method = "bilinear"))


quadToTriangle <- function(x) {
  v <- x$v
  v$vertex_ <- seq(nrow(v))
  meta <- x$meta
  tab <- x$qXv
  n4 <- nrow(tab) / 4L
  tXv <- tibble(vertex_ = x$qXv$vertex_[rep(c(1, 2, 3, 1, 3, 4), n4) + rep(seq(1, length = n4, by = 4)-1, each = 6)])
  tXv$triangle_ <- rep(seq(nrow(tXv)/3), each = 3)
  x <- list(o = tibble(object_ = "1"), t = tibble(triangle_ = seq(nrow(tXv)/3), object_ = "1"), 
       tXv = tXv, v = v, meta = meta)
  class(x) <- "trimesh"
  x
}

#r1 <- rangl(raster(volcano))
tr <- quadToTriangle(r1)
a <- plot(tr); aspect3d(1, 1, 0.1)
shade3d(a, col = rep(viridis::viridis(ncol(a$it)), each = 3))
aspect3d(1, 1, 0.1)
writeWebGL(dir = sprintf("mesh_%i", ncol(a$it)))

shade3d(a, col = viridis::viridis(ncol(a$it))[a$it])
aspect3d(1, 1, 0.1)


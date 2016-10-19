
library(rangl)
library(raster)
library(marmap) ## has topo data
data(hawaii)
library(rgeos)
haw <- setExtent(raster(unclass(hawaii)), extent(unlist(lapply(attr(hawaii, "dimnames"), function(x) as.numeric(c(head(x, 1), tail(x, 1)))))))
projection(haw) <- "+proj=longlat +ellps=WGS84"

hpoly <- rasterToPolygons(cut(haw, c(cellStats(haw, min), 0, cellStats(haw, max))), dissolve = TRUE)

## just some polygons
hpoly <- rgeos::gPolygonize(rasterToContour(cut(haw, c(cellStats(haw, min), 0, cellStats(haw, max))), lev = 1.5))

plot(globe(rangl(haw * 280)))
rh <- rangl(hpoly)
rh$v$z_ <- 4e6
plot(globe(rh))

f <- system.file("extdata/gebco1.tif", package = "anglr")
## ad hoc scaling as x,y and  z are different units
r <- raster::raster(f)/1000

library(sf)
nc <- read_sf(system.file("shape/nc.shp", package="sf"))
library(raster)
library(anglr) ## devtools::install_github("hypertidy/anglr")

## objects
## a relief map, triangles grouped by polygon with interpolated raster elevation 
p <- anglr(nc, max_area = 0.008) ## make small triangles (0.2 sq lon-lat degree)
g <- anglr(graticule::graticule(-85:-74, 32:37))
p$v$z_ <- extract(r, cbind(p$v$x_, p$v$y_), method = "bilinear")

## plot the scene
library(rgl)
rgl.clear(); 
plot(p); plot(g, color = "white"); 
bg3d("black"); material3d(specular = "black"); 
rglwidget(width =  900, height = 450)

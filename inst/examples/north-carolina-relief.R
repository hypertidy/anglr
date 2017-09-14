f <- system.file("extdata/gebco1.tif", package = "anglr")
## ad hoc scaling as x,y and  z are different units
r <- raster::raster(f)/1000

library(sf)
nc <- read_sf(system.file("shape/nc.shp", package="sf"))
library(raster)
library(anglr) ## devtools::install_github("hypertidy/anglr")

## objects
## a relief map, triangles grouped by polygon with interpolated raster elevation 
p_mesh <- anglr(nc, max_area = 0.008) ## make small triangles ( sq lon-lat degree)
#g <- anglr(graticule::graticule(-85:-74, 32:37))
p_mesh$v$z_ <- raster::extract(r, cbind(p_mesh$v$x_, p_mesh$v$y_), method = "bilinear")

## plot the scene
library(rgl)
rgl.clear() 
plot(pmesh) 
#plot(g, color = "white") 
bg3d("black"); material3d(specular = "black")
rglwidget(width =  900, height = 450)

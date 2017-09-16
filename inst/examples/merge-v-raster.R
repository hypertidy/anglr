## compare this projected one to 
## the readme unprojected on (2017-09-16)

topofile <- system.file("extdata/gebco1.tif", package = "anglr")
## ad hoc scaling as x,y and  z are different units
topo <- raster::raster(topofile) * 20

#library(sf)
#nc <- read_sf(system.file("shape/nc.shp", package="sf"))
data("inlandwaters", package= "silicate")
library(raster)
library(anglr) ## devtools::install_github("hypertidy/anglr")

## objects
## a relief map, triangles grouped by polygon with interpolated raster elevation 
library(sf)
tas <- inlandwaters[5, ]
tas <- st_cast(tas, "POLYGON")[-57, ]
## mix up the order
tas <- tas[sample(nrow(tas)), ]
p_mesh <- anglr(tas, max_area = 1e8) 
#g <- anglr(graticule::graticule(-85:-74, 32:37))
p_mesh$v$z_ <- raster::extract(topo, rgdal::project(cbind(p_mesh$v$x_, p_mesh$v$y_), p_mesh$meta$proj[1], inv = TRUE), method = "bilinear")
rgl::plot3d(p_mesh$v$x_, p_mesh$v$y_, p_mesh$v$z_)

## plot the scene
library(rgl)

#rgl.clear()  ## rerun the cycle from clear to widget in browser contexts 
plot(p_mesh, add_normals = TRUE) 
#plot(g, color = "white") 
bg3d("black"); material3d(specular = "black")
#rglwidget(width =  900, height = 450)  ## not needed if you have a local device

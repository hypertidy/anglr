## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
fig.path = "vignettes/readme-figure/README-"
)
rgl::par3d(windowRect = c(100, 100, 512 + 100, 512 +100))

## ----eval=TRUE-----------------------------------------------------------

library(rgl)
library(maptools)
data(wrld_simpl)
library(raster)

## convert to triangles and plot 
library(rangl)
cmesh <- rangl(wrld_simpl)
plot(cmesh)

## snapshot code is only for this README
rgl.snapshot("vignettes/readme-figure/README-wrld_simpl.png"); rgl.close()

## ------------------------------------------------------------------------
sids <- raster::shapefile(system.file("shapes/sids.shp", package="maptools"))
projection(sids) <- "+proj=longlat +ellps=clrk66"

ex <- extent(sids) + 5
gl <- graticule::graticule(seq(xmin(ex), xmax(ex), length = 15), 
                           seq(ymin(ex), ymax(ex), length = 8))


## convert to triangles, but wrap onto globe then plot
smesh <- rangl(sids)
plot(globe(smesh))
mgl <- rangl(gl)
mgl$o$color_ <- "black"
plot(globe(mgl), lwd = 2)

rgl.snapshot("vignettes/readme-figure/README-sids-globe.png"); rgl.close()


## ----eval=TRUE-----------------------------------------------------------
library(spbabel)
data(holey)

## SpatialPolygonsDataFrame
sph <- sp(holey)

glh <- rangl(sph)
plot(glh)
rgl::rgl.snapshot("vignettes/readme-figure/README-home.png"); rgl.close()


## ----eval=TRUE-----------------------------------------------------------
linehouse <- as(sph, "SpatialLinesDataFrame")
plot(rangl(linehouse))
rgl::rgl.snapshot("vignettes/readme-figure/README-lines.png"); rgl.close()

## ----eval=TRUE-----------------------------------------------------------
lmesh <- rangl(as(wrld_simpl, "SpatialLinesDataFrame"))
plot(globe(lmesh))
rgl::rgl.snapshot("vignettes/readme-figure/README-liney-world.png"); rgl.close()


## ----eval=TRUE-----------------------------------------------------------
library(rgl)

dod <- rangl(dodecahedron3d(col = "cyan"))
octo <- rangl(translate3d(octahedron3d(col = "blue"), 6, 0, 0))
plot(dod, col = viridis::viridis(5)[1], alpha = 0.3)
plot(octo, col = viridis::viridis(5)[5], alpha = 0.3)
bg3d("grey")
rgl::rgl.snapshot("vignettes/readme-figure/README-Platonic.png"); rgl.close()


## ------------------------------------------------------------------------
library(rangl)
library(maptools)
data(wrld_simpl)
mpts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame")
plot(rangl(mpts))
rgl::view3d(theta = 25, phi = 3)


## ----include=FALSE-------------------------------------------------------
rgl::rgl.snapshot("vignettes/readme-figure/README-MultiPoints.png"); rgl::rgl.close()



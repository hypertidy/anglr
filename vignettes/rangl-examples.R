## ----setup, echo=FALSE, results="asis"-----------------------------------
source("setup.R")
knitr::opts_chunk$set(rgl.newwindow = TRUE)
set.seed(123)

## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>")
rgl::par3d(windowRect = c(100, 100, 512 + 100, 512 +100))
library(raster)
library(maptools)

## ----eval=TRUE, webgl=TRUE-----------------------------------------------

library(rgl)
library(maptools)
data(wrld_simpl)
library(raster)

## convert to triangles and plot 
library(rangl)
cmesh <- rangl(wrld_simpl)
plot(cmesh)

## ---- webgl=TRUE---------------------------------------------------------
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

## ----eval=TRUE, webgl=TRUE-----------------------------------------------
library(spbabel)
data(holey)

## SpatialPolygonsDataFrame
sph <- sp(holey)

glh <- rangl(sph)
plot(glh)

## ---- webgl=TRUE---------------------------------------------------------
linehouse <- as(sph, "SpatialLinesDataFrame")
plot(rangl(linehouse))

## ---- webgl=TRUE---------------------------------------------------------
lmesh <- rangl(as(wrld_simpl, "SpatialLinesDataFrame"))
plot(globe(lmesh))

## ----webgl=TRUE----------------------------------------------------------
library(rgl)

dod <- rangl(dodecahedron3d(col = "cyan"))
octo <- rangl(translate3d(octahedron3d(col = "blue"), 6, 0, 0))
plot(dod, col = viridis::viridis(5)[1], alpha = 0.3)
plot(octo, col = viridis::viridis(5)[5], alpha = 0.3)
bg3d("grey")

## ---- webgl=TRUE---------------------------------------------------------
library(rangl)
library(maptools)
data(wrld_simpl)
mpts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame")
plot(rangl(mpts))
rgl::view3d(theta = 25, phi = 3)

## ---- webgl=TRUE---------------------------------------------------------
library(trip)
library(rangl)
data(walrus818)

library(graticule)
prj <-"+proj=laea +lon_0=0 +lat_0=90 +ellps=WGS84"
gr <- graticule(lats = seq(40, 85, by = 5), ylim = c(35, 89.5), proj = prj)
library(maptools)
data(wrld_simpl)

w <- spTransform(subset(wrld_simpl, coordinates(wrld_simpl)[,2] > -70),  prj)
library(graticule)
walrus <- spTransform(walrus818, prj)

gr$color_ <- "black"
rgl::par3d(windowRect = c(100, 100, 912 + 100, 912 +100))
plot(rangl(gr))
w$color_ <- sample(viridis::inferno(nrow(w)))
plot(rangl(w), specular = "black")
plot(rangl(walrus))
um <- structure(c(0.934230506420135, 0.343760699033737, 0.0950899347662926, 
                  0, -0.302941381931305, 0.905495941638947, -0.297159105539322, 
                  0, -0.188255190849304, 0.24880850315094, 0.950081348419189, 0, 
                  0, 0, 0, 1), .Dim = c(4L, 4L))
par3d(userMatrix = um)



## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "readme-figure/README-"
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
rgl.snapshot("readme-figure/README-wrld_simpl.png"); rgl.clear()

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

rgl.snapshot("readme-figure/README-sids-globe.png"); rgl.clear()


## ----eval=TRUE-----------------------------------------------------------
library(spbabel)
data(holey)

## SpatialPolygonsDataFrame
sph <- sp(holey)

glh <- rangl(sph)
plot(glh)
rgl::rgl.snapshot("readme-figure/README-home.png"); rgl.clear()


## ----eval=TRUE-----------------------------------------------------------
linehouse <- as(sph, "SpatialLinesDataFrame")
plot(rangl(linehouse))
rgl::rgl.snapshot("readme-figure/README-lines.png"); rgl.clear()

## ----eval=TRUE-----------------------------------------------------------
lmesh <- rangl(as(wrld_simpl, "SpatialLinesDataFrame"))
plot(globe(lmesh))
rgl::rgl.snapshot("readme-figure/README-liney-world.png"); rgl.clear()


## ----eval=TRUE-----------------------------------------------------------
library(rgl)

dod <- rangl(dodecahedron3d(col = "cyan"))
octo <- rangl(translate3d(octahedron3d(col = "blue"), 6, 0, 0))
plot(dod, col = viridis::viridis(5)[1], alpha = 0.3)
plot(octo, col = viridis::viridis(5)[5], alpha = 0.3)
bg3d("grey")
rgl::rgl.snapshot("readme-figure/README-Platonic.png"); rgl.clear()


## ------------------------------------------------------------------------
library(rangl)
library(maptools)
data(wrld_simpl)
mpts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame")
plot(rangl(mpts))
rgl::view3d(theta = 25, phi = 3)


## ----include=FALSE-------------------------------------------------------
rgl::rgl.snapshot("readme-figure/README-MultiPoints.png"); rgl::rgl.clear()


## ----eval=TRUE-----------------------------------------------------------
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
rgl.close()
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
rgl::rgl.snapshot("readme-figure/README-walrus.png"); rgl.clear()


## ------------------------------------------------------------------------
wrld <- rasterize(wrld_simpl, raster())
plot(rangl(wrld/10))
rgl::rgl.snapshot("readme-figure/README-raster.png"); rgl.clear()

plot(globe(rangl(wrld*100000)), specular = "black")
rgl::rgl.snapshot("readme-figure/README-rasterglobe.png"); rgl.clear()


## ---- eval=FALSE---------------------------------------------------------
#  ## Spatially explicit ecosystem model (Atlantis)
#  
#  
#  library(rbgm)
#  bgm <- bgmfile(bgmfiles::bgmfiles("antarctica_99"))
#  
#  bgm$boxes$botz
#  cols <- viridis::viridis(nrow(bgm$boxes))
#  for (i in seq(nrow(bgm$boxes))) {
#  #x <- sp::spTransform(boxSpatial(bgm)[i, ], "+proj=laea +lat_0=-65 +lon_0=100")
#  x <- boxSpatial(bgm)[i, ]
#  dz <- rbgm:::build_dz(x$botz)
#  #z0 <- 0
#  alp <- seq(0.05, 0.9, length = length(dz))
#  for (zi in seq_along(dz)) {
#    if (dz[zi] > 0) {
#      p <- spbabel::sptable(x)[, c("x_", "y_")]
#      rglobj <- rgl::extrude3d(p, thickness = dz[zi])
#      rglobj$vb[3,] <- rglobj$vb[3,] - sum(dz[seq(zi)])
#     # print(sum(dz[seq(zi)]))
#      rgl::shade3d(rglobj, col = cols[i], alpha = alp[zi],
#                   specular = "black")
#    }
#  }
#  }
#  rgl::aspect3d(1, 1, 1.2)


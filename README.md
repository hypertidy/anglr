
[![Travis-CI Build Status](https://travis-ci.org/r-gris/rangl.svg?branch=master)](https://travis-ci.org/r-gris/rangl) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-gris/rangl?branch=master&svg=true)](https://ci.appveyor.com/project/r-gris/rangl) [![Coverage Status](https://img.shields.io/codecov/c/github/r-gris/rangl/master.svg)](https://codecov.io/github/r-gris/rangl?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Tidy tables for spatial data
----------------------------

The basics for creating a mesh from a `SpatialPolygons` object is in place.

Multiple multi-part objects are decomposed to a set of related, linked tables. Object identity is maintained with attribute metadata and this is carried through to colour aesthetics in 3D plots.

Example, get some maps and plot in 3D - in plane view, or globe view.

``` r

library(rgl)
library(maptools)
data(wrld_simpl)
library(raster)
sids <- raster::shapefile(system.file("shapes/sids.shp", package="maptools"))
projection(sids) <- "+proj=longlat +ellps=clrk66"

## convert to triangles and plot 
library(rangl)
cmesh <- tri_mesh(wrld_simpl)
plot(cmesh)

## convert to triangles, but wrap onto globe then plot
smesh <- tri_mesh(sids)
rgl::rgl.open()
globe(smesh)
```

Holes are supported.

``` r
library(spbabel)
data(holey)

## SpatialPolygonsDataFrame
sph <- sp(holey)

glh <- tri_mesh(sph)
plot(glh)
```

`rgl` mesh3d triangle-primitive objects are supported.

Example TBD

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

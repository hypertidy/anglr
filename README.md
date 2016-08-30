
[![Travis-CI Build Status](https://travis-ci.org/r-gris/rangl.svg?branch=master)](https://travis-ci.org/r-gris/rangl) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-gris/rangl?branch=master&svg=true)](https://ci.appveyor.com/project/r-gris/rangl) [![Coverage Status](https://img.shields.io/codecov/c/github/r-gris/rangl/master.svg)](https://codecov.io/github/r-gris/rangl?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Tabular storage for spatial data
--------------------------------

The 'rangl' package illustrates some generalizations of GIS-y tasks in R with "tables".

The basic model is to create "mesh" objects from a variety of sources:

-   SpatialPolygons, SpatialLines
-   rgl 3D objects
-   --others to come-- see <https://github.com/mdsumner/spbabel>

Multiple multi-part objects are decomposed to a set of related, linked tables. Object identity is maintained with attribute metadata and this is carried through to colour aesthetics in 3D plots.

Plot methods take those tables and generate the "indexed array" structures needed for 'rgl'. In this way we get the best of both worlds of "GIS" and "3D models".

The core work for translating "Spatial" classes is done by the unspecialized 'spbabel::map\_table' function.

Installation
------------

This package is in active development and will see a number of breaking changes before release.

Also required are packages 'rgl' and 'RTriangle', so first make sure you can install and use these.

``` r
install.packages("rgl")
install.packages("RTriangle")
```

If you are still feeling adventurous, 'rangl' can be installed from Github. This will also ensure that the latest version of 'spbabel' is installed, until I get around to updating that on CRAN.

``` r
devtools::install_github("r-gris/rangl")
```

Get involved!
-------------

Let me know if you have problems, or are interested in this work. See the issues tab to make suggestions or report bugs.

<https://github.com/r-gris/rangl/issues>

Examples
--------

Get some maps and plot in 3D - in plane view, or globe view.

**NOTE:** these examples here just show snapshots. It's much more interesting to run these yourself locally and interact with the rgl plots.

``` r

library(rgl)
library(maptools)
data(wrld_simpl)
library(raster)

## convert to triangles and plot 
library(rangl)
cmesh <- mesh(wrld_simpl)
plot(cmesh)

## snapshot code is only for this README
rgl.snapshot("readme-figure/README-wrld_simpl.png"); rgl.clear()

sids <- raster::shapefile(system.file("shapes/sids.shp", package="maptools"))
projection(sids) <- "+proj=longlat +ellps=clrk66"

ex <- extent(sids) + 5
gl <- graticule::graticule(seq(xmin(ex), xmax(ex), length = 15), 
                           seq(ymin(ex), ymax(ex), length = 8))


## convert to triangles, but wrap onto globe then plot
smesh <- mesh(sids)
plot(globe(smesh))
mgl <- mesh(gl)
mgl$o$color_ <- "black"
plot(globe(mgl), lwd = 2)

rgl.snapshot("readme-figure/README-sids-globe.png"); rgl.clear()
```

![World simpl](readme-figure/README-wrld_simpl.png?raw=true "World simpl")

![SIDS globe](readme-figure/README-sids-globe.png?raw=true "SIDS globe")

Holes are supported.
--------------------

``` r
library(spbabel)
data(holey)

## SpatialPolygonsDataFrame
sph <- sp(holey)

glh <- mesh(sph)
plot(glh)
rgl::rgl.snapshot("readme-figure/README-home.png"); rgl.clear()
```

![Holey home](readme-figure/README-home.png?raw=true "Holey home")

Lines are supported.
--------------------

``` r
linehouse <- as(sph, "SpatialLinesDataFrame")
plot(mesh(linehouse))
rgl::rgl.snapshot("readme-figure/README-lines.png"); rgl.clear()
```

![Liney lines](readme-figure/README-lines.png?raw=true "Liney lines")

Globe lines
-----------

``` r
lmesh <- mesh(as(wrld_simpl, "SpatialLinesDataFrame"))
plot(globe(lmesh))
rgl::rgl.snapshot("readme-figure/README-liney-world.png"); rgl.clear()
```

![Liney world](readme-figure/README-liney-world.png?raw=true "Liney world")

Mesh3d rgl
----------

Rgl mesh3d objects that use "triangle" primitives are supported.

``` r
library(rgl)

dod <- mesh(dodecahedron3d(col = "cyan"))
octo <- mesh(translate3d(octahedron3d(col = "blue"), 6, 0, 0))
plot(dod, col = viridis::viridis(5)[1], alpha = 0.3)
plot(octo, col = viridis::viridis(5)[5], alpha = 0.3)
bg3d("grey")
rgl::rgl.snapshot("readme-figure/README-Platonic.png"); rgl.clear()
```

![Platonic](readme-figure/README-Platonic.png?raw=true "Platonic")

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

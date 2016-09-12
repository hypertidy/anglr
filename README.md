
[![Travis-CI Build Status](https://travis-ci.org/r-gris/rangl.svg?branch=master)](https://travis-ci.org/r-gris/rangl) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-gris/rangl?branch=master&svg=true)](https://ci.appveyor.com/project/r-gris/rangl) [![Coverage Status](https://img.shields.io/codecov/c/github/r-gris/rangl/master.svg)](https://codecov.io/github/r-gris/rangl?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Tabular storage for spatial data
--------------------------------

The 'rangl' package illustrates some generalizations of GIS-y tasks in R with "tables".

The basic idea is to create "toplogical" objects from a variety of sources:

-   SpatialPolygons, SpatialLines, SpatialMultipoints
-   rgl 3D objects
-   trip objects (animal tracking data)
-   --others to come-- see <https://github.com/mdsumner/spbabel>

Multiple multi-part objects are decomposed to a set of related, linked tables. Object identity is maintained with attribute metadata and this is carried through to colour and other aesthetics in 3D plots.

Plot methods take those tables and generate the "indexed array" structures needed for 'rgl'. In this way we get the best of both worlds of "GIS" and "3D models".

Ongoing design
--------------

The core work for translating "Spatial" classes is done by the unspecialized 'spbabel::map\_table' function.

This is likely to be replaced by a 'primitives()' function that takes any lines or polygons data and returns just the linked edges. \#\# Installation

This package is in active development and will see a number of breaking changes before release.

Also required are packages 'rgl' and 'RTriangle', so first make sure you can install and use these.

``` r
install.packages("rgl")
install.packages("RTriangle")
```

In examples below I use 'graticule' as well, so you might as well install that too.

``` r
install.packages("graticule")
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
#> Loading required package: sp
#> Checking rgeos availability: TRUE
data(wrld_simpl)
library(raster)

## convert to triangles and plot 
library(rangl)
cmesh <- rangl(wrld_simpl)
plot(cmesh)
#> Joining, by = "object_"
#> Joining, by = "triangle_"

## snapshot code is only for this README
rgl.snapshot("readme-figure/README-wrld_simpl.png"); rgl.clear()

sids <- raster::shapefile(system.file("shapes/sids.shp", package="maptools"))
#> Warning in .local(x, ...): .prj file is missing
projection(sids) <- "+proj=longlat +ellps=clrk66"

ex <- extent(sids) + 5
gl <- graticule::graticule(seq(xmin(ex), xmax(ex), length = 15), 
                           seq(ymin(ex), ymax(ex), length = 8))


## convert to triangles, but wrap onto globe then plot
smesh <- rangl(sids)
plot(globe(smesh))
#> Joining, by = "object_"
#> Joining, by = "triangle_"
mgl <- rangl(gl)
mgl$o$color_ <- "black"
plot(globe(mgl), lwd = 2)
#> Joining, by = "object_"
#> Joining, by = "segment_"

rgl.snapshot("readme-figure/README-sids-globe.png"); rgl.clear()
```

![World simpl](readme-figure/README-wrld_simpl.png?raw=true "World simpl")

![SIDS globe](readme-figure/README-sids-globe.png?raw=true "SIDS globe")

Holes are trivially supported.
------------------------------

It's trivial to have "holes", because there are no "holes" because we have a true surface, composed of 2D primitives (triangles).

``` r
library(spbabel)
data(holey)

## SpatialPolygonsDataFrame
sph <- sp(holey)

glh <- rangl(sph)
plot(glh)
#> Joining, by = "object_"
#> Joining, by = "triangle_"
rgl::rgl.snapshot("readme-figure/README-home.png"); rgl.clear()
```

![Holey home](readme-figure/README-home.png?raw=true "Holey home")

Lines are supported.
--------------------

``` r
linehouse <- as(sph, "SpatialLinesDataFrame")
plot(rangl(linehouse))
#> Joining, by = "object_"
#> Joining, by = "segment_"
rgl::rgl.snapshot("readme-figure/README-lines.png"); rgl.clear()
```

![Liney lines](readme-figure/README-lines.png?raw=true "Liney lines")

Globe lines
-----------

``` r
lmesh <- rangl(as(wrld_simpl, "SpatialLinesDataFrame"))
plot(globe(lmesh))
#> Joining, by = "object_"
#> Joining, by = "segment_"
rgl::rgl.snapshot("readme-figure/README-liney-world.png"); rgl.clear()
```

![Liney world](readme-figure/README-liney-world.png?raw=true "Liney world")

Mesh3d rgl
----------

Rgl mesh3d objects that use "triangle" primitives are supported.

``` r
library(rgl)

dod <- rangl(dodecahedron3d(col = "cyan"))
octo <- rangl(translate3d(octahedron3d(col = "blue"), 6, 0, 0))
plot(dod, col = viridis::viridis(5)[1], alpha = 0.3)
#> Joining, by = "object_"
#> Joining, by = "triangle_"
plot(octo, col = viridis::viridis(5)[5], alpha = 0.3)
#> Joining, by = "object_"
#> Joining, by = "triangle_"
bg3d("grey")
rgl::rgl.snapshot("readme-figure/README-Platonic.png"); rgl.clear()
```

![Platonic](readme-figure/README-Platonic.png?raw=true "Platonic")

Points
------

And points work! (Don't laugh).

``` r
library(rangl)
library(maptools)
data(wrld_simpl)
mpts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame")
plot(rangl(mpts))
#> Joining, by = "object_"
#> Joining, by = "branch_"
rgl::view3d(theta = 25, phi = 3)
```

![MultiPoints](readme-figure/README-MultiPoints.png?raw=true "MultiPoints")

Trips
-----

The soon to be released update to trip includes a 'walrus818' data set courtesy of Anthony Fischbach.

``` r
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
#> Joining, by = "object_"
#> Joining, by = "segment_"
w$color_ <- sample(viridis::inferno(nrow(w)))
plot(rangl(w), specular = "black")
#> Joining, by = "object_"
#> Joining, by = "triangle_"
plot(rangl(walrus))
#> Joining, by = "object_"
#> Joining, by = "segment_"
um <- structure(c(0.934230506420135, 0.343760699033737, 0.0950899347662926, 
0, -0.302941381931305, 0.905495941638947, -0.297159105539322, 
0, -0.188255190849304, 0.24880850315094, 0.950081348419189, 0, 
0, 0, 0, 1), .Dim = c(4L, 4L))
par3d(userMatrix = um)
rgl::rgl.snapshot("readme-figure/README-walrus.png"); rgl.clear()
```

![Walrus](readme-figure/README-walrus.png?raw=true "Walrus")

Open topics
-----------

-   single-points are a bit funny, it's wasteful to copy the branches model - needs thought
-   multi-points are also a bit funny, but this will form the basis of input tracking data for some applications

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

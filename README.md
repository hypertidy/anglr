
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
#> Loading required package: sp
#> Checking rgeos availability: TRUE
data(wrld_simpl)
library(raster)

## convert to triangles and plot 
library(rangl)
cmesh <- mesh(wrld_simpl)
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
smesh <- mesh(sids)
plot(globe(smesh))
#> Joining, by = "object_"
#> Joining, by = "triangle_"
mgl <- mesh(gl)
mgl$o$color_ <- "black"
plot(globe(mgl), lwd = 2)
#> Joining, by = "object_"
#> Joining, by = "segment_"

rgl.snapshot("readme-figure/README-sids-globe.png"); rgl.clear()
```

![World simpl](readme-figure/README-wrld_simpl.png?raw=true "World simpl")

![SIDS globe](readme-figure/README-sids-globe.png?raw=true "SIDS globe")

Holes are supported.

``` r
library(spbabel)
data(holey)

## SpatialPolygonsDataFrame
sph <- sp(holey)

glh <- mesh(sph)
plot(glh)
#> Joining, by = "object_"
#> Joining, by = "triangle_"
rgl::rgl.snapshot("readme-figure/README-home.png"); rgl.clear()
```

![Holey home](readme-figure/README-home.png?raw=true "Holey home")

Lines are supported.

``` r
linehouse <- as(sph, "SpatialLinesDataFrame")
plot(mesh(linehouse))
#> Joining, by = "object_"
#> Joining, by = "segment_"
rgl::rgl.snapshot("readme-figure/README-lines.png"); rgl.clear()
```

![Liney lines](readme-figure/README-lines.png?raw=true "Liney lines")

``` r
lmesh <- mesh(as(wrld_simpl, "SpatialLinesDataFrame"))
plot(globe(lmesh))
#> Joining, by = "object_"
#> Joining, by = "segment_"
rgl::rgl.snapshot("readme-figure/README-liney-world.png"); rgl.clear()
```

![Liney world](readme-figure/README-liney-world.png?raw=true "Liney world")

Rgl mesh3d objects that use "triangle" primitives are supported.

``` r
library(rgl)

dod <- mesh(dodecahedron3d(col = "cyan"))
octo <- mesh(translate3d(octahedron3d(col = "blue"), 6, 0, 0))
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

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

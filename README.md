
[![Travis-CI Build Status](https://travis-ci.org/hypertidy/anglr.svg?branch=master)](https://travis-ci.org/hypertidy/anglr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/anglr?branch=master&svg=true)](https://ci.appveyor.com/project/hypertidy/anglr) [![Coverage Status](https://img.shields.io/codecov/c/github/hypertidy/anglr/master.svg)](https://codecov.io/github/hypertidy/anglr?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Topological forms for plotting spatial data
-------------------------------------------

The 'anglr' package illustrates some generalizations of GIS-y tasks in R with a database-y approach.

The basic idea is to create toplogical objects from a variety of sources:

-   simple features
-   Spatial features
-   rgl 3D objects
-   trip objects (and general animal tracking data types)
-   regular raster grids
-   igraph
-   Lidar

Usage
=====

The general approach is to `anglr` your model and then plot it.

``` r

library(anglr)

#model <- sf::st_read("some/shapefile.shp")
#model <- raster::raster("some/gridraster.tif")

mesh <- anglr(model)
plot(mesh)
```

This is too simplistic for general use but these meshes can be used to merge disparate data into a single form, or used to convert standard spatial objects to `rgl` ready forms.

An example of merging vector and raster can be seen with this.

``` r
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
p_mesh$v$z_ <- extract(r, cbind(p_mesh$v$x_, p_mesh$v$y_), method = "bilinear")

## plot the scene
library(rgl)

rgl.clear()  ## rerun the cycle from clear to widget in browser contexts 
plot(pmesh) 
#plot(g, color = "white") 
bg3d("black"); material3d(specular = "black")
rglwidget(width =  900, height = 450)  ## not needed if you have a local device
```

Multiple multi-part objects are decomposed to a set of related, linked tables. Object identity is maintained with attribute metadata and this is carried through to colour and other aesthetics in 3D plots.

Plot methods take those tables and generate the "indexed array" structures needed for 'rgl'. In this way we get the best of both worlds of "GIS" and "3D models".

Ongoing design
--------------

The core work for translating "Spatial" classes is done by the unspecialized 'spbabel::map\_table' function.

This is likely to be replaced by a 'primitives()' function that takes any lines or polygons data and returns just the linked edges. Crucially, polygons and lines are described by the same 1D primitives, and this is easy to do. Harder is to generate 2D primitives and for that we rely on [Jonathan Richard Shewchuk's Triangle](https://www.cs.cmu.edu/~quake/triangle.html).

Triangulation is with `RTriangle` package using "constrained mostly-Delaunay Triangulation" from the Triangle library, but could alternatively use `rgl` with its ear clipping algorithm.

(With RTriangle we can set a max area for the triangles, so it can wrap around curves like globes and hills.)

Installation
------------

Also required are packages 'rgl' and 'RTriangle', so first make sure you can install and use these.

``` r
install.packages("rgl")
install.packages("RTriangle")
devtools::install_github("hypertidy/anglr")
```

Get involved!
-------------

Let me know if you have problems, or are interested in this work. See the issues tab to make suggestions or report bugs.

<https://github.com/hypertidy/anglr/issues>

Examples
--------

See the vignettes: <https://hypertidy.github.io/anglr/articles/index.html>

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.


[![Travis-CI Build Status](http://badges.herokuapp.com/travis/hypertidy/anglr?branch=master&env=BUILD_NAME=trusty_release&label=linux)](https://travis-ci.org/hypertidy/anglr) [![Build Status](http://badges.herokuapp.com/travis/hypertidy/anglr?branch=master&env=BUILD_NAME=osx_release&label=osx)](https://travis-ci.org/hypertidy/anglr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/anglr?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/anglr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/anglr)](https://cran.r-project.org/package=anglr) [![Coverage Status](https://img.shields.io/codecov/c/github/hypertidy/anglr/master.svg)](https://codecov.io/github/hypertidy/anglr?branch=master)

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
## a global DEM
f <- system.file("extdata/gebco1.tif", package = "anglr")
## ad hoc scaling as x,y and  z are different units
r <- raster::raster(f)/1000


library(sf)
## North Carolina, the sf boilerplate polygon layer
nc <- read_sf(system.file("shape/nc.shp", package="sf"))


library(raster)
library(anglr) ## devtools::install_github("hypertidy/anglr")

## a relief map, triangles grouped by polygon with interpolated raster elevation 
p_mesh <- anglr(nc, max_area = 0.008, z = r)

## plot the scene
library(rgl)

rgl.clear()  ## rerun the cycle from clear to widget in browser contexts 
plot(p_mesh) 
#plot(g, color = "white") 
bg3d("black"); material3d(specular = "black")
rglwidget()  ## not needed if you have a local device
```

Here the `z` argument to `anglr` is a raster, and so once the mesh is created the `z_` coordinate of the mesh is updated by extracting values from the raster (using bilinear interpolation method). If `z_` is alternately set to a column name in the layer this is used as a contstant offset for the `z_` value, and the mesh is "separated", by feature even if the original layer had touching neighbours. .

Multiple multi-part objects are decomposed to a set of related, linked tables. Object identity is maintained with attribute metadata and this is carried through to colour and other aesthetics in 3D plots.

Plot methods take those tables and generate the "indexed array" structures needed for 'rgl'. In this way we get the best of both worlds of "GIS" and "3D models".

Ongoing design
--------------

The core work for translating spatial classes is done by the unspecialized 'silicate::PATH' function and its underlying decomposition generics.

`anglr` then decomposes further, from path-types to primitive-types - where "primitive" means topological primitives, vertices, line segments (edges), triangles. Crucially, polygons and lines are described by the same 1D primitives, and this is easy to do. Harder is to generate 2D primitives and for that we rely on [Jonathan Richard Shewchuk's Triangle](https://www.cs.cmu.edu/~quake/triangle.html).

Triangulation is with `RTriangle` package using "constrained mostly-Delaunay Triangulation" from the Triangle library, but could alternatively use `rgl` with its ear clipping algorithm, and related work is in the `laridae` project to bring CGAL facilities to R.

With RTriangle we can set a max area for the triangles, so it can wrap around curves like globes and hills, and this can only be done by the addition of Steiner points. All of this takes us very far from the path-based types generally used by GIS-alikes.

Grids
-----

Raster gridded data are decomposed to "quad" forms, essentially a 2D primitive with four corners rather than 3. This works well in rgl but is slow in the browser for some reason, but we can always break quads down to triangles if needed.

Texture mapping is possible with rgl, but it needs a local coordinate system mapped to the index space of a PNG image. It's easy enough but requires a bit of awkward preparation, not yet simplified. Some different approaches:

<https://rpubs.com/cyclemumner/frink-polyogn>

Deprecated use of rangl here, but shows the general texture coordinate approach required:

<https://gist.github.com/mdsumner/dc80283de50bb23ff7681b14768b9367>

Installation
------------

Also required are packages 'rgl' and 'RTriangle', so first make sure you can install and use these.

``` r
install.packages("rgl")
install.packages("RTriangle")
devtools::install_github("hypertidy/anglr")
```

### Ubuntu/Debian

On Linux you will need at least the following installed by an administrator, here tested on Ubuntu Xenial 16.04 (note the apt/sources.list is specific to version).

``` bash
## key for apt-get update, see http://cran.r-project.org/bin/linux/ubuntu/README
echo 'deb https://cloud.r-project.org/bin/linux/ubuntu xenial/' >> /etc/apt/sources.list
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

## up to date GDAL and PROJ.4 and GEOS
## https://launchpad.net/~ubuntugis/+archive/ubuntu/ubuntugis-unstable
add-apt-repository ppa:ubuntugis/ubuntugis-unstable --yes

apt update 
apt upgrade --assume-yes

## Install 3rd parties
apt install libproj-dev libgdal-dev libgeos-dev  libssl-dev libgl1-mesa-dev libglu1-mesa-dev libudunits2-dev
## install R, if you need to
## apt install r-base r-base-dev 
```

Then in R

``` r
install.packages("devtools")
install.packages(c("dplyr", "proj4", "raster",  "rgl", "rlang", "RTriangle", "spbabel", "tibble", "viridis"))
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


<!--
[![R build status](https://github.com/hypertidy/anglr/workflows/R-CMD-check/badge.svg)](https://github.com/hypertidy/anglr/actions)
[![R build status](https://github.com/hypertidy/anglr/workflows/test-coverage/badge.svg)](https://github.com/hypertidy/anglr/actions)
[![R build status](https://github.com/hypertidy/anglr/workflows/test-pkgdown/badge.svg)](https://github.com/hypertidy/anglr/actions)
-->

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis-CI Build
Status](http://badges.herokuapp.com/travis/hypertidy/anglr?branch=master&env=BUILD_NAME=trusty_release&label=linux)](https://travis-ci.org/hypertidy/anglr)
[![Build
Status](http://badges.herokuapp.com/travis/hypertidy/anglr?branch=master&env=BUILD_NAME=osx_release&label=osx)](https://travis-ci.org/hypertidy/anglr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/anglr?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/anglr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/anglr)](https://cran.r-project.org/package=anglr)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/anglr)](https://cran.r-project.org/package=anglr)
[![Codecov test
coverage](https://codecov.io/gh/hypertidy/anglr/branch/master/graph/badge.svg)](https://codecov.io/gh/hypertidy/anglr?branch=master)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# anglr

<a href="https://gist.github.com/mdsumner/81be7955ab19c052cea753230d91a20c"><img align="right"  src="https://gist.githubusercontent.com/mdsumner/81be7955ab19c052cea753230d91a20c/raw/f6a2fdb773bcf73db97879d25de8c09a210b7a8e/anglr2.gif"/></a>

The anglr package aims to provide direct access to generic 3D tools.

… geospatial is cool … 3D is cool … putting them together is
*challenging* …

The anglr package provides a full suite of mesh-creation and 3D plotting
functions, extending the [rgl
package](https://CRAN.r-project.org/package=rgl) functions
`as.mesh3d()`, `plot3d()`, `persp3d()`, `wire3d()`, and `dot3d()`.

Key working examples are in
[?anglr::as.mesh3d](https://hypertidy.github.io/anglr/reference/as.mesh3d.html)
and
[?anglr::plot3d](https://hypertidy.github.io/anglr/reference/plot3d.html).

## Features

  - Convert geospatial objects to 3D with `as.mesh3d()` - works for
    matrix, raster, sf-polygons, sp-polygons, silicate-triangles

  - Plot geospatial objects with `plot3d()` - works for matrix, raster,
    sf (any type), sp, trip, silicate

  - Merge raster and vector with `as.mesh3d` - elevate polygons or lines
    with a raster dem

  - Drape imagery onto raster or polygon surfaces with `as.mesh3d` -
    `image_texture` argument

  - Copy Z values from a raster, polygon/line field value, or constant
    onto mesh vertices

  - Create any mesh above and reproject to any projection, including
    geocentric

  - Use `shade3d()` or `plot3d()` or `wire3d()` from the rgl package to
    plot any `mesh3d` object created. (`plot3d()` also calls the
    converters for non-mesh types, so we can quickly get a 3D scene from
    any spatial data)

## Why anglr, isn’t this all done?

The rgl package has long supported powerful interactive 3D
visualizations for points, lines, and surfaces. Surfaces can be of any
form, triangles or quad primitives wrapped around a bunny, a globe, or
platonic shapes such as cubes and octahedrons. There is support for
*textures*, which is essentially imagery draped onto a surface at native
resolution - there is no loss of image quality at all.

But, rgl is hard to use, and there are very few helpers for converting
existing formats into the right form. There are some, for example
`as.mesh3d()` methods for commonly used triangulation packages. It also
was harder to use in the past, but recently has been improved a great
deal for taking generic data and converting it.

There has been little support in rgl for *geo-spatial* data structures,
such as spatial rasters, polygons and lines that are common in GIS
workflows. The anglr package aims to fill this gap, and gives total
control over conversion of spatial and other data into mesh forms - even
if visualization is not the end goal.

There *are* packages that convert matrices and imagery to compelling
animated 3D scenes, but these are targetted at presentation rather than
generality and development. The scenes are targetted towards
presentation, and adding general extra data to a given scene requires a
lot of workarounds. There are also only few well-known packages that use
the rgl mesh3d format (Rvcg, …). Many examples use `rgl::surface3d()`
which is limited to the model that provides for an input matrix and
overlaid colours or textures. These cannot be wrapped arbitrarily around
curves in any 3D coordinate system.

## Installation

Install from [CRAN](https://CRAN.r-project.org/package=anglr) with

``` r
install.packages("anglr")
```

To install the development version use

``` r
## install.packages("remotes")  ## if required
remotes::install_github("hypertidy/anglr")
```

Feel free to contact me via the
[Issues](https://github.com/hypertidy/anglr/issues/) if you have
problems, or these notes don’t make sense.

### Ubuntu/Debian

On Linux you will need at least the following installed.

    libproj-dev \
     libgl1-mesa-dev \
     libglu1-mesa-dev

## Get involved\!

Let me know if you have problems, or are interested in this work. See
the issues tab to make suggestions or report bugs.

<https://github.com/hypertidy/anglr/issues>

## Examples

These examples are a bit out of date, anglr visualization works best
interactively and with your own data, but these demo pages give a
flavour for what is possible.

### Demo 01 - merge vector and raster data

An [example of merging vector and
raster](https://hypertidy.github.io/anglr-demos/demo01.html), with a
continuous interpretation applied to the polygon mesh and underlying
elevation.

``` r
## a global DEM
data("gebco", package = "anglr")
library(sf)
## North Carolina, the sf boilerplate polygon layer
nc <- read_sf(system.file("shape/nc.shp", package="sf"))


library(raster)
library(anglr) 
library(silicate)

p_mesh <- DEL(nc, max_area = 0.002)
## a relief map, composed of triangles grouped by polygon with ##  interpolated raster elevation 
p_mesh <- copy_down(p_mesh, z = gebco)


## plot the scene
library(rgl)

rgl.clear()  ## rerun the cycle from clear to widget in browser contexts 
plot3d(p_mesh) 
bg3d("black"); material3d(specular = "black")
aspect3d(1, 1, .1)
```

Follow this link to see the result:

[Demo01](https://hypertidy.github.io/anglr-demos/demo01.html)

Here the `z` argument to `copy_down()` is a raster, and so the `z_`
coordinate of the mesh is updated by extracting values from the raster
using bilinear interpolation.

### Demo 02 - copy discrete values to polygon elevation

An [example of elevating polygons with constant attribute
values](https://hypertidy.github.io/anglr-demos/demo02.html), a discrete
interpretation that requires separating the (until-now) continuous mesh
of polygon features.

With argument `z` set to a column in the layer or a specific vector of
values this is used as a constant offset for the `z_` value, and the
mesh is *separated by feature*.

(The simpler TRI model is used here because we only need poor quality
triangles for planar geometry. )

``` r

## either form works
#c_mesh <- copy_down(TRI(nc), z = p_mesh$object$BIR74)
c_mesh <- copy_down(TRI(nc), z = "BIR74")
open3d()
a <- plot3d(c_mesh) 
bg3d("black"); material3d(specular = "black")
aspect3d(1, 1, .2)
```

Follow this link to see the result:

[Demo02](https://hypertidy.github.io/anglr-demos/demo02.html)

In the silicate models, complex objects are decomposed to a set of
related, linked tables. Object identity is maintained with attribute
metadata and this is carried through to colour and other aesthetics.

Plot (3D) methods take those tables and generate the “indexed array”
structures needed for ‘rgl’. (`plot3d` will return the rgl-model form).
This gets us part of the way towards having the best of both worlds of
GIS and 3D graphics.

Another example shows this approach applied to a 3D multipatch
shapefile, with a few easy steps we can plot a wiremesh or roof/floor
polygons from a civic building footprint.

<http://rpubs.com/cyclemumner/367010>

-----

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/hypertidy/anglr/blob/master/CONDUCT.md). By
participating in this project you agree to abide by its terms.

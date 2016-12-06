
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

This is likely to be replaced by a 'primitives()' function that takes any lines or polygons data and returns just the linked edges. Crucially, polygons and lines are described by the same 1D primitives, and this is easy to do. Harder is to generate 2D primitives and for that we rely on [Jonathan Richard Shewchuk's Triangle](https://www.cs.cmu.edu/~quake/triangle.html).

Triangulation is with `RTriangle` package using "constrained mostly-Delaunay Triangulation" from the Triangle library, but could alternatively use `rgl` with its ear clipping algorithm.

(With RTriangle we can set a max area for the triangles, so it can wrap around curves like globes and hills.)

Installation
------------

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

See the vignettes: <https://r-gris.github.io/rangl/articles/index.html>

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

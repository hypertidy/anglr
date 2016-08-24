
[![Travis-CI Build Status](https://travis-ci.org/r-gris/rangl.svg?branch=master)](https://travis-ci.org/r-gris/rangl) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-gris/rangl?branch=master&svg=true)](https://ci.appveyor.com/project/r-gris/rangl) [![Coverage Status](https://img.shields.io/codecov/c/github/r-gris/rangl/master.svg)](https://codecov.io/github/r-gris/rangl?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Tidy tables for spatial data
----------------------------

The basics for creating a mesh from a `SpatialPolygons` object is in place.

Multiple multi-part objects are decomposed to a set of related, linked tables. Object identity is maintained with attribute metadata and this is carried through to colour aesthetics in 3D plots.

``` r
library(rglwidget)
library(rgl)
library(rworldmap)
data(countriesLow)


library(rangl)
cmesh <- tri_mesh(countriesLow[sample(nrow(countriesLow), 10), ])
plot(cmesh)

#subid <- currentSubscene3d()
#rglwidget(elementId="plot_flat")
```

Particular todos:

-   systematize the structure vs. relation indexes, right now it's a bit brute force
-   default plotting aesthetics use the `viridis` palette
-   the halo radius is not right . . .

Build notes
-----------

-   Travis needs libproj-dev installed, via .travis.yml (CRAN does not)
-   R-hub needs SystemRequirements: PROJ.4
-   All need export RGL\_USE\_NULL=TRUE
-   plot in examples is fine, but there's a warning from R CMD check in interactive mode (??)

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

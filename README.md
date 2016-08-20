
[![Travis-CI Build Status](https://travis-ci.org/r-gris/trimesh.svg?branch=master)](https://travis-ci.org/r-gris/trimesh) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-gris/trimesh?branch=master&svg=true)](https://ci.appveyor.com/project/r-gris/trimesh) [![Coverage Status](https://img.shields.io/codecov/c/github/r-gris/trimesh/master.svg)](https://codecov.io/github/r-gris/trimesh?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Tidy triangle meshes for spatial data
-------------------------------------

Work in progress, everything very fragile at the moment.

NOTE: there's no sense in the relation of a multi-object Spatial sp structure and the output of 'tri\_mesh'. Everything gets bundled into one "object", so plotting is fine, but thematic colours and so on will come later. Essentially it will have to be done with one rgl object per row in a Spatial\*DataFrame - much like it is done for polygons with polypath.

Particular todos:

-   systematize the structure vs. relation indexes, right now it's just pure luck
-   sort out multi-objects, and creating multi-object rgl scenes for SpDFs
-   default plotting aesthetics, user control
-   why are spheres so artefacty?

Build notes
-----------

Travis needs libproj-dev installed, via .travis.yml (CRAN does not) R-hub needs SystemRequirements: PROJ.4

All need export RGL\_USE\_NULL=TRUE

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

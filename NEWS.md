# anglr 0.5.0

* Fixed beginning of Description, thanks to CRAN feedback. 

* Changed behaviour of `auto_3d()`, now simply an alias to `rgl::aspect3d(1)`
 and any previous used arguments now ignored, with a message. 
 
* Tightened up behaviour of QUAD, being lazy and copying down the z. 

* Remove non-ascii columns (and others) from 'simpleworld', thanks to help by
Michael Chirico @michael_chirico.

* Align to obscured bug fix for silicate, #119. 

* Fixed `mesh_plot()` default colours for a textured mesh. 

* `mesh_plot()` now correctly reprojects a mesh to a `crs` argument, or accepts
coordinates for a plot with `coords` argument.

* Replaced data set `gebco1` with `gebco`, a cleaner version derived from 
GEBCO 2019, at 0.5 resolution and stored as integers. 

* Added degenerate path surface interpolation (i.e. triangulation for POINT or
MULTIPOINT with xy, xyz, xyzm, or xym), wish of
https://github.com/hypertidy/anglr/issues/115.

* Bug fix, now correctly culling invisible triangles for 2D and 3D plots of DEL
and TRI models, as was working for DEL0 and TRI0 (because they don't store
invisible triangles).

* Now providing the full suite of mesh plotting functions from rgl. `plot3d()`,
`dot3d()`, `wire3d()`, and `persp3d()` now work with matrix, raster, sf, sp,
trip, RTriangle, and silicate models. Each of the mesh-surface forms rely on
`as.mesh3d()` conversion behind the scenes, whereas `plot3d()` on linear types
(sf, sp, trip, silicate SC, SC0, PATH, PATH0, and ARC0) all are plotted using
rgl segments without going through a triangulated surface form.

* `as.mesh3d()` has been completely rebuilt, to consolidate code from quadmesh
and angstroms on a firmer basis. `as.mesh3d()` now understands all of the
surface types from sf, sp, raster, and silicate, and can accept a raw matrix as
input. It can also include a `z` argument to extract elevation values from a
raster, and an `image_texture` argument to drape an image from a raster RGB
object onto the surface. Map projections are automatically resolved to the
coordinate system of the `x` argument (as much as possible, there are lingering
issues with the ongoing changes to crs in PROJ library, and the reproj and proj4
packages which attempt to smooth over the changes in Spatial and sf and raster
objects.)
 
 
* `QUAD()` is now exported but still considered experimental. 

* Added experimental `DEL0()` model. 

* Now importing crsmeta, to get sf crs '$proj4string' or '$input'. If input is not a
 proj4string then we'll burn. (Can do manual reprojection until PROJ is here).
 
* Removed QUAD, hopefully temporarily. 

* Support for rgl's `plot3d` and `as.mesh3d` is now greatly improved, and now
 `tmesh3d()` and `qmesh3d()` are used rather than creating these types
 manually. 

* Now using reproj package instead of proj4 directly. 

* New data sets, `cad_tas` and `cont_tas`. 

* Old `anglr()` function now defunct. 

# anglr 0.4.7

* Added a `TRI` method for `QUAD`. 

* `DEL.SC` now removes duplicate vertices like `DEL.PATH` always did, but
triangulation is still done per object since we don't yet have edge to path
logic required for object classification within the mesh.

* Improved the triangulation and triangulation to edges logic.

* The plot methods for QUAD now maps cell value to colour. 

* new QUAD model, for raster data. By default the raster cell values are treated
as a fill aesthetic, and used to provide colour on a flat mesh. The `copy_down`
method for a QUAD requires only one argument will put the cell values on the
geometry for z_. This separation is required especially for more general
geometries like XYZ geocentric, because the cell value and geometric Z are not
necessarily related.
 
* Now re-exporting the magrittr pipe. 

* Added `gebco1` as a built-in global elevation data set, to avoid 
 reading from GeoTIFF. 

* Internal function `anglr_lines` is now deprecated, and points to 
 the silicate and plot3d approach. 
 
* Added `copy_down` generic to dispatch on `sc` and subclasses, to  
 transfer raw values, object column data, or raster values to vertices. 

* Added `plot3d` methods to (eventually) replace `plot(anglr(x))` and `linemesh
with `plot3d(silicate_model)` - currently only `SC` supported and plots as
(object-grouped) edges. Returns `rgl` form silently.

* Removed use of maptools wrld_simpl, replaced by in-built `simpleworld`. 

* Big update for new silicate-based approach, thanks to Andreja Stojic for 
 the feedback. 

* New approach for polygons now using pfft package, identifying triangle
 centroids by polygon. 

# anglr 0.4.6

* new "z = " support in `anglr` for a feature name (to copy as a constant) or a
raster object (to extract values onto vertices), for now the raster must be in
the same coordinate system as the input object

* new `add_normals` argument for plotting triangulations

* rename package (from rangl)

* now faster by relying on silicate for `sf`

# rangl 0.4.5

* release codename Just Work in Master, Dude

* old functions made defunct

* points now have meta table (it was missing), and singular points are now supported

* raster package is now an Import

* added support for RasterLayer

* fixed globe to keep PROJ.4

* quashed a major bug introduced by use of dplyr::distinct, best to use factor
unique classifier on character versions of coords

* several cleanup fixes

# rangl 0.3.0

* rename again, main function is called 'rangl', the term 'mesh' is too often
used across R

* rangl method for trip

* fix for spbabel now means MultiPoints are rangl()-able

# rangl 0.2.0

* removed old globe() plot behaviour, this function now just converts
coordinates to geocentric XYZ

* added generic "mesh()" function to convert SpatialPolygons, SpatialLines, and
rgl 'mesh3d' objects (only those that use triangle primitives)

* deprecated "tri_mesh" function, replaced by mesh()

* renamed package to 'rangl'

* improved code coverage of tests

* infrastructure and tests for globe

* new function globe

* performance improved for hole finding and removal from mesh

* achieved working package scaffolding

# trimesh 0.1.0

* first release to Github



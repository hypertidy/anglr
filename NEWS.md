# anglr dev

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

* quashed a major bug introduced by use of dplyr::distinct, best to use factor unique classifier on character versions of coords

* several cleanup fixes

# rangl 0.3.0

* rename again, main function is called 'rangl', the term 'mesh' is too often used across R 

* rangl method for trip

* fix for spbabel now means MultiPoints are rangl()-able

# rangl 0.2.0

* removed old globe() plot behaviour, this function now just converts coordinates to geocentric XYZ

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




library(mapdeck)
mapdeck() %>% add_mesh(data = melbourne_mesh)
library(anglr)
library(marmap)
library(ceramic)
cc <- cc_elevation(raster::extent(147.5, 147.6, -42.1, -42.0),
                   zoom = 9)

mesh <- reproj::reproj(quadmesh::triangmesh(cc), 4326)
mesh$vb[3, ] <- mesh$vb[3, ] * 35
mesh$material$col <- colourvalues::color_values(mesh$vb[3, mesh$it])
mapdeck() %>% add_mesh(data = mesh)


## from dev anglr
topo_quad <- as.mesh3d(raster::crop(quadmesh::etopo, raster::extent(100, 180, -50, -20)))
#rgl::clear3d();rgl::shade3d(topo_quad);rgl::aspect3d(1, 1, 0.01);  rgl::rglwidget()
topo_triangle <- as.mesh3d(raster::crop(quadmesh::etopo, raster::extent(100, 180, -50, -20)),
                           triangles = TRUE)
#rgl::clear3d();rgl::shade3d(topo_triangle);rgl::aspect3d(1, 1, 0.01);  rgl::rglwidget()

saveRDS(list(topo_quad = topo_quad,
             topo_triangle = topo_triangle), file = "data-raw/mesh3d_examples.rds", compress = "xz")

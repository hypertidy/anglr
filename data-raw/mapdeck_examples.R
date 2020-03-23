
library(mapdeck)
mapdeck() %>% add_mesh(data = melbourne_mesh)
library(anglr)
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
library(sf)
poly <- st_transform(subset(silicate::inlandwaters, Province %in% c("Victoria", "South Australia")),
                     4326) ## reproj via anglr doesn't work atm because WKT2
poly_triangle <- as.mesh3d(silicate::TRI0(poly))
poly_triangle_dense <- as.mesh3d(
  copy_down(anglr::DEL(poly, max_area =.005), quadmesh::etopo)
)
#rgl::clear3d();rgl::shade3d(poly_triangle_dense);rgl::aspect3d(1, 1, 0.3)
saveRDS(list(topo_quad = topo_quad,
             topo_triangle = topo_triangle,
             poly_triangle = poly_triangle,
             poly_triangle_dense = poly_triangle_dense),
        file = "data-raw/mesh3d_examples.rds", compress = "xz")


library(mapdeck)
mapdeck() %>% add_mesh(data = poly_triangle)

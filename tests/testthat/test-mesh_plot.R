context("mesh_plot")
# BasicRaster
# DEL/DEL0
# QUAD
# Spatial
# TRI/TRI0
# matrix (array?)
# mesh3d
# sf/sfc/sfg
# triangulation (the RTriangle one)

m <- volcano[1:10, 20:1]
r <- raster::raster(m)
del <- DEL(silicate::minimal_mesh)

#.raster_to_coords(r)
library(raster)

test_that("mesh_plot works", {
  mesh_plot(r)
  mesh_plot(raster::brick(r, r, r))

  mesh_plot(del)
  mesh_plot(DEL0(del))
  xy <- brick(setValues(r, sort(m)), setValues(r, row(m)))
  mesh_plot(QUAD(r))
  mesh_plot(QUAD(m))

  mesh_plot(r, coords = xy)

  ## realerish example
  grd <- crop(gebco, extent(100, 180, -50, -30))
  mesh_plot(grd, coords = .raster_to_coords(grd))

  x <- raster::aggregate(gebco, fact = 4)
  coords <- .raster_to_coords(x)
  crs <- "+proj=laea +lon_0=147 +lat_0=-43 +datum=WGS84"
  ## it works to give the coords, on their own
  mesh_plot(x,
            coords = coords)
  ## to give the coords and the crs (the crs is relative to the coords, assumed longlat)
  mesh_plot(x,
             coords = coords,
             crs = crs)


  ## to give the crs only
  mesh_plot(x, crs = crs)
})

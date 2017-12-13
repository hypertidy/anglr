## we want to rasterize contours that have an elevation value
library(sf)

x <- read_sf("~/Git/vr_vs_sp/alexander_clark_park/QSC_Extracted_Data_20171212_140035727000-20064/Contours_1_metre.shp")
library(irreg)  ## devtools::install_github("mdsumner/irreg")
library(dplyr)
## L1, L2 are the holes, and islands respectively
coords <- st_coordinates(x) %>% 
  as_tibble() %>%  sample_n(10000)
xy <- coords %>% dplyr::transmute(x = X, y = Y) %>% as.matrix()
value <- x$ELEVATION[dplyr::pull(coords, L2)]
## helper function to build a raster from the extent of a set of points
grid <- defaultgrid(xy, ncol = 120, nrow = 100)

## this function triangulates the points (no account is taken of the lines)
## and uses barycentric interpolation on triangles to interpolate value
## (this is what griddata in Matlab does, default method)
trigrid <- tri_fun(xy,
                   value, 
                   grid)
library(raster)
plot(trigrid)
library(rgl)
library(quadmesh)
#rgl.clear()
shade3d(quadmesh(trigrid), col = "white")
aspect3d(1, 1, 1/100)
#rglwidget()



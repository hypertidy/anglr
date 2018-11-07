dp <- "/rdsi/PUBLIC/raad/data/listdata.thelist.tas.gov.au/opendata/data"
dsn1 <- file.path(dp, "list_parcels_hobart.gdb")
dsn2 <- file.path(dp, "list_5m_contours_hobart.gdb")
layer1 <- "list_parcels_hobart"
layer2 <- "list_5m_contours_hobart"
library(sf)
cad0 <- read_sf(dsn1, layer1)
cont0 <- read_sf(dsn2, layer2 )



library(raster)
# p <- as(extent(cad), "SpatialPolygons")
# projection(p) <- projection(cad)
# library(dismo)
# 
# gm <- gmap(p, type = "satellite")


e <- new("Extent"
         , xmin = 523391.060663871
         , xmax = 524529.793420587
         , ymin = 5249817.21778203
         , ymax = 5250452.78955322
)

bb <- st_bbox(e)
cad_tas <- st_crop(cad0, bb)
cont_tas <- st_cast(st_crop(cont0,bb), "MULTILINESTRING")
usethis::use_data(cad_tas, cont_tas)

context("sf")

## TODO: Rename context
## TODO: Add more tests
library(sf)

pol <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
lin <- st_cast(pol, "MULTILINESTRING")
pnt <- st_cast(lin, "MULTIPOINT")

test_that("rangl sf works", {
  rangl(pol)  
})

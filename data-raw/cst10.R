library(anglr)
## https://github.com/hypertidy/anglr/issues/7#issuecomment-628362518

## fix #155
# prj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +datum=WGS84"
# cst10 <- readRDS(system.file("extdata/cst10_polygon.rds", package = "anglr", mustWork = TRUE))
# cst10@proj4string <- sp::CRS(prj)
# saveRDS(cst10, "inst/extdata/cst10_polygon.rds")

#usethis::use_data(cst10, version = 2)



## download the CCAMLR shapefiles . . .
library(sp)
ssmu0 <- readRDS("data-raw/ssmumap.rds")


library(rangl)
spbabel::sptable(ssmu0)

plot(rangl(ssmu0), specular = "black")


#plot(ssmu[1:5,])
idx <- 1:14
ssmu <- rangl(ssmu0[idx, ], max_area = 5e6)
#print(ssmu$v)
print(ssmu$tXv)
print(all(spbabel::sptable(ssmu0[idx, ])$island_))
library(raster)
topo <- raster("C:/data/Etopo1/ETOPO1_Ice_c_gdal.grd")
ll <- rgdal::project(as.matrix(ssmu$v[, c("x_", "y_")]), ssmu$meta$proj[1], inv = TRUE)
ssmu$v$z <- extract(topo, ll, method = "bilinear")
rm(ll, topo)
gc();gc()

ssmu$v$z_ <- ssmu$v$z * 10
plot(ssmu)


plot(globe(ssmu), specular = "black")
library(rgl);bg3d("lightgrey")

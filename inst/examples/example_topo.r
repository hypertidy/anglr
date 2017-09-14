 library(rworldxtra)
 library(raadtools)
 top <- brick(readAll(readtopo("etopo2")))
 

 data(countriesHigh)
 sv <- c("New Zealand", "Antarctica", "Papua New Guinea", "Indonesia", "Malaysia", "Fiji", "Australia")
 a <- subset(countriesHigh, SOVEREIGNT %in% sv)
 b <- tri_mesh(a, max_area = 0.01)
 

 b$v$z_ <- raster::extract(top, b$v[, c("x_", "y_")], method = "bilinear") 
 
 b$v$z_0 <- b$v$z_
 b$v$z_ <- b$v$z_0 * 5000
 globe(b, rad = 6378137.0, specular = "black");rgl::bg3d("grey")
 
 
 b2 <- b
 b2$v$z_ <- b2$v$z_0 * 50
 
 xy <- rgdal::project(as.matrix(b2$v[, c("x_", "y_")]), "+proj=laea +lon_0=130 +lat_0=-42 +ellps=WGS84")
 b2$v$x_ <- xy[,1]
 b2$v$y_ <- xy[,2]
 
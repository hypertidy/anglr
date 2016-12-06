## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(spbabel)
library(rgdal)
library(dplyr)

## ------------------------------------------------------------------------
#library(rworldmap)
#data(countriesLow)
#p <- countriesLow
library(rgdal)
p <- readOGR(system.file("extdata", "small_world.gpkg", package = "rangl"), "ne_110m_admin_0_countries")
plot(p, col = viridis::viridis(nrow(p)))

## ------------------------------------------------------------------------
library(spbabel)
(pnganz <- subset(p, name %in% c("Australia", "Indonesia", "New Zealand", "Papua New Guinea")))
pnganz$color <- viridis::viridis(nrow(pnganz))
plot(pnganz, col = viridis::viridis(nrow(pnganz)))

## ------------------------------------------------------------------------
plot(as(pnganz, "SpatialLinesDataFrame"))
plot(as(pnganz, "SpatialLinesDataFrame"), col = viridis::viridis(nrow(pnganz), alpha = 0.7), lwd = c(2, 4), add = TRUE)

str(as(geometry(pnganz[1,]), "SpatialLines"))

## ------------------------------------------------------------------------
ptabs <- spbabel::map_table(pnganz)
print(names(ptabs))
print(sapply(ptabs, nrow))

## ------------------------------------------------------------------------
ptabs$o

## ------------------------------------------------------------------------
ptabs$b

## ------------------------------------------------------------------------
ptabs$bXv

## ------------------------------------------------------------------------
ptabs$v

## ------------------------------------------------------------------------
ltabs <- spbabel::map_table(as(pnganz, "SpatialLinesDataFrame"))

for (i in seq_along(ltabs)) {
  writeLines("------------------------------")
  print(ltabs[i])
  print(ptabs[i])
  writeLines("")
  
}

## ------------------------------------------------------------------------
lsegment <- rangl::rangl(as(pnganz, "SpatialLinesDataFrame"))
as.data.frame(lapply(lsegment, nrow))

## ----eval=FALSE,include=FALSE--------------------------------------------
#  library(geosphere)
#  library(rangl)
#  x <- globe(rangl(SpatialMultiPoints(list(randomCoordinates(5e4)),
#                     proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))),
#             "+proj=geocent +a=1")
#  
#  tri <- geometry::convhulln(cbind(x$v$x_, x$v$y_, x$v$z_))
#  rgl::triangles3d(cbind(x$v$x_, x$v$y_, x$v$z_)[t(tri), ],
#                   specular = "black",
#                   color = "skyblue", alpha = 0.4)
#  
#  plot(globe(lsegment, "+proj=geocent +a=1.1"))
#  

## ------------------------------------------------------------------------
par(mar = rep(0, 4))
plot(lsegment$v$x_, lsegment$v$y_, asp = 1, pch = ".", axes = FALSE)
lines(lsegment$v$x_, lsegment$v$y_, col = viridis::viridis(4))

## ------------------------------------------------------------------------
par(mar = rep(0, 4))
plot(lsegment$v$x_, lsegment$v$y_, asp = 1, pch = ".", axes = FALSE)
lsegment$o$color <- viridis::viridis(nrow(lsegment$o))
#segs <- lsegment$l %>% inner_join(lsegment$o %>% select(object_, color)) %>% inner_join(lsegment$lXv) %>% select(color, vertex_, segment_) %>% inner_join(lsegment$v)
segs <- lsegment$lXv %>% inner_join(lsegment$l) %>% inner_join(lsegment$o %>% dplyr::select(object_, color)) %>% dplyr::select(color, vertex_, segment_) %>% inner_join(lsegment$v)
ix <- seq(1, nrow(segs)-1, by  = 2);  segments(segs$x_[ix], segs$y_[ix], segs$x_[ix + 1], segs$y_[ix+1], col = segs$color[ix], lwd = 4)

## ------------------------------------------------------------------------
prim2D <- rangl::rangl(pnganz)
plot(pnganz, border = "black", col = "transparent", lwd = 4)
for (i in seq(nrow(prim2D$t))) {
  tri <- prim2D$t[i, ] %>% inner_join(prim2D$tXv, "triangle_") %>% inner_join(prim2D$v, "vertex_") %>% dplyr::select(x_, y_)
  polygon(tri$x_, tri$y_, border = (prim2D$t[i, ] %>% inner_join(prim2D$o, "object_"))$color[1])
}


## ------------------------------------------------------------------------
library(rgl)
plot(prim2D, specular = "black")
subid <- currentSubscene3d()
rglwidget(elementId="pnganz")
plot(rangl::globe(prim2D), specular = "black")
subid <- currentSubscene3d()
rglwidget(elementId="png_anz_globe")

## ------------------------------------------------------------------------
prim2D$v
rangl::globe(prim2D)$v
subid <- currentSubscene3d()
rglwidget(elementId="prim2D")

## ------------------------------------------------------------------------
rangl::globe(prim2D)$meta[, c("proj", "ctime")]


## ------------------------------------------------------------------------
## TBD

## ------------------------------------------------------------------------
p1 <- cbind(x = c(0, 0, 0.75, 1,   0.5, 0.8, 0.69, 0), 
           y = c(0, 1, 1,    0.8, 0.7, 0.6, 0,    0))
p2 <- cbind(x = c(0.2, 0.2, 0.3, 0.5, 0.5, 0.2), 
            y = c(0.2, 0.4, 0.6, 0.4, 0.2, 0.2))
p4 <- cbind(x = c(0.69, 0.8, 1.1, 1.23, 0.69), 
            y = c(0, 0.6, 0.63, 0.3, 0))
pp <- rbind(p1, NA,  p2[nrow(p2):1, ])
plot(rbind(pp, p4), cex = 1.3, main = "two polygons, shared edge")
polypath(pp, col = "grey")
polypath(p4, col = "firebrick")


## ------------------------------------------------------------------------
#devtools::install_github("edzer/sfr")
library(sf)
library(tibble)
x <- st_as_sf(tibble(a = 1:2, geom = st_sfc(list(st_multipolygon(list(list(p1, p2[rev(seq(nrow(p2))), ]))),  
                                                 st_multipolygon(list(list(p4)))))))
plot(x, col = c("grey", "firebrick"))

## ------------------------------------------------------------------------
library(sp)
spgdf <- as(x, "Spatial")
plot(spgdf, col = c("grey", "firebrick"))

## ------------------------------------------------------------------------
## fortify model
library(dplyr)
meta <- as_tibble(x %>% dplyr::select(-geom) %>% mutate(object_ = row_number()))
map <- spbabel::sptable(spgdf)
library(ggplot2)
ggcols <- ggplot2::scale_fill_manual(values = c("1" = "grey", "2"  = "firebrick")) 
ggplot(map %>% mutate(rn = row_number()) %>% inner_join(meta)) + aes(x = x_, y = y_, group = branch_, fill = factor(object_)) +
  ggcols + ggpolypath::geom_polypath() + geom_path()

## ------------------------------------------------------------------------
library(spbabel)
mp <- map_table(spgdf)
ggcols <- ggplot2::scale_fill_manual(values = setNames(c("grey", "firebrick"), mp$o$object_))
gg <- ggplot(mp$o %>% inner_join(mp$b) %>% inner_join(mp$bXv) %>% inner_join(mp$v))
gg + aes(x = x_, y = y_, group = branch_, fill = object_) + ggcols + ggpolypath::geom_polypath() + geom_path(lwd = 2)

## ------------------------------------------------------------------------
p2seg <- function(x) as_tibble(rangl:::path2seg(x$vertex_))
BxE <- mp$bXv %>% split(.$branch_) %>% purrr::map(p2seg) %>% bind_rows(.id = "branch_") 
ggplot(BxE %>% inner_join(mp$v %>% rename(x = x_, y = y_), c("V1" = "vertex_")) %>% 
                            inner_join(mp$v, c("V2" = "vertex_"))) + 
  geom_segment(aes(x = x, y = y, xend = x_, yend = y_, colour = V1)) #+ guides(colour = FALSE)


uE <- mp$bXv %>% split(.$branch_) %>% purrr::map(p2seg) %>% bind_rows(.id = "branch_")%>% mutate(edge_ = row_number()) %>% 
   mutate(uu = paste(pmin(V1, V2), pmax(V1, V2), sep = "_")) %>% distinct(uu, .keep_all = TRUE)
nodes <- bind_rows(mp$v %>% dplyr::select(vertex_) %>% inner_join(uE, c("vertex_" = "V1")), 
          mp$v %>% dplyr::select(vertex_) %>% inner_join(uE, c("vertex_" = "V2"))) %>% 
  distinct(edge_, vertex_) %>% 
  group_by(vertex_) %>% mutate(nb = n()) %>% ungroup() %>% 
  filter(nb > 2) %>% distinct(vertex_) %>% inner_join(mp$v)


plot(spgdf)
points(nodes$x_, nodes$y_, cex = 0.9)
       

## ------------------------------------------------------------------------
## what are the nodes?
x1 <- st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267)
mp <- spbabel::map_table(x1)
uE <- mp$bXv %>% split(.$branch_) %>% purrr::map(p2seg) %>% bind_rows(.id = "branch_")%>% mutate(edge_ = row_number()) %>% 
   mutate(uu = paste(pmin(V1, V2), pmax(V1, V2), sep = "_")) %>% distinct(uu, .keep_all = TRUE)
nodes <- bind_rows(mp$v %>% dplyr::select(vertex_) %>% inner_join(uE, c("vertex_" = "V1")), 
          mp$v %>% dplyr::select(vertex_) %>% inner_join(uE, c("vertex_" = "V2"))) %>% 
  distinct(edge_, vertex_) %>% 
  group_by(vertex_) %>% mutate(nb = n()) %>% ungroup() %>% 
  filter(nb > 2) %>% distinct(vertex_) %>% inner_join(mp$v)


plot(x1)
points(nodes$x_, nodes$y_, cex = 0.9)

## ------------------------------------------------------------------------
sc <- rangl::rangl(spgdf)

plot(sc)

plot(x)
l1 <- inner_join(sc$o[1, ], sc$t) %>% split(.$triangle_) %>% purrr::map(function(x) inner_join(x, sc$tXv) %>% inner_join(sc$v)) 
j <- lapply(l1, function(x) polygon(cbind(x$x_, x$y_), col = "grey"))
l2 <- inner_join(sc$o[2, ], sc$t) %>% split(.$triangle_) %>% purrr::map(function(x) inner_join(x, sc$tXv) %>% inner_join(sc$v)) 
j <- lapply(l2, function(x) polygon(cbind(x$x_, x$y_), col = "firebrick"))


## this is slow but does show that we have a full and constrained triangulation
 set.seed(75)
sc <- rangl::rangl(as(x1[24:30, ], "Spatial"))
plot(x1[24:30, ], border = "black", lwd = 4)
for (i in seq(nrow(x1))) {
  l1 <- inner_join(sc$o[i, ], sc$t, "object_") %>% split(.$triangle_) %>% purrr::map(function(x) inner_join(x, sc$tXv, "triangle_") %>% inner_join(sc$v, "vertex_")) 
 
  j <- lapply(l1, function(x) polygon(cbind(x$x_, x$y_), border = sample(c("grey", "firebrick", viridis::viridis(5)), 1)))
}


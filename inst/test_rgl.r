## terminology of materials in rgl


library(rgl)

## two distinct polygons
x1 <- cbind(c(0, 0, 1, 1), c(0, 1, 1, 0))

x2 <- cbind(c(1.1, 1.1, 2, 2),  
            c(0, 1, 1, 0) + 0.1)

x3 <- cbind(x2[,1] - 0.5, x2[,2]) * 0.2
plot(rbind(x1, x2, x3))
polypath(rbind(x1, NA, x3, NA, x2))

rgl::triangulate(rbind(x1, NA, x3, NA, x2))


## unique vertices
coords <- cbind(x = c(0, 0, 0.75, 1, 0.5, 0.8, 0.69, 0.2, 0.2, 0.3, 0.5, 0.5, 1.1, 1.23), 
                y = c(0, 1, 1, 0.8, 0.7, 0.6, 0, 0.2, 0.4, 0.6, 0.4, 0.2, 0.63, 0.3)) 

coords <- rbind(coords, cbind(x = c(1.3, 1.3, 1.5, 1.5), y = c(0.5, 0.8, 0.8, 0.5)))
## indexes for each polygon part
## these are all neighbours
## removed duplicate of final / first
i1 <- c(1, 2, 3, 4, 5, 6, 7)
i2 <- c(8, 9, 10, 11, 12) ## OFF  ## holes are reversed for winding rule
i3 <- c(7, 6, 13, 14)

## another island
i4 <- c(15, 16, 17, 18)

plot(coords)
polypath(coords)  ## not what is intended
## note that we only get one colour, we can't differentiate
## in one call like this
polypath(coords[c(i1, NA, i2, NA, i3, NA, i4), ], col = "grey", rule = "evenodd")

index <- c(i1, NA, i3)
d <- setNames(as.data.frame(cbind(coords, 0)[index, ]), c("x", "y", "z"))
rgl::triangulate(d$x, d$y, plot = TRUE)


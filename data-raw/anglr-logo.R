png("data-raw/anglr-logo.png", width = 384, height = 384)
par(mfrow = n2mfrow(64), mar = rep(0, 4), mai = rep(0, 4), bg = "black")
ind <- 3:66
rem <- c(1, 2, 7, 8, 9, #10, 15,
         16,
         49, #50,
         #55,
         56,  57,
         58, 63, 64)
for (i in seq_along(ind)) {
  if (i %in% rem) {
    print(i)
    plot(0, axes = FALSE, xlab = "", ylab = "", type = "n")
  } else {
  mesh_plot(as.mesh3d(QUAD(raster::raster(volcano)),
                      material = list(color = viridis::viridis(ind[i]))), xaxs = "i", yaxs = "i")
  }}

dev.off()

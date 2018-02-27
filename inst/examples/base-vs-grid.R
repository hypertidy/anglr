library(rangl)

r <- plot(rangl(simpleworld))
library(grid)


scl <- function(x) {
  rg <- range(x, na.rm = TRUE)
  (x - rg[1])/diff(rg)
}
idx <- rbind(r$it, r$it[1, ])
x <- scl(r$vb[1, idx])
y <- scl(r$vb[2, idx])
#id <- c(1, 1, 1, 1)
id <- rep(seq_len(ncol(r$it)), each = 4)
grid.newpage()
pushViewport(viewport())
system.time({
grid.path(x, y, id = id, gp = gpar(fill = "grey", col = NA))
})

par(xpd = NA, mar = rep(0, 4), xaxs = "i", yaxs = "i")
library(sp)
system.time(plot(simpleworld, col = "grey", border = NA, asp = ""))


grid.newpage()
pushViewport(viewport())

system.time({
  X <- matrix(x, nrow = 4)
  Y <- matrix(y, nrow = 4)
  cols <- viridis::viridis(100)[scl(colMeans(X)) * 99 + 1 ]
  ones <- c(1, 1, 1, 1)
  for (i in seq(ncol(X))) {
  grid.path(X[,i], Y[,i], id = ones, gp = gpar(fill = cols[i], col = NA))
  }
})

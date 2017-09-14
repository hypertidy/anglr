#devtools::install_github("hypertidy/anglr")
library(anglr)
library(rgl)
library(dplyr)
data("flight_tracks", package = "silicate")
## convert to topological graph
aa <- anglr(flight_tracks)
rgl.points(aa$v[, c("x_", "y_", "z_")], size = 0)
aspect3d(1, 1, 0.05)
## group by M
aa$v <- aa$v %>% mutate(g = cut(m_, 1500, labels = FALSE)) %>% group_by(g)
for (gi in sort(unique(aa$v$g))) {
  try(plot(anglr:::semi_cascade(aa, g == gi,  tables = c("v", "lXv", "l", "o"))))

  #Sys.sleep(0.5)
}

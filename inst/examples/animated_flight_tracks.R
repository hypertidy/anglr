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


inner_cascade <- function(x) {
  tabnames <- silicate:::join_ramp(x)
  tab <- x[[tabnames[1]]]
  for (ni in tabnames[-1L]) tab <- dplyr::inner_join(tab, x[[ni]])
  tab
}

v <- inner_join(flight_tracks$path_link_vertex, flight_tracks$vertex)
library(ggplot2)
ggplot(v %>% group_by(path) %>% mutate(m_ = row_number()) %>% ungroup(), aes(m_, z_, group = path)) + 
  geom_path() + 
  xlim(c(0, 300))

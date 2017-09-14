#' @name plot-anglr
#' @export
play_xyzm <- function(x,  ..., add = FALSE) {
  library(anglr)
  library(rgl)
  library(dplyr)
  data("flight_tracks", package = "silicate")
  ## convert to topological graph
  aa <- anglr(flight_tracks)
  
  ## group by M
  aa$v <- aa$v %>% mutate(g = dplyr::ntile(m_, 50)) %>% group_by(g)
  for (gi in unique(aa$v$g)) {
     plot(anglr:::semi_cascade(aa, g == 1,  tables = c("v", "lXv", "l", "o")))
    rglwidget()
    Sys.sleep(0.5)
  }
  
}
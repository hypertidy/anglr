data("sfzoo", package = "silicate")
library(sf)
sf_data_zoo <- lapply(lapply(sfzoo, st_sfc), function(x) st_as_sf(tibble::tibble(geometry = x, a = 1)))
devtools::use_data(sf_data_zoo)
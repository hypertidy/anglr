simpleworld <- rgeos::gBuffer(rnaturalearth::countries110, width= 0, byid = TRUE)
simpleworld <- simpleworld[c("sovereignt", "continent")]
usethis::use_data(simpleworld)

simpleworld <- rgeos::gBuffer(rnaturalearth::countries110, width= 0, byid = TRUE)
simpleworld <- simpleworld[c("sovereignt", "continent")]
## #155
simpleworld@proj4string <- sp::CRS("+proj=longlat +datum=WGS84")
usethis::use_data(simpleworld)

simpleworld <- rgeos::gBuffer(rnaturalearth::countries110, width= 0, byid = TRUE)
usethis::use_data(simpleworld)

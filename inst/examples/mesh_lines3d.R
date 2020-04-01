nz <- spData::nz
cl <- sfheaders::sf_to_df(
  sf::st_as_sf(raster::rasterToContour(raster::crop(quadmesh::etopo,
                                                    sf::st_transform(nz, 4326)))),
  fill = TRUE)

## contour levels are factor
cl$level <- as.numeric(levels(cl$level)[cl$level])


sfx <- sfheaders::sf_multilinestring(cl, x = "x", y = "y",
                                     z = "level",
                                     multilinestring_id = "multilinestring_id",
                                     linestring_id = "linestring_id")

library(osmdata)
library(sf)
library(sp)

rm(list = ls())

path = "/Users/patrickschulze/Desktop/Master/Semester3/Research_Proj/data/workfiles"

# ---------------------------------------------------------------------------------------------------

# Koordinaten von München's Bezirken laden
q <- osmdata::getbb("munich germany") %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature("boundary", "administrative")
shape_muc_tmp <- osmdata::osmdata_sf(q)$osm_multipolygons
shape_muc_tmp <- shape_muc_tmp[as.character(shape_muc_tmp$admin_level) == 10, ]
shape_muc <- shape_muc_tmp[grepl('Bezirksteil', shape_muc_tmp$name), 2]
plot(shape_muc, main = "Munich")

# Polygonzüge der Grenzen der Bezirke extrahieren
muc_geom <- sf::st_geometry(shape_muc)

# Es gibt zwei sehr kleine "Enklaven" im 2. und 52. Bezirk, welche Probleme verursachen
plot(muc_geom, reset = FALSE)
plot(muc_geom[[2]], col = 'red', add = TRUE)
plot(muc_geom[[52]], col = 'red', add = TRUE)

# Wir entfernen diese Enklaven, um eine 
# durchgezogene Stadtgrenze zu erhalten
muc_geom[[2]][[1]][[2]] <- NULL # Enklave im 2. Distrikt entfernen
muc_geom[[52]][[1]][[2]] <- NULL # Enklave im 52. Distrikt entfernen

# Resultat begutachten
plot(muc_geom, reset = FALSE)
plot(muc_geom[[2]], col = 'red', add = TRUE)
plot(muc_geom[[52]], col = 'red', add = TRUE)

# Nun können alle Bezirksgrenzen zu einem großen Polygon aggregiert werden
munich_pol <- sf::st_union(muc_geom[1:107])
plot(munich_pol)

# Mit diesem kann leicht überprüft werden, ob ein Ort in München liegt
point_in_munich <- c(11.4, 48.15)
point_not_in_munich <- c(11.4, 48.25)
plot(munich_pol, reset = FALSE)
points(x = point_in_munich[1], y = point_in_munich[2], col = "red", pch = 16)
points(x = point_not_in_munich[1], y = point_not_in_munich[2], col = "red", pch = 16)
# Ja
sp::point.in.polygon(point.x = point_in_munich[1], point.y = point_in_munich[2], 
                 pol.x = munich_pol[[1]][[1]][,1], pol.y = munich_pol[[1]][[1]][,2])
# Nein
sp::point.in.polygon(point.x = point_not_in_munich[1], point.y = point_not_in_munich[2], 
                 pol.x = munich_pol[[1]][[1]][,1], pol.y = munich_pol[[1]][[1]][,2])

# ---------------------------------------------------------------------------------------------------

# Polygon speichern
sf::st_write(munich_pol, paste(path, "munich_pol.shp", sep="/"), delete_layer = TRUE)
sf::st_write(shape_muc, paste(path, "shape_muc.shp", sep="/"), delete_layer = TRUE)

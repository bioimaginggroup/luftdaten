library(tidyverse)
library(sf)
library(sp)

rm(list = ls())

path_data_all = "/Users/patrickschulze/Desktop/pm_research_proj/data/all_sensors"
path_workfiles = "/Users/patrickschulze/Desktop/pm_research_proj/data/workfiles"
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

#--------- Daten laden (welche mit download_all_sensors.R heruntergeladen wurden) -----------
# Liste der Sensortypen
types <- list("bme280", "bmp180", "bmp280", "dht22", "ds18b20", "hpm", "htu21d", "laerm", 
                      "pms3003", "pms5003", "pms7003", "ppd42ns", "sds011", "sht31")
# Eine beschädigte Datei muss gelöscht werden (2019-07-25_sds011_sensor_25443.csv)
file.remove(paste(path_data_all, "all_sensors", "sds011/2019-07-25_sds011_sensor_25443", sep="/"))
# Zudem sind Dateien des Sensortyps pms1003 beschädigt (nur 3 Sensoren insgesamt), 
# Deshalb werden Daten dieses Typs nicht geladen

# Erzeuge tibble für jeden Sensortyp (da Spalten verschiedener Typen variieren)
for (sensor in types){
  nam <- sensor
  path_sensortype <- paste(path_data_all, sensor, sep = "/")
  dir_all_filenames <- dir(path_sensortype, full.names = TRUE)
  assign(nam, dir_all_filenames %>% purrr::map_df(readr::read_delim, delim =";"))
}

# Erzeuge Liste der tibbles
sensors <- list(
  "bme280" = bme280, 
  "bmp180" = bmp180, 
  "bmp280" = bmp280, 
  "dht22" = dht22, 
  "ds18b20" = ds18b20, 
  "hpm" = hpm, 
  "htu21d" = htu21d, 
  "laerm" = laerm, 
  "pms3003" = pms3003, 
  "pms5003" = pms5003, 
  "pms7003" = pms7003, 
  "ppd42ns" = ppd42ns, 
  "sds011" = sds011, 
  "sht31" = sht31
)
#--------------------------------------------------------------------------------------------

#------------------------------------- Extract sensors in Munich ----------------------------

# Lade Koordinaten von München's Stadtgrenze (siehe get_munich_boundary.R)
munich_pol <- sf::st_read(paste(path_workfiles, "munich_pol.shp", sep="/"))$geometry

# Rückgabewert 1 falls Ort in München, 0 sonst
is_in_muc <- function(lon, lat){
  sp::point.in.polygon(point.x = lon, point.y = lat, 
                       pol.x = munich_pol[[1]][[1]][,1], pol.y = munich_pol[[1]][[1]][,2])
}
# löscht alle Zeilen (=Sensoren) eines tibbles, wenn diese nicht in München sind
filter_muc <- function(x){
  x %>% dplyr::filter(is_in_muc(lon,lat)==1)
}
# Filter auf Liste aller tibbles anwenden (dauert etwas, ca. 2-3 Min.)
sensors_munich <- sensors %>% purrr::map(filter_muc)

# Die meisten Sensortypen sind nicht in München präsent
# Nur bme280 (pressure), bmp180 (pressure), dht22 (temperature & humidity), und
# sds011 (particle matter) existieren in München:
(not_in_muc <- (sensors_munich %>% purrr::map(nrow))>0)
# Löschen aller Typen (aus Liste der tibbles) welche nicht in München vorhanden sind
sensors_munich <- sensors_munich[not_in_muc]

# Erhalte eindeutige Koordinaten der Sensoren in München
sensors_munich_coords <- sensors_munich %>% 
  purrr::map(function(x) unique(cbind(x$lon,x$lat))) %>%
  (function(l) do.call(rbind, l))

# Plot um Resultat zu checken:
plot(munich_pol)
points(x=sensors_munich_coords, col = "red", pch = 16)

# Extrahiere & speichere Typ-ID Paare der Sensoren in München
sensors_name_id_munich <- purrr::map(seq_along(sensors_munich), 
    function(x,n,i){
      paste(n[[i]], "sensor", unique(x[[i]]$sensor_id), sep = "_")
    }, x = sensors_munich, n = names(sensors_munich)
)
write.table(unlist(sensors_name_id_munich), 
            paste(path, "workfiles", "sensors_munich", sep = "/"),
            row.names = FALSE, col.names = FALSE, sep = "\n", quote = FALSE)
#-------------------------------------------------------------------------------------------------

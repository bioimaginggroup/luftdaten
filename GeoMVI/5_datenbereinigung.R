library(tidyverse)
library(lubridate)
library(spatstat.utils)
rm(list = ls())
path_workfiles <- "data/workfiles"

## toDo:besser automatisieren, schreibe Funktion
## * suche Quantile
## * welche sensoren haben mehr als x% ausreisser
## * gebe liste dieser sensoren plus allgemeine aussreisser zurück

# Sensortypen in München
types <- c("bme280", "bmp180", "dht22", "sds011")
# Daten laden
# Für jeden Sensortyp ein Tibble
# Tibbles werden in Liste gespeichert
filename_in <- paste(path_workfiles, paste0(types, ".csv"), sep ="/")
sensors_munich <- filename_in %>% purrr::map(readr::read_delim, delim=",")
names(sensors_munich) <- types

# Variablen entfernen/generieren
# Nicht benötigte bzw. Variablen mit fehlenden Messwerten entfernen 
# Zur Quantifizierung des Feinstaubs wählen wir PM10(=P1) aus.
sensors_munich$bme280 <-
  sensors_munich$bme280[, c("sensor_id","lat", "lon", "timestamp", "pressure")] %>%
  dplyr::rename(id_bme280 = sensor_id)

sensors_munich$bmp180 <-
  sensors_munich$bmp180[, c("sensor_id","lat", "lon", "timestamp", "pressure")] %>%
  dplyr::rename(id_bmp180 = sensor_id)

sensors_munich$dht22 <-
  sensors_munich$dht22[, c("sensor_id","lat", "lon",
                           "timestamp", "temperature", "humidity")]%>%
  dplyr::rename(id_dht22 = sensor_id)

sensors_munich$sds011 <-
  sensors_munich$sds011[, c("sensor_id","P1", "P2", "lat", "lon", "timestamp")] %>%
  dplyr::rename(pm25 = P2, pm10 = P1, id_sds011 = sensor_id)

# Stunde (1,2...,24) und Tag (1,2,...,21) erstellen
create_time_vars <- function(x){
  x %>%
    dplyr::mutate(hour = lubridate::hour(timestamp)+1, 
                  date = lubridate::date(timestamp)) %>%
    dplyr::mutate(day = lubridate::day(date)) %>% 
    dplyr::mutate(day = day) %>% 
#    dplyr::mutate(day = day+(day<0)*31) %>% 
    dplyr::select(-"timestamp")
}
sensors_munich <- sensors_munich %>% purrr::map(create_time_vars) 
print(sensors_munich)

# Ausreißer entfernen

ggplot(data=sensors_munich$dht22, aes(x=as.factor(hour), y=temperature)) + 
  geom_boxplot() +
  labs(x = "hour")

# Liste für ausreißerbereinigte Daten initialisieren
sensors_munich_cleaned <- list(bme280 = NULL, bmp180 = NULL, dht22 = NULL, sds011 = NULL)

xy<-quantile(sensors_munich$dht22$temperature,c(.01,.99))

# Keine Temperatur unter 8° gemessen
sensors_munich$dht22 %>% 
  dplyr::filter(spatstat.utils::inside.range(temperature, c(-Inf,xy[1])))

#
(dht22_outliers_temperature <- sensors_munich$dht22 %>%
  dplyr::filter(spatstat.utils::inside.range(temperature, c(xy[2], Inf))))

sensor_mean_temperature <- sensors_munich$dht22 %>% dplyr::group_by(id_dht22) %>%
  dplyr::summarize(mean_temperature=mean(temperature))

n_defect <- dht22_outliers_temperature %>% 
  dplyr::group_by(id_dht22) %>% dplyr::tally() %>% 
  dplyr::rename(n_defect = n) %>% dplyr::arrange(desc(n_defect))
n_total <- sensors_munich$dht22 %>%
  dplyr::group_by(id_dht22) %>% dplyr::tally() %>%
  dplyr::filter(id_dht22 %in% n_defect$id_dht22) %>% 
  dplyr::rename(n_total = n) %>% dplyr::arrange(desc(n_total))
percent_defect <- n_defect %>% dplyr::inner_join(n_total, by = "id_dht22") %>%
  dplyr::inner_join(sensor_mean_temperature, by = "id_dht22") %>%
  dplyr::mutate(percent_defect = 100*n_defect/n_total) %>%
  dplyr::arrange(desc(percent_defect))
print(percent_defect, n=5)

outliers_dht22_temperature <-
  !spatstat.utils::inside.range(sensors_munich$dht22$temperature, xy)

quantile(sensors_munich$dht22[,"humidity", drop=TRUE], 0.6, na.rm = TRUE)
xy<-quantile(sensors_munich$dht22$humidity,c(.01,.99),na.rm=TRUE)
# Ausreißer filtern
# Auch hier etwas Toleranz berücksichtigen (nach oben jedoch nicht, da mehr als 100% unmöglich)
(dht22_outliers_humidity <- sensors_munich$dht22 %>%
dplyr::filter(!spatstat.utils::inside.range(humidity, xy)))

sensor_mean_humidity <- sensors_munich$dht22 %>% dplyr::group_by(id_dht22) %>%
  dplyr::summarize(mean_humidity=mean(humidity, na.rm = TRUE))

n_defect <- dht22_outliers_humidity %>% 
  dplyr::group_by(id_dht22) %>% dplyr::tally() %>% 
  dplyr::rename(n_defect = n) %>% dplyr::arrange(desc(n_defect))
n_total <- sensors_munich$dht22 %>%
  dplyr::group_by(id_dht22) %>% dplyr::tally() %>%
  dplyr::filter(id_dht22 %in% n_defect$id_dht22) %>% 
  dplyr::rename(n_total = n) %>% dplyr::arrange(desc(n_total))
percent_defect <- n_defect %>% dplyr::inner_join(n_total, by = "id_dht22") %>%
  dplyr::inner_join(sensor_mean_humidity, by = "id_dht22") %>% 
  dplyr::mutate(percent_defect = 100*n_defect/n_total) %>%
  dplyr::arrange(desc(percent_defect))
print(percent_defect, n = 8)

# Mittelwert je Tag dieser Sensoren im Vergleich
# zum Gesamtmittelwert (um eindeutige Ausreißer bereinigt) plotten
sensors_munich$dht22 %>%
  dplyr::filter(id_dht22 != "827", spatstat.utils::inside.range(humidity, xy)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarize(mean_humidity = mean(humidity, na.rm = TRUE)) %>%
  plot(type = "l", ylim=c(0,150), col = 2, ylab = "daily mean humidity")
axis(1,at=1:24)
abline(h = 100, col = "red", lty = 3) 
abline(h = 26, col = "red", lty = 3)

ids_defect <- percent_defect[2:5,] %>% dplyr::pull(id_dht22)
i=3
for (id in ids_defect) {
  sensors_munich$dht22 %>% dplyr::filter(id_dht22 == id) %>%
    dplyr::group_by(day) %>% 
    dplyr::summarize(mean_humidity = mean(humidity, na.rm =TRUE)) %>%
    lines(col = i)
i <- i+1 }
legend("topright", c("all sensors", paste0("ID = ", ids_defect)), fill = 2:6)

del_12345 <- sensors_munich$dht22$id_dht22 %in% unlist(percent_defect[1:5,"id_dht22"])

del_single <- !spatstat.utils::inside.range(sensors_munich$dht22$humidity, xy)
# Alle Ausreißer für Luftfeuchtigkeit
outliers_dht22_humidity <- del_12345 | del_single

 # Alle Ausreißer des Typs dht22 (Temperatur und Luftfeuchtigkeit)
outliers_dht22 <- (outliers_dht22_humidity | outliers_dht22_temperature)
length(which(outliers_dht22))/length(sensors_munich$dht22$humidity)

# Entfernen
sensors_munich_cleaned$dht22 <- sensors_munich$dht22[!outliers_dht22,]

## Ausreißer Luftdruck

(interval_outliers1 <- quantile(
  sensors_munich$bme280[,"pressure", drop=TRUE], c(0.02,0.98), na.rm = TRUE))

bme280_outliers_pressure <- sensors_munich$bme280 %>%
  dplyr::filter(!(spatstat.utils::inside.range(pressure,interval_outliers1)))

sensor_mean_bme280 <- sensors_munich$bme280 %>% dplyr::group_by(id_bme280) %>%
  dplyr::summarize(mean_bme280=mean(pressure, na.rm = TRUE))

n_defect <- bme280_outliers_pressure %>% 
  dplyr::group_by(id_bme280) %>% dplyr::tally() %>% 
  dplyr::rename(n_defect = n) %>% dplyr::arrange(desc(n_defect))
n_total <- sensors_munich$bme280 %>%
  dplyr::group_by(id_bme280) %>% dplyr::tally() %>%
  dplyr::filter(id_bme280 %in% n_defect$id_bme280) %>% 
  dplyr::rename(n_total = n) %>% dplyr::arrange(desc(n_total))
percent_defect <- n_defect %>% dplyr::inner_join(n_total, by = "id_bme280") %>%
  dplyr::inner_join(sensor_mean_bme280, by = "id_bme280") %>% 
  dplyr::mutate(percent_defect = 100*n_defect/n_total) %>%
  dplyr::arrange(desc(percent_defect))
print(percent_defect, n=5)

# Entferne Sensor mit ID 29239 komplett
del1 <- sensors_munich$bme280$id_bme280 == "66074"
# Entferne zudem alle einzelnen Ausreißer
del_single <- !spatstat.utils::inside.range(sensors_munich$bme280$pressure,
                                            interval_outliers1)
del <- del1 | del_single

length(which(del))/length(sensors_munich$bme280$pressure)
# Löschen
sensors_munich_cleaned$bme280 <- sensors_munich$bme[!del,]

(interval_outliers2 <- quantile(
  sensors_munich$bmp180[,"pressure", drop=TRUE], c(0.02,0.98), na.rm = TRUE))

bmp180_outliers_pressure <- sensors_munich$bmp180 %>%
  dplyr::filter(!(spatstat.utils::inside.range(pressure,interval_outliers2)))

sensor_mean_bmp180 <- sensors_munich$bmp180 %>% dplyr::group_by(id_bmp180) %>%
  dplyr::summarize(mean_bmp180=mean(pressure, na.rm = TRUE))

n_defect <- bmp180_outliers_pressure %>% 
  dplyr::group_by(id_bmp180) %>% dplyr::tally() %>% 
  dplyr::rename(n_defect = n) %>% dplyr::arrange(desc(n_defect))
n_total <- sensors_munich$bmp180 %>%
  dplyr::group_by(id_bmp180) %>% dplyr::tally() %>%
  dplyr::filter(id_bmp180 %in% n_defect$id_bmp180) %>% dplyr::rename(n_total = n) %>%
  dplyr::arrange(desc(n_total))
percent_defect <- n_defect %>% dplyr::inner_join(n_total, by = "id_bmp180") %>%
  dplyr::inner_join(sensor_mean_bmp180, by = "id_bmp180") %>% 
  dplyr::mutate(percent_defect = 100*n_defect/n_total) %>%
  dplyr::arrange(desc(percent_defect))
print(percent_defect, n=5)

# Einzelne Ausreißer
del <- !spatstat.utils::inside.range(sensors_munich$bmp180$pressure, interval_outliers2)
# ca. 4% werden geläscht
length(which(del))/length(sensors_munich$bmp180$pressure)
# Löschen
sensors_munich_cleaned$bmp180 <- sensors_munich$bmp180[!del,]

quantile(sensors_munich$sds011[,"pm25", drop=TRUE], 0.999, na.rm=TRUE)

pm25_outliers <- sensors_munich$sds011 %>% 
  dplyr::filter(spatstat.utils::inside.range(pm25, c(100, Inf)))

sensor_mean_pm25 <- sensors_munich$sds011 %>% dplyr::group_by(id_sds011) %>%
  dplyr::summarize(mean_pm25=mean(pm25))

n_defect <- pm25_outliers %>%
  dplyr::group_by(id_sds011) %>% dplyr::tally() %>% 
  dplyr::rename(n_defect = n) %>% dplyr::arrange(desc(n_defect))
n_total <- sensors_munich$sds011 %>%
  dplyr::group_by(id_sds011) %>% dplyr::tally() %>%
  dplyr::filter(id_sds011 %in% n_defect$id_sds011) %>% dplyr::rename(n_total = n) %>%
  dplyr::arrange(desc(n_total))
(percent_defect <- n_defect %>% dplyr::inner_join(n_total, by = "id_sds011") %>%
    dplyr::inner_join(sensor_mean_pm25, by = "id_sds011") %>% 
    dplyr::mutate(percent_defect = 100*n_defect/n_total) %>%
    dplyr::arrange(desc(percent_defect)))

# Plotte Mittelwert je Stunde für potentiell defekte Sensoren
sensors_munich$sds011 %>%
  dplyr::group_by(hour) %>% dplyr::summarize(mean_pm25 = mean(pm25)) %>% 
  plot(type = "l", ylim=c(0,500), col = 2, ylab = "hourly mean pm25")
ids_defect <- percent_defect[2:7,] %>% pull(id_sds011)
i=3
for (id in ids_defect) {
  sensors_munich$sds011 %>% dplyr::filter(id_sds011 == id) %>% 
    dplyr::group_by(hour) %>% dplyr::summarize(mean_pm25 = mean(pm25)) %>% 
    lines(col = i)
  i <- i+1 
}
legend("topright", c("all sensors", paste0("ID = ", ids_defect)), fill = 2:8)


# Gleiche Prozedur für Mittelwert je Tag
sensors_munich$sds011 %>%
  dplyr::group_by(day) %>% dplyr::summarize(mean_pm25 = mean(pm25)) %>% 
  plot(type = "l", ylim=c(0,500), col = 2, ylab = "daily mean pm25")
axis(1,at=1:24)

ids_defect <- percent_defect[2:7,] %>% pull(id_sds011)
i=3
for (id in ids_defect) {
  sensors_munich$sds011 %>% filter(id_sds011 == id) %>% 
    dplyr::group_by(day) %>% dplyr::summarize(mean_pm25 = mean(pm25)) %>% 
    lines(col = i)
  i <- i+1
}
legend("topright", c("all sensors", paste0("ID = ", ids_defect)), fill = 2:8)

# Sensoren 1-4 (IDs 8135, 4252, 13220)
#del_1234 <- sensors_munich$sds011$id_sds011 %in% ids_defect[1:4]
# Sensor 5 (ID 4676) für Tage 1-6
#del_5 <- (sensors_munich$sds011$id_sds011 %in% ids_defect[5]) & (sensors_munich$sds011$day %in% 1:6)
# Sensor 6 (ID 10793) für Tage 18-21
#del_6 <- (sensors_munich$sds011$id_sds011 %in% ids_defect[6]) & (sensors_munich$sds011$day %in% 18:21)

del_single <- sensors_munich$sds011$pm25 >= 50

del_na <- is.na(sensors_munich$sds011$pm25)

quantile(sensors_munich$sds011[,"pm10", drop=TRUE], 0.999, na.rm=TRUE)

pm10_outliers <- sensors_munich$sds011 %>% 
  dplyr::filter(spatstat.utils::inside.range(pm10, c(100, Inf)))

sensor_mean_pm10 <- sensors_munich$sds011 %>% dplyr::group_by(id_sds011) %>%
  dplyr::summarize(mean_pm10=mean(pm10))

n_defect <- pm10_outliers %>%
  dplyr::group_by(id_sds011) %>% dplyr::tally() %>% 
  dplyr::rename(n_defect = n) %>% dplyr::arrange(desc(n_defect))
n_total <- sensors_munich$sds011 %>%
  dplyr::group_by(id_sds011) %>% dplyr::tally() %>%
  dplyr::filter(id_sds011 %in% n_defect$id_sds011) %>% dplyr::rename(n_total = n) %>%
  dplyr::arrange(desc(n_total))
(percent_defect <- n_defect %>% dplyr::inner_join(n_total, by = "id_sds011") %>%
    dplyr::inner_join(sensor_mean_pm10, by = "id_sds011") %>% 
    dplyr::mutate(percent_defect = 100*n_defect/n_total) %>%
    dplyr::arrange(desc(percent_defect)))

# Plotte Mittelwert je Stunde für potentiell defekte Sensoren
sensors_munich$sds011 %>%
  dplyr::group_by(hour) %>% dplyr::summarize(mean_pm10 = mean(pm10)) %>% 
  plot(type = "l", ylim=c(0,500), col = 2, ylab = "hourly mean pm10")
ids_defect <- percent_defect[2:7,] %>% pull(id_sds011)
i=3
for (id in ids_defect) {
  sensors_munich$sds011 %>% dplyr::filter(id_sds011 == id) %>% 
    dplyr::group_by(hour) %>% dplyr::summarize(mean_pm10 = mean(pm10)) %>% 
    lines(col = i)
  i <- i+1 
}
legend("topright", c("all sensors", paste0("ID = ", ids_defect)), fill = 2:8)


# Gleiche Prozedur für Mittelwert je Tag
sensors_munich$sds011 %>%
  dplyr::group_by(day) %>% dplyr::summarize(mean_pm10 = mean(pm10)) %>% 
  plot(type = "l", ylim=c(0,500), col = 2, ylab = "daily mean pm10")
axis(1,at=1:24)

ids_defect <- percent_defect[2:7,] %>% pull(id_sds011)
i=3
for (id in ids_defect) {
  sensors_munich$sds011 %>% filter(id_sds011 == id) %>% 
    dplyr::group_by(day) %>% dplyr::summarize(mean_pm10 = mean(pm10)) %>% 
    lines(col = i)
  i <- i+1
}
legend("topright", c("all sensors", paste0("ID = ", ids_defect)), fill = 2:8)

# Sensoren 1-4 (IDs 8135, 4252, 13220)
#del_1234 <- sensors_munich$sds011$id_sds011 %in% ids_defect[1:4]
# Sensor 5 (ID 4676) für Tage 1-6
#del_5 <- (sensors_munich$sds011$id_sds011 %in% ids_defect[5]) & (sensors_munich$sds011$day %in% 1:6)
# Sensor 6 (ID 10793) für Tage 18-21
#del_6 <- (sensors_munich$sds011$id_sds011 %in% ids_defect[6]) & (sensors_munich$sds011$day %in% 18:21)

del_single10 <- sensors_munich$sds011$pm10 >= 100

del_na10 <- is.na(sensors_munich$sds011$pm10)

outliers_sds011 <- del_single | del_na | del_single10 | del_na10
# Anteil der Ausreißer bestimmen
length(which(outliers_sds011))/length(sensors_munich$sds011$pm25)
# Enfternen
sensors_munich_cleaned$sds011 <- sensors_munich$sds011[!outliers_sds011,]


# Funktion um Mittelwert pro Stunde und Sensor für Variable var eines tibbles x zu berechn
hourly_mean_per_sensor <- function(x, var){ 
  x %>%
    dplyr::group_by(lat, lon, day, hour) %>% 
    dplyr::summarize_at(dplyr::vars(var), mean, na.rm=TRUE) %>% 
    dplyr::ungroup()
}
# Funktion um Mittelwert pro Stunde für Variable var eines tibbles x zu berechnen
hourly_mean_agg <- function(x, var){ 
  x %>%
    dplyr::group_by(day, hour) %>% 
    dplyr::summarize_at(dplyr::vars(var), mean, na.rm=TRUE) %>% 
    dplyr::ungroup()
}
# Schlüssel kreieren, um verschiedene Sensortypen anzuspielen
make_key1 <- function(x){ 
  x %>%
    dplyr::mutate(key1 = paste(lat, lon, day, hour, sep="-")) 
}

make_key2 <- function(x){
  x %>%
    dplyr::mutate(key2 = paste(day, hour, sep="-")) 
}
# Funktion anwenden, um Mittelwert pro Stunde und Sensor für Sensortypen 
# sds011 (PM10) und dht22 (Temperatur, Feuchtigkeit) zu berechnen 
new_data1 <- sensors_munich_cleaned[c("sds011", "dht22")] %>%
  purrr::map2(list(c("pm25", "pm10"), c("temperature", "humidity")), 
              hourly_mean_per_sensor) %>%
  purrr::map(make_key1) %>% 
  purrr::map(make_key2)
# Aggregierte PM10- und Temperatur-/Feuchtigkeitsdaten vor dem Mergen:
new_data1[c("sds011", "dht22")]
# Für bmp180 und bme280 zu wenige Sensoren vorhanden.
# Beim mergen auf Sensorebene würden daher fast alle Sensoren verloren gehen.
# Deshalb Berechnung des Mittelwerts für diese Sensortypen aggregiert über alle Sensoren
sensors_munich_cleaned$bme280 <- sensors_munich_cleaned$bme280 %>%
  dplyr::rename(pressure1 = pressure) 
sensors_munich_cleaned$bmp180 <- sensors_munich_cleaned$bmp180 %>%
  dplyr::rename(pressure2 = pressure)
new_data2 <- sensors_munich_cleaned[c("bme280", "bmp180")] %>%
  purrr::map2(list("pressure1", "pressure2"), 
              hourly_mean_agg) %>%
  purrr::map(make_key2)
# Da beide Sensoren Luftdruck messen, wird Mittelwert beider Sensortypen verwendet 
# Falls für einen der beiden Sensoren Wert fehlt, nehme anderen Wert
new_data2 <- new_data2$bme280 %>%
  dplyr::full_join(new_data2$bmp180) %>% 
  dplyr::mutate(pressure = 0.5*pressure1+0.5*pressure2) %>% 
  dplyr::select(-c("pressure1", "pressure2"))

# Tibbles Mergen
new_data <- new_data1$sds011 %>% 
  dplyr::inner_join(new_data1$dht22) %>%
  dplyr::select(-"key1") %>% 
  dplyr::left_join(new_data2)

# Transformationen und zeitlich-räumliche Indizierung der Daten

# Chronologischen Zeitindex aus Tag und Stunde erstellen, 
# da wir später einen AR(1)-Prozess annehmen werden 
new_data <- new_data %>% mutate(t = hour+24*(day-1))

# Neue ID kreieren für alle Messungen am gleichen Ort
new_data <- new_data %>% dplyr::group_by(lon,lat) %>% 
  mutate(ID = group_indices()) %>% dplyr::ungroup()

# Koordinaten in metrisches Referenzsystem transformieren
coord_metric <- new_data[,c("lon","lat")] %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% # lat/lon sf-Objekt erstellen
  sf::st_transform(25832) %>% # CRS von WGS84 zu UTM Zone 32 (Nähe München) transformieren
  sf::st_coordinates() # metrische Koordinaten aus sf-Objekt extrahieren
# Im Datensatz lat/lon Koordinaten durch metrische Koordinaten ersetzen
# Zudem km statt m verwenden, um numerische Probleme beim Fitten zu vermeiden 
new_data <- new_data %>% dplyr::mutate(X = coord_metric[,1]/1000,
                                       Y = coord_metric[,2]/1000)

(new_data <- new_data %>%
   dplyr::select(c("ID", "t", "pm25", "pm10", "X", "Y", "lon", "lat",
                   "day", "hour", "temperature", "humidity",
                   "pressure")) %>%
   arrange(t,ID))

# Daten speichern
filename_out <- paste(path_workfiles, "pm_all.csv", sep ="/") 
write.table(new_data, file = filename_out, sep = ",", row.names = FALSE)



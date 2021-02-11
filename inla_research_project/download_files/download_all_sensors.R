library(xml2)
library(rvest)
library(httr)
library(tidyverse)

rm(list = ls())

date = "2019-07-25"
path_in = "/Users/patrickschulze/Desktop/Master/Semester3/Research_Proj"
path_out = "/Users/patrickschulze/Desktop/Master/Semester3/Research_Proj/data/all_sensors"
url = paste0("http://archive.luftdaten.info/",date,"/")

# -------------------------------------------------------------------------------------------
types <- c("bme280", "bmp180", "bmp280", "dht22", "ds18b20", "hpm", "htu21d", "laerm", 
             "pms1003", "pms3003", "pms5003", "pms7003", "ppd42ns", "sds011", "sht31")

# Ordner für jeden Sensortyp anlegen (nur bei der ersten Ausführung des Skripts)
# purrr::map(paste(path_out, types, sep = "/"), dir.create)

# -------------------------------------------------------------------------------------------
# ------------------------------ Download vorbereiten ---------------------------------------
# -------------------------------------------------------------------------------------------

# XPath Anfrage kreieren, welche alle potentiellen Sensoren auf der Website enthält
sensors_query <- paste(date, types, "sensor", sep ="_")
xpq_vec <- paste0("contains(@href, '", sensors_query, "')")
xpq <- paste0(".//a[", paste(xpq_vec, collapse = " or "), "]")

# Links aller tatsächlich auf der Website vorhandenen Sensoren abspeichern
pg <- xml2::read_html(url)
fils <- rvest::html_nodes(pg, xpath = xpq)
href <- paste0(url, rvest::html_attr(fils, "href"))

# Filenamen aus Link kreieren
filename <- gsub(paste0(".*\\info/", date, "/(.*)\\..*"), "\\1", href)

# Paare Filename-Link erzeugen
download_names <- tibble::tibble(
  filename = filename,
  href = href
)

# Funktion schreiben um Download-Fortschritt anzuzeigen
dot_every <- function(f, n) {
  force(f)
  force(n)
  
  i <- 0
  function(...) {
    i <<- i + 1
    if (i %% n == 0) cat(i, "\n")
    f(...)
  }
}

# -------------------------------------------------------------------------------------------
# --------------------------------------- Download  -----------------------------------------
# -------------------------------------------------------------------------------------------

# Sensoren downloaden: Sensoren werden automatisch in Ordnern je nach Sensortyp gespeichert
for (type in types) {
  s <- grep(type, download_names$filename)
  n_total <- length(s)
  cat("# of ", type, " sensors downloaded (", n_total, " in total):", "\n", sep="")
  purrr::walk2(
    download_names$href[s], paste(path_out, type, download_names$filename[s], sep="/"), 
    download.file %>% dot_every(50), 
    quiet = TRUE
  )
  cat(n_total, "\n")
}
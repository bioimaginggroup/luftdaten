library(xml2)
library(rvest)
library(httr)
library(tidyverse)
library(foreach)

rm(list = ls())

# Zeitraum spezifizieren
start_date = as.Date("2021-11-01")
end_date = as.Date("2021-11-30")

path_workfiles = "data/workfiles"
path_data = "data/munich_sensors"

# -------------------------------------------------------------------------------------------
# ------------------------------ Download vorbereiten ---------------------------------------
# -------------------------------------------------------------------------------------------

# Sensortypen in München
types <- c("bme280", "bmp180", "dht22", "sds011")

# Ordner für jeden Sensortyp anlegen (nur bei der ersten Ausführung des Skripts)
purrr::map(paste(path_data, types, sep = "/"), dir.create)

# Textdatei mit Sensoren in München einlesen
sensors_munich <- read.table(paste0(path_workfiles,"/sensors_munich.txt"))

# Funktion schreiben um Download-Fortschritt anzuzeigen
show_every <- function(f, n) {
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
range <- as.character(seq(start_date, end_date, by = 1))

cl <- parallel::makeCluster(8)
doParallel::registerDoParallel(cl)
foreach(date_i=range)%dopar%{
  
  library(tidyverse)
  cat("day: ", date_i, "\n")
  
  # XPath Anfrage welche alle Sensoren in München enthält
  sensors_query <- paste(date_i, unlist(sensors_munich), sep ="_")
  xpq_vec <- paste0("contains(@href, '", sensors_query, ".csv')")
  xpq <- paste0(".//a[", paste(xpq_vec, collapse = " or "), "]")
  
  # url des jeweiligen Tages 
  url = paste0("http://archive.luftdaten.info/",date_i,"/")
  
  # Liste mit links aller Sensoren erzeugen 
  pg <- xml2::read_html(url)
  fils <- rvest::html_nodes(pg, xpath = xpq)
  href <- paste0(url, rvest::html_attr(fils, "href"))
  
  # Prüfe welche Sensoren für den Tag in München verfügbar und speichere Position falls verfügbar
  psn <- unlist(purrr::map(href, stringr::str_which, paste0(sensors_query,".csv")))
  
  # Paare Filename-Link erzeugen
  download_names <- tibble::tibble(
    filename = sensors_query[psn],
    href = href
  )
  
  # Eigentlicher Download
  for (type in types) {
    s <- grep(type, download_names$filename)
    n_total <- length(s)
    cat("# of ", type, " sensors downloaded (", n_total, " in total):", "\n", sep="")
    hrefs <- download_names$href[s]
    paths <- paste(path_data, type, download_names$filename[s], sep="/")
    purrr::walk2(
      hrefs, paths,
      download.file %>% show_every(10), 
      quiet = TRUE
    )
    cat(n_total, "\n")
  }
}

#-------------------------------------------------------------------------------------------
#---------------------------- Daten zusammenfügen und speichern ----------------------------
#-------------------------------------------------------------------------------------------

# Daten laden, welche zuvor heruntergeladen wurden 
# Tibble für jeden Sensortyp erstellen und abspeichern 
# Output z.B. bme280.csv im workfile Verzeichnis
for (type in types){
  # laden
  nam <- type
  path_sensortype <- paste(path_data, type, sep = "/")
  dir_all_filenames <- dir(path_sensortype, full.names = TRUE)
  assign(nam, dir_all_filenames %>% purrr::map_df(readr::read_delim, delim =";", show_col_types=FALSE))
  # speichern
  filename_out <- paste(path_workfiles, paste0(type, ".csv"), sep="/")
  write.table(get(nam), file = filename_out, sep = ",", row.names = FALSE)
}

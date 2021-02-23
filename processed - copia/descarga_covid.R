#Clear all
rm(list = ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)

setwd("C:/Users/Valeria/Documents/GitHub/EstrategiaVacunacionMX/data/code/")

#Descarga de la base principal
site.covid <- paste0("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip")
temp <- tempfile()
download.file(site.covid, temp)
dats <- read_csv(unz(temp, unzip(temp, list = TRUE)["Name"]))
unlink(temp)
  
#Descarga de diccionario de datos para ver el nombre del estado
temp <- tempfile()
site.covid.dic <- paste0("http://datosabiertos.salud.gob.mx/gobmx/salud/",
                         "datos_abiertos/diccionario_datos_covid19.zip")
download.file(site.covid.dic, temp)
filenames <- unzip(zipfile=temp, list = TRUE)
fname     <- filenames[which(str_detect(filenames$Name, "Cat.*logo.*")),"Name"]
unzip(zipfile=temp, files = fname, exdir=".")
diccionario.covid <- read_excel(fname,
                                sheet = "Catálogo de ENTIDADES")
unlink(temp)
  
write_rds(diccionario.covid, "diccionario_covid.rds")
write_rds(dats, "dats_covid.rds")

if (dir.exists("diccionario_datos_covid19")){
  unlink("diccionario_datos_covid19", recursive = TRUE)
}

#Pegar para poder juntar
diccionario.covid <- diccionario.covid %>% 
  dplyr::rename(ENTIDAD_RES = CLAVE_ENTIDAD)

#Juntamos nombre del estado
dats <- dats %>% 
  left_join(diccionario.covid, by = "ENTIDAD_RES")


beepr::beep(3)

# 10 de Febrero 2021
# Código para encontrar y acomodar las bases de datos necesarias para análisis
# vacunación en México
#-------------------------------------------
# Base de datos 1 
# Contiene la población por municipio y estado divido por grupo de edad
#
# Base de datos 2
# Contiene el numero de gente con sintomas, hospitalizados(ingreso) y fallecida
#
# Base de datos 3
# Es una combinación de las dos anteriores. Contiene la población por estado 
# además de el conteo de gete con sintomas, hospitalizados y fallecida
#
# Autora : Valeria Pérez



rm(list = ls())
setwd("~/GitHub/EstrategiaVacunacionMX")
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library("rio")
library(dplyr)
library(data.table)
memory.limit(9999999999)     #Solo correr si la computadora dice que no puede encontrar vectores de x tamaño 

setwd("~/GitHub/EstrategiaVacunacionMX/data/processed")

# ---------- Primero las BASES DE DATOS DE ENTIDAD Y MUNICIPIO --------

nombres_entidades <- read_excel("~/GitHub/EstrategiaVacunacionMX/data/raw/Catalogo_Entidad.xlsx")
nombres_municipios <- read_excel("~/GitHub/EstrategiaVacunacionMX/data/raw/Catalogo_Municipio.xlsx")

nombres_entidades <- nombres_entidades %>%
  select(-ABREVIATURA)

nombres_entidades <- nombres_entidades %>%
  mutate(ENTIDAD_FEDERATIVA = if_else(ENTIDAD_FEDERATIVA == "MÉXICO", "ESTADO DE MÉXICO", ENTIDAD_FEDERATIVA))

#---------------- Primera base de datos-------------
# Fuente: https://www.inegi.org.mx/sistemas/Olap/Proyectos/bd/censos/cpv2020/pt.asp
# Censo de Población y Vivienda 2020
# INEGI


datos <- read_excel("~/GitHub/EstrategiaVacunacionMX/data/raw/Base_datos_Poblacion.xlsx")

datos <- datos [-c(nrow(datos), (nrow(datos) - 1)), ]

datos <- datos %>%
  mutate(`No especificado` = replace_na(`No especificado`, 0))

#Primero convertimos todo a mayusculas
datos <- mutate_if(datos, is.character, toupper)

#Acomodamos estado por clave
datos <- left_join(datos, nombres_entidades, 
                   by = c("Estado" = "ENTIDAD_FEDERATIVA"))

#Acomodados municipios por claves
datos <- left_join (nombres_municipios, datos,
                    by = c("CLAVE_ENTIDAD" = "CLAVE_ENTIDAD",
                    "MUNICIPIO" = "Municipio"))


export(datos, "datos_pob.rds")

#---------------Segunda base de datos ---------------

#-----------En caso de que no este descargada la base de datos correr el siguiente código

#### INICIO DESCARGA ####

#Descarga de la base principal
site.covid <- paste0("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip")
temp <- tempfile()
download.file(site.covid, temp)
dats <- read_csv(unz(temp, unzip(temp, list = TRUE)["Name"]))
unlink(temp)

#Descarga de diccionario de datos para ver el nombre del estado
    # temp <- tempfile()
    # site.covid.dic <- paste0("http://datosabiertos.salud.gob.mx/gobmx/salud/",
    #                          "datos_abiertos/diccionario_datos_covid19.zip")
    # download.file(site.covid.dic, temp)
    # filenames <- unzip(zipfile=temp, list = TRUE)
    # fname     <- filenames[which(str_detect(filenames$Name, "Cat.*logo.*")),"Name"]
    # unzip(zipfile=temp, files = fname, exdir=".")
    # diccionario.covid <- read_excel(fname,
    #                                 sheet = "Catálogo de ENTIDADES")
    # unlink(temp)
    # 
    # write_rds(diccionario.covid, "diccionario_covid.rds")
    # write_rds(dats, "dats_covid.rds")
    # 
    # if (dir.exists("diccionario_datos_covid19")){
    #   unlink("diccionario_datos_covid19", recursive = TRUE)
    # }
    # 
    # #Pegar para poder juntar
    # diccionario.covid <- diccionario.covid %>% 
    #   dplyr::rename(ENTIDAD_RES = CLAVE_ENTIDAD)
    # 
    # #Juntamos nombre del estado
    # dats <- dats %>% 
    #   left_join(diccionario.covid, by = "ENTIDAD_RES")
    # 
    # 
    # beepr::beep(3)

#### FIN DESCARGA #####

datos_2 <- dats
max_fechas <- max(datos_2$FECHA_INGRESO)
seq_fechas <- seq(ymd("2020/01/01"),max_fechas, by = 1)

#Selecciono las columnas para la muestra
datos_2 <- datos_2 %>%
  #mutate(ENTIDAD_RES = as.factor(ENTIDAD_RES)) %>%                  
  #mutate(MUNICIPIO_RES =  as.factor(MUNICIPIO_RES))  %>%                
  mutate(FECHA_INGRESO =   as.Date(FECHA_INGRESO)) %>%          
  mutate(FECHA_DEF = as.Date(FECHA_DEF)) %>%
  mutate(CLASIFICACION_FINAL = as.factor(CLASIFICACION_FINAL)) %>%
  mutate(TIPO_PACIENTE = as.factor(TIPO_PACIENTE)) %>% #Hago las columnas del tipo correcto
  mutate( FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS)) %>%
  select(ENTIDAD_RES, MUNICIPIO_RES, FECHA_INGRESO, EDAD, FECHA_SINTOMAS,
         CLASIFICACION_FINAL, FECHA_DEF, TIPO_PACIENTE ) %>%           #Selecciono las columnas que necesito
  arrange(FECHA_INGRESO, MUNICIPIO_RES, ENTIDAD_RES)             #Acomodo de acuerdo a la fecha, municipio y entidad



edadbreaks <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 500)
edadlabels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                "75-79", "80+")
setDT(datos_2)[ , EDAD_GRUPOS := cut(EDAD,
                                     breaks = edadbreaks,
                                     right = FALSE,
                                     labels = edadlabels)]
datos_2 <- datos_2 %>%
  mutate(EDAD_GRUPOS = as.factor(EDAD_GRUPOS)) %>%
  select(-EDAD) 

# ----------------------------------------------------------------------

#Para ------DEFUNCIONES------- 

#Defunciones con todo mundo
#Contamos 
prueba <- datos_2 %>%
  rename(FECHA = FECHA_DEF) %>%
  group_by(ENTIDAD_RES, MUNICIPIO_RES, EDAD_GRUPOS, FECHA) %>%
  tally() %>%
  drop_na()

#Todas las combinaciones posibles entre entidad y municipio
combinaciones <- datos_2 %>%
  select(ENTIDAD_RES, MUNICIPIO_RES) %>% 
  distinct()

#Una las combinaciones de entidad y municipio con fecha y edad_grupos
combinaciones <- combinaciones %>% crossing(data.frame(FECHA = seq_fechas)) %>%
  crossing(data.frame(EDAD_GRUPOS = edadlabels))


prueba <- full_join(prueba, combinaciones, by=c( "ENTIDAD_RES", 
                                                  "MUNICIPIO_RES",
                                                  "FECHA", 
                                                  "EDAD_GRUPOS") )

#prueba <- prueba %>%
#  arrange ( ENTIDAD_RES, MUNICIPIO_RES, FECHA, EDAD_GRUPOS)

prueba <- prueba %>%
  mutate(n=replace_na(n,0))

prueba <- rename(prueba, defunciones_totales = n )

#Defunciones con los positivos del covid

prueba_covid <- datos_2 %>%
  filter(CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 2 | CLASIFICACION_FINAL == 3) %>%
  rename(FECHA = FECHA_DEF) %>%
  group_by(ENTIDAD_RES, MUNICIPIO_RES, EDAD_GRUPOS, FECHA) %>%
  tally() %>%
  drop_na()


prueba_covid <- prueba_covid %>% full_join(combinaciones,
                                           by=c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "EDAD_GRUPOS") )

prueba_covid <- prueba_covid %>%
  arrange ( ENTIDAD_RES, MUNICIPIO_RES, FECHA, EDAD_GRUPOS, n)

prueba_covid <- prueba_covid %>%
  mutate(n=replace_na(n,0))

prueba_covid <- rename(prueba_covid, defunciones_covid = n)

#Para----- SINTOMAS-------

# Para sintomas con todo mundo
sintomas_totales <- datos_2 %>%
  rename(FECHA = FECHA_SINTOMAS) %>%
  group_by(MUNICIPIO_RES, EDAD_GRUPOS, ENTIDAD_RES, FECHA) %>%
  tally() %>%
  drop_na()

sintomas_totales <- sintomas_totales %>%
  full_join(combinaciones, by = c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "EDAD_GRUPOS") )

sintomas_totales <- sintomas_totales %>%
  arrange (ENTIDAD_RES, MUNICIPIO_RES, FECHA, EDAD_GRUPOS)

sintomas_totales <- sintomas_totales %>%
    mutate(n=replace_na(n,0))

sintomas_totales <- rename(sintomas_totales, sintomas_tot = n)

# Para sintomas con gente confirmada de que sí tienen covid
sintomas_covid <- datos_2 %>%
  filter(CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 2 | CLASIFICACION_FINAL == 3) %>%
  rename(FECHA = FECHA_SINTOMAS) %>%
  group_by(MUNICIPIO_RES, EDAD_GRUPOS, ENTIDAD_RES, FECHA) %>%
  tally() %>%
  drop_na()


sintomas_covid <- sintomas_covid %>%
  full_join(combinaciones, by = c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "EDAD_GRUPOS") )

sintomas_covid <- sintomas_covid %>%
  arrange (ENTIDAD_RES, MUNICIPIO_RES, FECHA, EDAD_GRUPOS)

sintomas_covid <- sintomas_covid %>%
  mutate(n=replace_na(n,0))

sintomas_covid <- rename(sintomas_covid, sintomas_COVID = n)

# Para ------ INGRESO --------

# Para ingreso con todo mundo
ingreso_totales <- datos_2 %>%
  rename(FECHA = FECHA_INGRESO) %>%
  group_by(MUNICIPIO_RES, EDAD_GRUPOS, ENTIDAD_RES, FECHA) %>%
  tally() %>%
  drop_na()

ingreso_totales <- ingreso_totales %>%
  full_join(combinaciones, by = c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "EDAD_GRUPOS") )

ingreso_totales <- ingreso_totales %>%
  arrange (ENTIDAD_RES, MUNICIPIO_RES, FECHA, EDAD_GRUPOS)

ingreso_totales <- ingreso_totales %>%
  mutate(n=replace_na(n,0))

ingreso_totales <- rename(ingreso_totales, ingreso_tot = n )

# Para ingreso con gente confirmada de que sí tienen covid
ingreso_covid <- datos_2 %>%
  filter(CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 2 | CLASIFICACION_FINAL == 3) %>%
  group_by(MUNICIPIO_RES, EDAD_GRUPOS, ENTIDAD_RES, FECHA_INGRESO) %>%
  rename(FECHA = FECHA_INGRESO) %>%
  tally() %>%
  drop_na()


ingreso_covid <- ingreso_covid %>%
  full_join(combinaciones, by = c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "EDAD_GRUPOS") )

ingreso_covid <- ingreso_covid %>%
  arrange (ENTIDAD_RES, MUNICIPIO_RES, FECHA, EDAD_GRUPOS)

ingreso_covid <- ingreso_covid %>%
  mutate(n=replace_na(n,0))

ingreso_covid <- rename(ingreso_covid, ingreso_COVID = n )

#------------------Unimos todo
completos <- full_join(prueba, prueba_covid, by= c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "EDAD_GRUPOS"))

completos <- full_join(completos, sintomas_totales, by = c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "EDAD_GRUPOS"))

completos <- full_join(completos, sintomas_covid, by = c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "EDAD_GRUPOS"))

completos <- full_join(completos, ingreso_totales, by = c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "EDAD_GRUPOS"))

completos <- full_join(completos, ingreso_covid, by = c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "EDAD_GRUPOS"))

completos <- completos %>%
  mutate(defunciones_totales = replace_na(defunciones_totales, 0)) %>%
  mutate(defunciones_covid = replace_na(defunciones_totales, 0)) %>%
  mutate(sintomas_COVID = replace_na(sintomas_COVID, 0)) %>%
  mutate(sintomas_tot = replace_na(sintomas_tot, 0)) %>%
  mutate(ingreso_tot = replace_na(ingreso_tot, 0)) %>%
  mutate(ingreso_COVID = replace_na(ingreso_COVID, 0))

completos <- completos %>% relocate(ENTIDAD_RES, MUNICIPIO_RES, FECHA, EDAD_GRUPOS, 
                                    sintomas_COVID, sintomas_tot, ingreso_COVID, 
                                    ingreso_tot, defunciones_covid, defunciones_totales)

#Unimos las claves de las entidades con los nombres
completos <- left_join(completos, nombres_entidades, by = c("ENTIDAD_RES"="CLAVE_ENTIDAD"))

#Unimos las claves de los municipios con los nombres
completos <- left_join(completos, nombres_municipios, 
                       by = c("ENTIDAD_RES"="CLAVE_ENTIDAD", 
                              "MUNICIPIO_RES" = "CLAVE_MUNICIPIO"))


export(completos, "base_datos_2.rds")

# ----------------------- Termina la base de datos_2 -----------

HospitalizacionesMX <- readRDS("~/GitHub/CapacidadHospitalariaMX/processed/HospitalizacionesMX.rds")

datos_base_1 <- datos %>%
  #mutate(Estado = as.factor(Estado)) %>%
  select(-`De 0 a 14 años`)

nombres_datos_base_1 <- c("ESTADO", "MUNICIPIO", "TOTAL", 
                          edadlabels, "NO_ESPECIFICADO", 
                          "CLAVE_ENTIDAD", "CLAVE_MUNICIPIO")

colnames(datos_base_1) <- nombres_datos_base_1

edadbreaks <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 500)
edadlabels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                "75-79", "80+")

#Pivoteamos                                                       
datos_base_1 <- pivot_longer(datos_base_1, edadlabels)

#Renombramos las columnas nuevas
datos_base_1 <- rename(datos_base_1, "EDAD_GRUPOS" = name, "POBLACION" = value)

datos_base_1 %>%
  #mutate(EDAD_GRUPOS = as.factor(EDAD_GRUPOS)) %>%
  mutate(POBLACION = as.numeric(POBLACION))

datos_3 <- full_join(datos_base_1, datos_2, by= c("CLAVE_ENTIDAD" = "ENTIDAD_RES",
                                                  "CLAVE_MUNICIPIO" = "MUNICIPIO_RES",
                                                  "EDAD_GRUPOS" = "EDAD_GRUPOS"))
datos_3 <- rename(datos_3, TOTAL_ENTIDAD = TOTAL)

# Vamos a trabajar con la base de datos de hospitalizaciones

hospitalizados <- HospitalizacionesMX

hospitalizados <- hospitalizados %>%
  select(Estado, `Hospitalizados (%)`, Fecha) %>%
  mutate(Fecha = as.Date(Fecha)) # %>%
  #mutate(Estado = as.factor(Estado))

#convertimos los niveles a la norma oficial
levels(hospitalizados$Estado) <- nombres_entidades$ENTIDAD_FEDERATIVA


hospitalizados <- mutate_if(hospitalizados, is.character, toupper)

#Vamos a dividir el data frame de hospitalizados
hospitalizados_2 <- hospitalizados

conjunto <- split(hospitalizados_2, hospitalizados_2$Estado)

for (i in 1:length(conjunto)) {
  assign(paste0("hosp_estado_", i), as.data.frame(conjunto[[i]]))
}

#Ahora vamos a dividir el data frame de datos_3
conjunto_datos <- split(datos_3, datos_3$ESTADO)
  
for (i in 1:length(conjunto_datos)) {
  assign(paste0("datos3_estado_", i), as.data.frame(conjunto_datos[[i]]))
}

#Ahora vamos a unirlo

for (i in 1:length(conjunto)){
  assign(paste0("datos_finales_estado_", i),full_join(conjunto_datos[[i]], 
                                                  conjunto[[i]], 
                                                  by = c("ESTADO" = "Estado", 
                                                  "FECHA_INGRESO" = "Fecha" )))
}

export(datos_finales_estado_1, "datos_finales_estado_1.rds")
export(datos_finales_estado_2, "datos_finales_estado_2.rds")
export(datos_finales_estado_3, "datos_finales_estado_3.rds")
export(datos_finales_estado_4, "datos_finales_estado_4.rds")
export(datos_finales_estado_5, "datos_finales_estado_5.rds")
export(datos_finales_estado_6, "datos_finales_estado_6.rds")
export(datos_finales_estado_7, "datos_finales_estado_7.rds")
export(datos_finales_estado_8, "datos_finales_estado_8.rds")
export(datos_finales_estado_9, "datos_finales_estado_9.rds")
export(datos_finales_estado_10, "datos_finales_estado_10.rds")
export(datos_finales_estado_11, "datos_finales_estado_11.rds")
export(datos_finales_estado_12, "datos_finales_estado_12.rds")
export(datos_finales_estado_13, "datos_finales_estado_13.rds")
export(datos_finales_estado_14, "datos_finales_estado_14.rds")
export(datos_finales_estado_15, "datos_finales_estado_15.rds")
export(datos_finales_estado_16, "datos_finales_estado_16.rds")
export(datos_finales_estado_17, "datos_finales_estado_17.rds")
export(datos_finales_estado_18, "datos_finales_estado_18.rds")
export(datos_finales_estado_19, "datos_finales_estado_19.rds")
export(datos_finales_estado_20, "datos_finales_estado_20.rds")
export(datos_finales_estado_21, "datos_finales_estado_21.rds")
export(datos_finales_estado_22, "datos_finales_estado_22.rds")
export(datos_finales_estado_23, "datos_finales_estado_23.rds")
export(datos_finales_estado_24, "datos_finales_estado_24.rds")
export(datos_finales_estado_25, "datos_finales_estado_25.rds")
export(datos_finales_estado_26, "datos_finales_estado_26.rds")
export(datos_finales_estado_27, "datos_finales_estado_27.rds")
export(datos_finales_estado_28, "datos_finales_estado_28.rds")
export(datos_finales_estado_29, "datos_finales_estado_29.rds")
export(datos_finales_estado_30, "datos_finales_estado_30.rds")
export(datos_finales_estado_31, "datos_finales_estado_31.rds")
export(datos_finales_estado_32, "datos_finales_estado_32.rds")




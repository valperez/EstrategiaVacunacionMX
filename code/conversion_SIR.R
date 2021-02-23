rm(list = ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(readr)
library(readxl)

#Ponemos el wd para que lo guarde en processed
setwd("~/GitHub/EstrategiaVacunacionMX/data/processed")

#Solo hacer T en caso de ser necesaria la descarga pero ya todo esta en el repo
descargar <- F

if (descargar == T){
  #Base de datos descargada de la página
  source("descarga_covid.R")
}


#Base de poblacion
inegi_dats <- readRDS("~/GitHub/EstrategiaVacunacionMX/data/processed/datos_pob.rds") %>%
  mutate(Estado = toupper(Estado)) %>%
  mutate(CLAVE_MUNICIPIO = as.numeric(CLAVE_MUNICIPIO)) %>%
  mutate(CLAVE_ENTIDAD = as.numeric(CLAVE_ENTIDAD))

#Datos completos de covid
dats <- readRDS("~/GitHub/EstrategiaVacunacionMX/data/code/dats_covid.rds") %>%
  mutate(ENTIDAD_RES = as.numeric(ENTIDAD_RES)) %>%
  mutate(MUNICIPIO_RES = as.numeric(MUNICIPIO_RES)) 

#Diccionario donde viene la clave de entidad con el nombre de la entidad
diccionario.covid <- readRDS("~/GitHub/EstrategiaVacunacionMX/data/diccionario_covid.rds") %>%
  mutate(CLAVE_ENTIDAD = as.numeric(CLAVE_ENTIDAD)) %>%
  mutate(ENTIDAD_FEDERATIVA = if_else(ENTIDAD_FEDERATIVA == "MÉXICO", "ESTADO DE MÉXICO", ENTIDAD_FEDERATIVA))%>%
  filter(ENTIDAD_FEDERATIVA != "ESTADOS UNIDOS MEXICANOS") %>%
  filter(ENTIDAD_FEDERATIVA != "NO APLICA") %>%
  filter(ENTIDAD_FEDERATIVA != "SE IGNORA") %>%
  filter(ENTIDAD_FEDERATIVA != "NO ESPECIFICADO")

### OJO que este doc no puede ni debe estar en GITHUB ###
# Estos son los escenarios 
municipios_escenarios <- read_excel("~/IMSS/EstrategiaVacunacionMX/data/raw/Municipios_y_escenarios.xlsx") %>%
  mutate(Estado = if_else(Estado == "México", "Estado de México", Estado))

fecha_min <- ymd("2020/04/10")
fecha_max <- max(c(dats$FECHA_SINTOMAS, dats$FECHA_INGRESO, dats$FECHA_DEF), na.rm = TRUE)

secuencia_dias <- seq(fecha_min, fecha_max, by = 1)
edades <- c("Edad < 40", "Edad 40 - 49", "Edad 50 - 59", 
            "Edad 60 - 69", "Edad 70 - 79", "Edad 80 +") 


combinaciones <- crossing(data.frame(Fecha = secuencia_dias)) %>%
  crossing(data.frame(Tercil = c(1:3))) %>%
  crossing(data.frame(ENTIDAD_FEDERATIVA = diccionario.covid$ENTIDAD_FEDERATIVA)) %>%
  crossing(data.frame(EDAD_CAT = edades)) 


## --- Aquí ya está todo corrido y arreglado ----

inegi_dats <- inegi_dats %>%
  mutate(`Edad < 40` = rowSums(select(. , `De 0 a 14 años`:`De 35 a 39 años`))) %>%
  mutate(`Edad 40 - 49` = rowSums(select(. , c(`De 40 a 44 años`:`De 45 a 49 años`)))) %>%
  mutate(`Edad 50 - 59` = rowSums(select(. , c(`De 50 a 54 años`:`De 55 a 59 años`)))) %>%
  mutate(`Edad 60 - 69` = rowSums(select(. , c(`De 60 a 64 años`:`De 65 a 69 años`)))) %>%
  mutate(`Edad 70 - 79` = rowSums(select(. , c(`De 70 a 74 años`:`De 75 a 79 años`)))) %>%
  mutate(`Edad 80 +` = rowSums(select(. , c(`Mayores a 80 años`)))) 


inegi_dats <- inegi_dats %>%
  select(-starts_with("De"), -starts_with("Mayores"), -Total,  -`No especificado`) %>%
  pivot_longer(cols = starts_with("Edad"), names_to = "EDAD_CAT", values_to = "Poblacion") %>%
  filter(MUNICIPIO != "NO ESPECIFICADO") %>%
  drop_na(Poblacion)


inegi_dats <- full_join(inegi_dats, combinaciones, 
                    by= c("Estado" = "ENTIDAD_FEDERATIVA", "EDAD_CAT" = "EDAD_CAT"))

#AQUI VOY
inegi_dats <- inegi_dats %>%
  group_by(Estado, CLAVE_ENTIDAD, Tercil, EDAD_CAT) %>%
  summarise(sum(Poblacion))

inegi_dats <- rename(inegi_dats, Pop = `sum(Poblacion)`)

# -------- hasta aquí vamos bien

# Ahora vamos con dats
dats <- dats %>%
  select(TIPO_PACIENTE, FECHA_SINTOMAS, EDAD, ENTIDAD_RES, MUNICIPIO_RES, 
         FECHA_INGRESO, FECHA_DEF) %>% 
  mutate(EDAD_CAT = case_when(
    EDAD < 40 ~  "Edad < 40",
    EDAD >= 40 & EDAD < 50 ~ "Edad 40 - 49",
    EDAD >= 50 & EDAD < 60 ~ "Edad 50 - 59",
    EDAD >= 60 & EDAD < 70 ~ "Edad 60 - 69",
    EDAD >= 70 & EDAD < 80 ~ "Edad 70 - 79",
    EDAD >= 80 ~ "Edad 80 +")) %>%
  select(-EDAD)

dats <- dats %>% 
  right_join(diccionario.covid, by = c("ENTIDAD_RES" = "CLAVE_ENTIDAD")) %>%
  right_join(municipios_escenarios, by = c("ENTIDAD_RES" = "cve_ent", 
                                           "MUNICIPIO_RES" = "cve_mun")) %>%
  select(-Estado, -ABREVIATURA)

infectados <- dats %>%
  group_by(ENTIDAD_FEDERATIVA, EDAD_CAT, FECHA_SINTOMAS, Tercil) %>%
  rename(Fecha = FECHA_SINTOMAS) %>%
  tally() %>%
  rename(Infectados = n) %>%
  full_join(combinaciones, by = c("ENTIDAD_FEDERATIVA", "EDAD_CAT", "Fecha", "Tercil"))

muertos <- dats %>%
  group_by(ENTIDAD_FEDERATIVA, EDAD_CAT, FECHA_DEF, Tercil) %>%
  rename(Fecha = FECHA_DEF) %>%
  tally() %>%
  rename(Muertos = n) %>%
  full_join(combinaciones, by = c("ENTIDAD_FEDERATIVA", "EDAD_CAT", "Fecha", "Tercil"))

hospital <- dats %>%
  group_by(ENTIDAD_FEDERATIVA, EDAD_CAT, FECHA_INGRESO, Tercil) %>%
  rename(Fecha = FECHA_INGRESO) %>%
  tally() %>%
  rename(Hospitalizados = n) %>%
  full_join(combinaciones, by = c("ENTIDAD_FEDERATIVA", "EDAD_CAT", "Fecha", "Tercil"))

base_modelo <- infectados %>%
  left_join(muertos, by = c("Fecha","EDAD_CAT", "Tercil", "ENTIDAD_FEDERATIVA")) %>%
  left_join(hospital, by = c("Fecha","EDAD_CAT", "Tercil", "ENTIDAD_FEDERATIVA")) %>%
  mutate(Muertos = replace_na(Muertos, 0)) %>%
  mutate(Hospitalizados = replace_na(Hospitalizados, 0)) %>%
  mutate(Fecha = ymd(Fecha)) %>%
  filter(Fecha > ymd("2020/04/10"))


base_modelo_final <- left_join(inegi_dats, base_modelo,
                    by = c("Estado" = "ENTIDAD_FEDERATIVA" , 
                           "EDAD_CAT" = "EDAD_CAT", "Tercil" = "Tercil"))


base_modelo_final <- base_modelo_final %>%
  mutate(I = Infectados/Pop) %>%
  mutate(H = Hospitalizados/Pop) %>%
  mutate(M = Muertos/Pop) 

base_modelo_final <- base_modelo_final %>%
  mutate(Dia = as.numeric(Fecha - fecha_min)) 

base_modelo_final <- base_modelo_final %>%
  select(-Infectados, -Muertos, -Hospitalizados) %>%
  pivot_wider(names_from = c(EDAD_CAT), values_from = c("I","M","H", "Pop"))

# base_modelo_final <- base_modelo_final %>%
#   mutate(across(starts_with("I"), ~ replace_na(., 0))) %>%
#   mutate(across(starts_with("M"), ~ replace_na(., 0))) %>%
#   mutate(across(starts_with("H"), ~ replace_na(., 0))) 

saveRDS(base_modelo_final, "datos_SIR_porTercil.rds")


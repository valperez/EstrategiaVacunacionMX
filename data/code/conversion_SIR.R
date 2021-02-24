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


#Datos completos de covid
dats <- readRDS("~/GitHub/EstrategiaVacunacionMX/data/code/dats_covid.rds") %>%
  mutate(ENTIDAD_RES = as.numeric(ENTIDAD_RES)) %>%
  mutate(MUNICIPIO_RES = as.numeric(MUNICIPIO_RES)) 

#Diccionario donde viene la clave de entidad con el nombre de la entidad
diccionario.covid <- readRDS("~/GitHub/EstrategiaVacunacionMX/data/processed/diccionario_covid.rds") %>%
  mutate(CLAVE_ENTIDAD = as.numeric(CLAVE_ENTIDAD)) %>%
  mutate(ENTIDAD_FEDERATIVA = if_else(ENTIDAD_FEDERATIVA == "MÉXICO", "ESTADO DE MÉXICO", ENTIDAD_FEDERATIVA))%>%
  filter(ENTIDAD_FEDERATIVA != "ESTADOS UNIDOS MEXICANOS") %>%
  filter(ENTIDAD_FEDERATIVA != "NO APLICA") %>%
  filter(ENTIDAD_FEDERATIVA != "SE IGNORA") %>%
  filter(ENTIDAD_FEDERATIVA != "NO ESPECIFICADO")

### OJO que este doc no puede ni debe estar en GITHUB ###
# Estos son los escenarios 
municipios_escenarios <- read_excel("~/IMSS/EstrategiaVacunacionMX/data/raw/Municipios_y_escenarios.xlsx") %>%
  mutate(Estado = case_when(
    Estado == "Coahuila" ~ "Coahuila de Zaragoza",
    Estado == "Michoacán" ~ "Michoacán de Ocampo",
    Estado == "Veracruz" ~ "Veracruz de Ignacio de la Llave",
    Estado == "México" ~ "Estado de México",
    TRUE ~ Estado
  ))

#Base de datos de poblacion
inegi_dats <- read_excel("~/GitHub/EstrategiaVacunacionMX/data/raw/Base_datos_Poblacion.xlsx") %>%
  select(-`No especificado`, - `Total`) %>%
  pivot_longer(cols = contains("años"), names_to = "Age", values_to = "Pop") %>%
  mutate(EDAD_CAT = case_when(
    str_detect(Age, "14|19|24|29|34|39") ~ "Edad < 40",
    str_detect(Age, "44|49") ~ "Edad 40 - 49",
    str_detect(Age, "54|59") ~ "Edad 50 - 59",
    str_detect(Age, "64|69") ~ "Edad 60 - 69",
    str_detect(Age, "74|79") ~ "Edad 70 - 79",
    str_detect(Age, "Mayores a 80") ~ "Edad 80 +",
    TRUE ~ "No especificado"
  )) %>% 
  select(-Age) %>%
  left_join(municipios_escenarios, by = c("Estado","Municipio")) %>%
  group_by(EDAD_CAT, Estado, Municipio, Tercil, cve_ent, cve_mun) %>%
  summarise(Pop = sum(Pop)) %>%
  drop_na(Tercil)

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

# inegi_dats <- inegi_dats %>%
#   mutate(`Edad < 40` = rowSums(select(. , `De 0 a 14 años`:`De 35 a 39 años`))) %>%
#   mutate(`Edad 40 - 49` = rowSums(select(. , c(`De 40 a 44 años`:`De 45 a 49 años`)))) %>%
#   mutate(`Edad 50 - 59` = rowSums(select(. , c(`De 50 a 54 años`:`De 55 a 59 años`)))) %>%
#   mutate(`Edad 60 - 69` = rowSums(select(. , c(`De 60 a 64 años`:`De 65 a 69 años`)))) %>%
#   mutate(`Edad 70 - 79` = rowSums(select(. , c(`De 70 a 74 años`:`De 75 a 79 años`)))) %>%
#   mutate(`Edad 80 +` = rowSums(select(. , c(`Mayores a 80 años`)))) 


# inegi_dats <- inegi_dats %>%
#   select(-starts_with("De"), -starts_with("Mayores"), -Total,  -`No especificado`) %>%
#   pivot_longer(cols = starts_with("Edad"), names_to = "EDAD_CAT", values_to = "Poblacion") %>%
#   filter(MUNICIPIO != "NO ESPECIFICADO") %>%
#   drop_na(Poblacion)
# 
# 
# inegi_dats <- full_join(inegi_dats, combinaciones, 
#                     by= c("Estado" = "ENTIDAD_FEDERATIVA", "EDAD_CAT" = "EDAD_CAT"))
# 
# inegi_dats <- inegi_dats %>%
#   group_by(Estado, CLAVE_ENTIDAD, Tercil, EDAD_CAT) %>%
#   summarise(sum(Poblacion))
# 
# inegi_dats <- rename(inegi_dats, Pop = `sum(Poblacion)`)

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
  rename(Fecha = FECHA_SINTOMAS) %>%
  filter(Fecha > fecha_min) %>%
  group_by(ENTIDAD_FEDERATIVA, EDAD_CAT, Fecha, Tercil, ENTIDAD_RES) %>%
  tally() %>%
  full_join(combinaciones, by = c("Fecha","EDAD_CAT", "Tercil", "ENTIDAD_FEDERATIVA")) %>%
  rename(Infectados = n)

muertos <- dats %>%
  rename(Fecha = FECHA_DEF) %>%
  filter(Fecha > fecha_min) %>%
  group_by(ENTIDAD_FEDERATIVA, EDAD_CAT, Fecha, Tercil, ENTIDAD_RES) %>%
  tally() %>%
  full_join(combinaciones, by = c("Fecha","EDAD_CAT", "Tercil", "ENTIDAD_FEDERATIVA")) %>%
  rename(Muertos = n)

hospital <- dats %>%
  rename(Fecha = FECHA_INGRESO) %>%
  filter(Fecha > fecha_min) %>%
  group_by(ENTIDAD_FEDERATIVA, EDAD_CAT, Fecha, Tercil, ENTIDAD_RES) %>%
  tally() %>%
  full_join(combinaciones, by = c("Fecha","EDAD_CAT", "Tercil", "ENTIDAD_FEDERATIVA")) %>%
  rename(Hospitalizados = n)

base_modelo <- infectados %>%
  left_join(muertos, by = c("Fecha","EDAD_CAT", "Tercil", "ENTIDAD_FEDERATIVA", "ENTIDAD_RES")) %>%
  left_join(hospital, by = c("Fecha","EDAD_CAT", "Tercil", "ENTIDAD_FEDERATIVA", "ENTIDAD_RES")) %>%
  mutate(Muertos = replace_na(Muertos, 0)) %>%
  mutate(Hospitalizados = replace_na(Hospitalizados, 0)) %>%
  mutate(Fecha = ymd(Fecha)) %>%
  filter(Fecha > ymd("2020/04/10"))

#FIX ME el pop?
base_modelo_final <- base_modelo %>%
  mutate(Dia = as.numeric(Fecha - !!fecha_min))  

municipio_tercil_pop <- inegi_dats %>%
  group_by(cve_ent, EDAD_CAT, Tercil) %>%
  summarise(Poblacion = sum(Pop)) 

all_terciles <- data.frame(Tercil = c(1:3)) %>%
  crossing(data.frame(cve_ent = municipio_tercil_pop$cve_ent)) %>%
  crossing(data.frame(EDAD_CAT = edades)) 

not_terciles <- all_terciles %>% anti_join(municipio_tercil_pop, 
                                           by = c("EDAD_CAT", "cve_ent", "Tercil"))

Poptotal <- inegi_dats %>% ungroup() %>% select(Pop) %>% summarise(Poblacion = sum(Pop, na.rm = T)) %>% as.numeric()

base_modelo_final2 <- base_modelo_final %>% 
  left_join(municipio_tercil_pop, by = c("EDAD_CAT","Tercil","ENTIDAD_RES" = "cve_ent")) %>%
  group_by(Fecha, Dia, Tercil, EDAD_CAT) %>% 
  summarise(Infectados     = sum(Infectados, na.rm = T), 
            Hospitalizados = sum(Hospitalizados, na.rm = T), 
            Muertos        = sum(Muertos, na.rm = T),
            Poblacion      = sum(Poblacion, na.rm = T)) %>%
  mutate(I = Infectados/Poblacion)  %>%
  mutate(H = Hospitalizados/Poblacion) %>%
  mutate(M = Muertos/Poblacion) %>%
  mutate(Acumulados_I = cumsum(I)) %>%
  mutate(Acumulados_H = cumsum(H)) %>%
  mutate(Acumulados_M = cumsum(M)) %>%
  mutate(S = Poblacion/!!Poptotal - Acumulados_I - Acumulados_H - Acumulados_M)

base_modelo_final2 <- base_modelo_final2 %>%
  select(-Infectados, -Hospitalizados, -Muertos, -M, -Acumulados_I, -Acumulados_H) %>%
  pivot_wider(names_from = c(EDAD_CAT, Tercil), values_from = c(S,I,H,Acumulados_M, Poblacion))







base_modelo_final <- left_join(inegi_dats, base_modelo,
                    by = c("Estado" = "ENTIDAD_FEDERATIVA" , 
                           "EDAD_CAT" = "EDAD_CAT", "Tercil" = "Tercil"))


base_modelo_final <- base_modelo_final %>%
  mutate(I = Infectados/Pop) %>%
  mutate(H = Hospitalizados/Pop) %>%
  mutate(M = Muertos/Pop) 

base_modelo_final <- base_modelo_final %>%
  mutate(Dia = as.numeric(Fecha - !!fecha_min)) 

base_modelo_final <- base_modelo_final %>%
  select(-Infectados, -Muertos, -Hospitalizados) %>%
  pivot_wider(names_from = c(EDAD_CAT), values_from = c("I","M","H", "Pop"))

# base_modelo_final <- base_modelo_final %>%
#   mutate(across(starts_with("I"), ~ replace_na(., 0))) %>%
#   mutate(across(starts_with("M"), ~ replace_na(., 0))) %>%
#   mutate(across(starts_with("H"), ~ replace_na(., 0))) 

saveRDS(base_modelo_final, "datos_SIR_porTercil.rds")


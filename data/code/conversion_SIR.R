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
  summarise(Pop = sum(Pop, na.rm = T)) %>%
  drop_na(Tercil)

fecha_min <- ymd("2020/04/10")
fecha_max <- max(c(dats$FECHA_SINTOMAS, dats$FECHA_INGRESO, dats$FECHA_DEF), na.rm = TRUE)

secuencia_dias <- seq(fecha_min, fecha_max, by = 1)
edades <- c("Edad < 40", "Edad 40 - 49", "Edad 50 - 59", 
            "Edad 60 - 69", "Edad 70 - 79", "Edad 80 +") 


combinaciones <- crossing(data.frame(Fecha = secuencia_dias)) %>%
  crossing(data.frame(Tercil = c(1:3))) %>%
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
  right_join(inegi_dats, by = c("ENTIDAD_RES" = "cve_ent", "EDAD_CAT",
                                           "MUNICIPIO_RES" = "cve_mun")) %>%
  select(-Estado, -ABREVIATURA)

infectados <- dats %>%
  rename(Fecha = FECHA_SINTOMAS) %>%
  filter(Fecha > fecha_min) %>%
  group_by(EDAD_CAT, Fecha, Tercil) %>%
  tally() %>%
  full_join(combinaciones, by = c("Fecha","EDAD_CAT", "Tercil")) %>%
  rename(Infectados = n)

# -- infectados checked sí funcionan

muertos <- dats %>%
  rename(Fecha = FECHA_DEF) %>%
  filter(Fecha > fecha_min) %>%
  group_by(EDAD_CAT, Fecha, Tercil, ENTIDAD_RES) %>%
  tally() %>%
  full_join(combinaciones, by = c("Fecha","EDAD_CAT", "Tercil")) %>%
  rename(Muertos = n)

# --- muertos checked sí funciona

hospital <- dats %>%
  rename(Fecha = FECHA_INGRESO) %>%
  filter(Fecha > fecha_min) %>%
  group_by(EDAD_CAT, Fecha, Tercil, ENTIDAD_RES) %>%
  tally() %>%
  full_join(combinaciones, by = c("Fecha","EDAD_CAT", "Tercil")) %>%
  rename(Hospitalizados = n)

# --- hospital checked sí funciona


base_modelo <- infectados %>%
  left_join(muertos, by = c("Fecha","EDAD_CAT", "Tercil")) %>%
  left_join(hospital, by = c("Fecha","EDAD_CAT", "Tercil")) %>%
  mutate(Muertos = replace_na(Muertos, 0)) %>%
  mutate(Hospitalizados = replace_na(Hospitalizados, 0)) %>%
  mutate(Fecha = ymd(Fecha)) %>%
  filter(Fecha > ymd("2020/04/10"))


#FIX ME el pop?
base_modelo_final <- base_modelo %>%
  mutate(Dia = as.numeric(Fecha - !!fecha_min))  
# --- Hasta aquí parece que igual, todo bien

municipio_tercil_pop <- inegi_dats %>%
  group_by(EDAD_CAT, Tercil) %>%
  summarise(Poblacion = sum(Pop, na.rm = T))


all_terciles <- data.frame(Tercil = c(1:3)) %>%
  crossing(data.frame(EDAD_CAT = edades)) 

not_terciles <- all_terciles %>% anti_join(municipio_tercil_pop, 
                                           by = c("EDAD_CAT", "Tercil"))

Poptotal <- inegi_dats %>% ungroup() %>% select(Pop) %>% 
  summarise(Poblacion = sum(Pop, na.rm = T)) %>% as.numeric()


base_modelo_final2 <- base_modelo_final %>% 
  left_join(municipio_tercil_pop, by = c("EDAD_CAT","Tercil")) %>%
  group_by(Fecha, Dia, Tercil, EDAD_CAT, Poblacion) %>% 
  summarise(Infectados     = sum(Infectados, na.rm = T), 
            Hospitalizados = sum(Hospitalizados, na.rm = T), 
            Muertos        = sum(Muertos, na.rm = T)) 

base_modelo_final2 <- base_modelo_final2 %>%
  mutate(Infectados     = replace_na(Infectados,0)) %>%
  mutate(Hospitalizados = replace_na(Hospitalizados,0)) %>%
  mutate(Muertos        = replace_na(Muertos,0)) %>%
  group_by(Fecha, Dia, Tercil, EDAD_CAT) %>% 
  mutate(I = Infectados/!!Poptotal)  %>%
  mutate(H = Hospitalizados/!!Poptotal) %>%
  mutate(M = Muertos/!!Poptotal) %>%
  ungroup() %>%
  group_by(Tercil, EDAD_CAT) %>% 
  mutate(Acumulados_I = cumsum(I)) %>%
  mutate(Acumulados_H = cumsum(H)) %>%
  mutate(Acumulados_M = cumsum(M)) %>%
  mutate(S = Poblacion/!!Poptotal - I - H - M)
  #mutate(S = Poblacion/!!Poptotal - Acumulados_I - Acumulados_H - Acumulados_M)

base_modelo_final2 <- base_modelo_final2 %>%
  arrange(-Tercil) %>%
  select(-Infectados, -Hospitalizados, -Muertos, -M) %>%
  rename(M = Acumulados_M) %>%
  pivot_wider(names_from = c(EDAD_CAT, Tercil), values_from = c(S,I,H,M, Acumulados_I, Acumulados_H, Poblacion))

base_modelo_final3 <- 
  base_modelo_final2 %>% 
  select(Fecha, Dia, 
         starts_with("S_Edad < 40"),
         starts_with("S_Edad 40"), starts_with("S_Edad 50"),
         starts_with("S_Edad 60"), starts_with("S_Edad 70"), 
         starts_with("S_Edad 80"), starts_with("I_Edad < 40"),
         starts_with("I_Edad 40"), starts_with("I_Edad 50"),
         starts_with("I_Edad 60"), starts_with("I_Edad 70"), 
         starts_with("I_Edad 80"), starts_with("H_Edad < 40"),
         starts_with("H_Edad 40"), starts_with("H_Edad 50"),
         starts_with("H_Edad 60"), starts_with("H_Edad 70"), 
         starts_with("H_Edad 80"), starts_with("M_Edad < 40"),
         starts_with("M_Edad 40"), starts_with("M_Edad 50"),
         starts_with("M_Edad 60"), starts_with("M_Edad 70"), 
         starts_with("M_Edad 80"), starts_with("Acumulados_I_Edad < 40"),
         starts_with("Acumulados_I_Edad 40"), starts_with("Acumulados_I_Edad 50"),
         starts_with("Acumulados_I_Edad 60"), starts_with("Acumulados_I_Edad 70"), 
         starts_with("Acumulados_I_Edad 80"), starts_with("Acumulados_H_Edad < 40"),
         starts_with("Acumulados_H_Edad 40"), starts_with("Acumulados_H_Edad 50"),
         starts_with("Acumulados_H_Edad 60"), starts_with("Acumulados_H_Edad 70"), 
         starts_with("Acumulados_H_Edad 80") )

write_rds(base_modelo_final3, "datos_SIR_porTercil_julia.rds")


# Checks para base_modelo_final3

# Primero vamos a hacer un for que cheque que los acumulados todos sean mayor o 
# igual que el anterior

#para checar que sea creciente
prueba <- base_modelo_final3 %>%
  select(starts_with("Acumulados"))

suma <- c()
suma_par <- 0

for (j in 1:ncol(prueba)){
  for (i in 1:(nrow(prueba) - 1)){
    suma_par <- suma_par + (prueba[i + 1, j] - prueba[i, j] < 0)
  }
  suma[j] <- suma_par
}

#Este resultado debe dar TRUE
sum(suma) == 0

base_positivos <- base_modelo_final3 %>%
  select(-Fecha, -Dia)

positivos <- 0

#Para checar que sea positiva
for (j in 1:ncol(base_positivos)){
  for (i in 1:nrow(base_positivos)){
    positivos <- positivos + (base_positivos[i,j] < 0 )
  }
}

#Esto debe dar TRUE
positivos == 0


suma_1 <- base_modelo_final3 %>%
  select(-starts_with("Acumulados"), -Fecha, -Dia)

unidad <- rowSums(suma_1[1, ])

#Esto debe dar TRUE
unidad == 1


ggplot(base_modelo_final2) +
  geom_point(aes(x = Dia, y =`Acumulados_H_Edad < 40_1` + `Acumulados_H_Edad < 40_2` + `Acumulados_H_Edad < 40_3`)) 







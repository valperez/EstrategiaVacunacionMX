rm(list = ls())
library(readr)
library(tidyverse)
library(cmdstanr)
library(posterior)
library(lubridate)
library(dplyr)
library(ggplot2)
library(rstan)
library(zoo)
library(readxl)
library("rio")
library(data.table)
library(beepr)

site.covid <- paste0("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip")
temp <- tempfile()
download.file(site.covid, temp)
dats <- read_csv(unz(temp, unzip(temp, list = TRUE)["Name"]))
unlink(temp)
stan_fname<- "PoissonModelCountries.stan"

setwd("C:/Users/Valeria/Documents/GitHub/CapacidadHospitalariaMX/model")

datos_2 <- dats
max_fechas <- max(datos_2$FECHA_INGRESO)
seq_fechas <- seq(ymd("2020/01/01"),max_fechas, by = 1)
#convertir a TRUE cuando queremos filtrar ciertas fechas
filtrar_fechas <- T

set.seed(99)

edadbreaks <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 500)
edadlabels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                "75-79", "80+")

#Selecciono las columnas para la muestra
datos_2 <- datos_2 %>%
  mutate(FECHA_DEF = ymd(FECHA_DEF)) %>%
  mutate(CLASIFICACION_FINAL = as.factor(CLASIFICACION_FINAL)) %>%
  mutate(TIPO_PACIENTE = as.factor(TIPO_PACIENTE)) %>% #Hago las columnas del tipo correcto
  select(ENTIDAD_RES, MUNICIPIO_RES, EDAD,
         CLASIFICACION_FINAL, FECHA_DEF, TIPO_PACIENTE ) %>%           #Selecciono las columnas que necesito
  arrange(ENTIDAD_RES, MUNICIPIO_RES, FECHA_DEF)

setDT(datos_2)[ , EDAD_GRUPOS := cut(EDAD,
                                     breaks = edadbreaks,
                                     right = FALSE,
                                     labels = edadlabels)]

datos_2 <- datos_2 %>%
  mutate(EDAD_GRUPOS = as.factor(EDAD_GRUPOS)) %>%
  select(-EDAD) 

if (filtrar_fechas == T){
  fecha_inicial <- ymd("2020/04/01")       #Insertar fecha inicial
  fecha_final <- ymd("2021/01/31")         #Insertar fecha final
  seq_fechas <- seq( from = fecha_inicial, to = fecha_final, by = 1)
  datos_2 <- datos_2 %>%
    filter(FECHA_DEF >= fecha_inicial & FECHA_DEF <= fecha_final)
  combinaciones <- crossing(data.frame(FECHA_DEF = seq_fechas)) %>% #OJO AQUI
    crossing(data.frame(EDAD_GRUPOS = edadlabels))
} else {
  #Una las combinaciones de entidad y municipio con fecha y edad_grupos
combinaciones <- crossing(data.frame(FECHA_DEF = seq_fechas)) %>% #OJO AQUI
  crossing(data.frame(EDAD_GRUPOS = edadlabels))
}


#Defunciones con los positivos del covid
def_covid <- datos_2 %>%
  filter(CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 2 | CLASIFICACION_FINAL == 3) %>%
  group_by(EDAD_GRUPOS, FECHA_DEF) %>%
  tally() %>%
  drop_na()

def_covid <- full_join(combinaciones, def_covid, 
                           by=c("FECHA_DEF", "EDAD_GRUPOS"))

def_covid <- def_covid %>%
  mutate(n = replace_na(n,0)) %>%
  rename(defunciones_covid = n)

grafica_todos <- ggplot(def_covid, aes(x = FECHA_DEF, y = defunciones_covid, color = EDAD_GRUPOS)) +
  geom_line()

#Vamos a quitar a los menores de 30 porque pues a ver que pasa
vector_prohibidos <- c("15-19", "20-24", "25-29")
def_covid <- def_covid %>%
  filter(!EDAD_GRUPOS %in% vector_prohibidos) 

#Solo correrlo cuando quiera qye ya me haga el sample
def_covid <- def_covid %>%
  filter(EDAD_GRUPOS %in% sample(unique(EDAD_GRUPOS), 3))

grafica <- ggplot(def_covid, aes(x = FECHA_DEF, y = defunciones_covid, color = EDAD_GRUPOS)) +
  geom_line()



#Pivoteamos para obtener la matriz
def_covid <- pivot_wider(def_covid, 
                         values_from = defunciones_covid, 
                         names_from = FECHA_DEF)

defunciones_match_edades <- def_covid %>% select(EDAD_GRUPOS) %>%
  mutate(GrupoNum = row_number())

P_edades <- (def_covid %>% select(-EDAD_GRUPOS) %>% as.matrix())

media <- c(mean(P_edades[1,]), mean(P_edades[2,]), mean(P_edades[3,]))
varianza <- c(var(P_edades[1,]), var(P_edades[2,]), var(P_edades[3,]))


#Caracter?sticas del modelo 
chains = 2; iter_warmup = 500; nsim = 500; pchains = 2; m = 7; # threads = 1;
data  <- list( m = m, 
               npaises = 1,
               dias_predict = 30,
               ndias = ncol(P_edades) , nedades = nrow(P_edades), P_edades = P_edades,
               sigma_mu_hiper = 1,
               mu_mu_time_hiper = 0, #sigma_sigma_time_hiper = 0.1,
               mu_mu_hiper = log(100), sigma_sigma_hiper = 1, sigma_edad_hiper = 1) 

#Vamos a intentar con rstan
#sc_model <- rstan:: stan(file = stan_fname, data = data, chains = 1, warmup = 90, iter = 100)
sc_model <- cmdstanr::cmdstan_model(stan_fname, pedantic = F, force_recompile = T)
                                   # cpp_options = list(stan_threads = TRUE))


fit      <- sc_model$sample(data = data, 
                            refresh=0, iter_warmup = iter_warmup,
                            iter_sampling = nsim,  parallel_chains = pchains,
                            #threads_per_chain = threads,
                            chains = chains, adapt_delta = 0.95)  
save(fit, file = "ModelPoissonfit.RData")
beepr::beep(3)

#Guardamos las simulaciones por si las dudas
noms <- colnames(as_draws_df(fit$draws()))
modelo_ajustado <- fit$summary()

muertes <- modelo_ajustado %>% filter(str_detect(variable, "EdadPred"))
muertes <- muertes %>%
  mutate(EdadNum = str_extract(variable, "\\[.*,")) %>%
  mutate(DiaNum    = str_extract(variable, ",.*\\]")) %>%
  mutate(EdadNum = str_remove_all(EdadNum,"\\[|,")) %>%
  mutate(DiaNum    = str_remove_all(DiaNum,"\\]|,")) %>%
  mutate(EdadNum = as.numeric(EdadNum)) %>%
  mutate(DiaNum    = as.numeric(DiaNum)) 
  

#pdf("Hosp_predict_today.pdf", width = 20, height = 10)
# ggplot(muertes, x = mean) +
#   geom_ribbon(aes(y = mean, ymin = q5, ymax = q95), alpha = 0.75, fill = "gray75") +
#   geom_line(x = mean, aes(y = mean, color = "Predichos"), size = 0.25) +
#   #geom_point(aes(y = P_edades, color = "Observados"), alpha = 0.5, 
#    #          size = 0.2) +
#   #scale_y_continuous(labels = scales::percent) +
#   #scale_x_date(breaks = "2 months") +
#   theme_classic() +
#   scale_color_manual("Modelo", values = c("Observados" = "tomato3", "Predichos" = "gray50")) 
# dev.off()



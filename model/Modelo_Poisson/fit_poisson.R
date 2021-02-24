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
library(data.table)
library(tidybayes)
library(viridis)
options(mc.cores = max(parallel::detectCores() - 2, 1))

set.seed(99)

setwd("~/GitHub/EstrategiaVacunacionMX/")
stan_fname = "model/Modelo_Poisson/PoissonModel.stan"

## ----- Datos de México -------

descargar <- T

if (descargar == T){
  source("data/processed/descarga_covid.R")
}

edadbreaks <- c(0, 40, 50, 60, 70, 80, Inf)
edadlabels <- c("Edad < 40", "Edad 40 - 49", "Edad 50 - 59", 
                          "Edad 60 - 69", "Edad 70 - 79", "Edad 80 +") 

#Cambiar a TRUE dependiendo de lo que se requiera calcular
muertos <- T
hosp <- F

if (muertos == T){
  dats <- dats %>%
    mutate(FECHA_ESCOGIDA = FECHA_DEF)
}

if (hosp == T){
  dats <- dats %>%
    mutate(FECHA_ESCOGIDA = FECHA_INGRESO)
}

setwd("~/GitHub/EstrategiaVacunacionMX/")

#---Trabajemos primero con la POBLACION México
datos_pob <- readRDS("data/processed/datos_pob.rds")

datos_pob <- datos_pob %>%
  mutate(`Edad < 40` = rowSums(select(. , `De 0 a 14 años`:`De 35 a 39 años`))) %>%
  mutate(`Edad 40 - 49` = rowSums(select(. , c(`De 40 a 44 años`:`De 45 a 49 años`)))) %>%
  mutate(`Edad 50 - 59` = rowSums(select(. , c(`De 50 a 54 años`:`De 55 a 59 años`)))) %>%
  mutate(`Edad 60 - 69` = rowSums(select(. , c(`De 60 a 64 años`:`De 65 a 69 años`)))) %>%
  mutate(`Edad 70 - 79` = rowSums(select(. , c(`De 70 a 74 años`:`De 75 a 79 años`)))) %>%
  mutate(`Edad 80 +` = rowSums(select(. , c(`Mayores a 80 años`)))) 


datos_pob <- datos_pob %>%
  select(-starts_with("De"), -starts_with("Mayores"), -Total,  -`No especificado`) %>%
  pivot_longer(cols = starts_with("Edad"), names_to = "EDAD_CAT", values_to = "Poblacion") %>%
  filter(MUNICIPIO != "NO ESPECIFICADO") %>%
  drop_na(Poblacion)

datos_pob <- datos_pob %>%
  group_by(EDAD_CAT) %>%
  summarise(sum(Poblacion))

datos_pob <- rename(datos_pob, Pop = `sum(Poblacion)`)


#Selecciono las columnas para la muestra
datos_2 <- dats %>%
  mutate(FECHA_ESCOGIDA = ymd(FECHA_ESCOGIDA)) %>%
  select(ENTIDAD_RES, MUNICIPIO_RES, EDAD,
         FECHA_ESCOGIDA, TIPO_PACIENTE ) %>%           #Selecciono las columnas que necesito
  arrange(ENTIDAD_RES, MUNICIPIO_RES, FECHA_ESCOGIDA)

setDT(datos_2)[ , EDAD_GRUPOS := cut(EDAD,
                                     breaks = edadbreaks,
                                     right = FALSE,
                                     labels = edadlabels)]

datos_2 <- datos_2 %>%
  select(-EDAD) 

max_fechas <- max(datos_2$FECHA_ESCOGIDA, na.rm = TRUE)

seq_fechas <- seq(from = ymd("2020/01/01"), max_fechas, by = 1)

#Una las combinaciones de entidad y municipio con fecha y edad_grupos
combinaciones <- crossing(data.frame(FECHA_ESCOGIDA = seq_fechas)) %>% #OJO AQUI
  crossing(data.frame(EDAD_GRUPOS = edadlabels))


#Dan positivo a covid
def_covid <- datos_2 %>%
  #filter(CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 2 | CLASIFICACION_FINAL == 3) %>%
  group_by(EDAD_GRUPOS, FECHA_ESCOGIDA) %>%
  tally() %>%
  drop_na()

def_covid <- full_join(combinaciones, def_covid, 
                       by=c("FECHA_ESCOGIDA", "EDAD_GRUPOS"))

def_covid <- def_covid %>%
  mutate(n = replace_na(n,0)) %>%
  rename(totales = n) 

ggplot(def_covid, aes(x = FECHA_ESCOGIDA, y = totales, color = EDAD_GRUPOS)) +
  geom_line()




#Eliminamos los 0s
observados <- def_covid %>% filter(FECHA_ESCOGIDA >= ymd("2020/05/01") & FECHA_ESCOGIDA < max(FECHA_ESCOGIDA) - 10)
min_fecha  <- min(def_covid$FECHA_ESCOGIDA)
 ggplot(observados, aes(x = FECHA_ESCOGIDA, y = totales, color = EDAD_GRUPOS)) +
   geom_line()

#Pivoteamos para obtener la matriz
def_covid <- pivot_wider(observados, 
                         values_from = totales, 
                         names_from = FECHA_ESCOGIDA)

totales_match_edades <- def_covid %>% select(EDAD_GRUPOS) %>%
  mutate(GrupoNum = row_number())

P_edades <- (def_covid %>% select(-EDAD_GRUPOS) %>% as.matrix())

## ----- Datos de Israel


# Fuente : https://www.worldometers.info/world-population/israel-population/
pob_total_israel <- 8745792

if (muertos == T){
  #muertos
  muertos_isreal <- readRDS("Otros_Paises_Datos/Israel/muertos_totales_israel.rds") %>%
    filter(date >= ymd("2020/05/01")) %>%
    pivot_wider(mort_daily, values_from = mort_daily, names_from = date )
}

if (hosp == T){
  #hospitalizados
  hospitalizados_israel <- readRDS("Otros_Paises_Datos/Israel/hospitalizados_totales_israel.rds")
}

#Vacunados totales de la primera dosis solamente
vacunados_israel <- readRDS("Otros_Paises_Datos/Israel/vacunados_israel.rds") %>%
  group_by(EDAD_GRUPOS) %>%
  summarise(sum(first_dose))


## ----- Caracter?sticas del modelo 
chains = 1; iter_warmup = 100; nsim = 200; pchains = 1; m = 7; # threads = 1;
#chains = 4; iter_warmup = 500; nsim = 1000; pchains = 4; m = 7; # threads = 1;
datos  <- list( m = m, 
                npaises = 1, #esto siempre tiene q ser 1 si no truena está mal el diseño
                dias_predict = 300,
                ndias = ncol(P_edades) , nedades = length(edadlabels), P_edades = P_edades, 
                sigma_mu_hiper = 1,
                mu_mu_hiper = 0, sigma_sigma_hiper = 1, sigma_edad_hiper = 1,
                P_poblacion = as.data.frame(datos_pob[,2]/10000), 
                ndias_isr = ncol(muertos_isreal),
                vacunados = vacunados_israel[, 2], 
                P_pais_2 = muertos_isreal,
                pob_total_isr = pob_total_israel) 

# Tengo que checar la dimension de vacunados 


# function form 2 with an argument named `chain_id`
initf2 <- function(chain_id = 1) {
  list(alpha = rnorm(nrow(P_edades), 1, 1),
       beta_yearly_cosine = rnorm(nrow(P_edades), 0, 0.1),
       beta_yearly_sine = rnorm(nrow(P_edades), 0, 0.1),
       lambda = rnorm(m, 0, 0.1),
       mu_edad = rnorm(1, 0, 1),
       sigma_edad = abs(rnorm(1, 0, 1)),
       mu     = rnorm(1, 1, 0.1),
       gamma  = rnorm(nrow(P_edades),0,0.1),
       sigma  = abs(rnorm(1, 0, 1)))
}

# generate a list of lists to specify initial values
init_ll <- lapply(1:chains, function(id) initf2(chain_id = id))


#Vamos a intentar con rstan
#sc_model <- rstan:: stan(file = stan_fname, data = data, chains = 1, warmup = 90, iter = 100)
sc_model <- stan(file = stan_fname, 
                 model_name = paste0("Modelo_poisson_covid_", runif(1)), 
                 iter = nsim, init = init_ll,
                 warmup = iter_warmup, data = datos, chains = chains, seed = 654,
                 control = list(adapt_delta = 0.95))
saveRDS(sc_model, paste0("model_fit_poisson_valeria_",m,".rds"))

#Guardamos las simulaciones por si las dudas
modelo_ajustado <- summarise_draws(sc_model, 
                                   ~ quantile(., probs = c(0.005, 0.025, 0.05, 
                                                           0.125, 0.25, 0.5,
                                                           0.75, 0.875, 0.95, 
                                                           0.975, 0.995), na.rm = T))
Morts            <- modelo_ajustado %>% 
  filter(str_detect(variable, "EdadPred")) %>%
  mutate(GrupoNum = str_extract(variable, "\\[.*,")) %>%
  mutate(DiaNum    = str_extract(variable, ",.*\\]")) %>%
  mutate(GrupoNum = str_remove_all(GrupoNum,"\\[|,")) %>%
  mutate(DiaNum    = str_remove_all(DiaNum,"\\]|,")) %>%
  mutate(GrupoNum = as.numeric(GrupoNum)) %>%
  mutate(DiaNum    = as.numeric(DiaNum)) %>%
  select(-variable) %>%
  left_join(defunciones_match_edades, by = "GrupoNum") %>%
  mutate(Fecha = !!ymd("2020/05/01") + DiaNum) %>% 
  full_join(observados %>% rename(Fecha = FECHA_DEF), by = c("Fecha","EDAD_GRUPOS")) %>%
  arrange(EDAD_GRUPOS, Fecha)


ggplot(Morts, aes(x = Fecha)) +
  geom_ribbon(aes(ymin = `0.5%`, ymax = `99.5%`, fill = "99%"), alpha = 0.2) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`, fill = "95%"), alpha = 0.2) +
  geom_ribbon(aes(ymin = `5%`, ymax = `95%`, fill = "90%"), alpha = 0.2) +
  geom_ribbon(aes(ymin = `12.5%`, ymax = `87.5%`, fill = "75%"), alpha = 0.2) +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`, fill = "50%"), alpha = 0.2) +
  geom_line(aes(y = `50%`, color = "Predichos"), size = 0.1) +
  geom_point(aes(y = defunciones_covid, color = "Observados"),
             size = 0.1) +
  geom_line(aes(y = defunciones_covid, color = "Observados"),
            size = 0.1) +
  facet_wrap(~EDAD_GRUPOS, ncol = 8) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = "2 months") +
  theme_classic() +
  theme(axis.text.x  = element_text(angle = 90, hjust = 1)) +
  scale_color_manual("Modelo", 
                     values = c("Observados" = "firebrick", 
                                "Predichos" = "gray25")) +
  scale_fill_manual("Probabilidad\ndel escenario", 
                    values = c("50%" = viridis(5)[1],
                               "75%" = viridis(5)[2],
                               "90%" = viridis(5)[3],
                               "95%" = viridis(5)[4],
                               "99%" =  viridis(5)[5])) +
  labs(
    x = "Fecha",
    y = "Defunciones",
    title = "Predicciones a largo plazo mortalidad a partir de SINAVE",
    subtitle = "Modelo Poisson-Bayesiano"
  ) +
  ggsave(paste0("Mort_predict_",today(),".pdf"), width = 20, height = 10)


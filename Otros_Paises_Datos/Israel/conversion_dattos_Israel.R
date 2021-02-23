# --- Script que edita los datos de vacunacion, poblacion, hospitalizados y muertos
# ---  de Israel
# 
# 22 de Febrero del 2021
#
# Autores
# Rodrigo Zepeda rzepeda17@gmail.com
# Valeria Pérez valeria.perez.mat@gmail.com
#

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

setwd("~/GitHub/EstrategiaVacunacionMX/Otros_Paises_Datos/Israel/")

# ----- Vamos a ver los datos de los vacunados de Israel 
# Fuente: https://data.gov.il/dataset/covid-19/resource/57410611-936c-49a6-ac3c-838171055b1f
israel <- read.csv("~/GitHub/EstrategiaVacunacionMX/Otros_Paises_Datos/Israel/vaccinated-per-day-2021-02-20.csv") %>%
  rename(Vaccination_date = ï..VaccinationDate) %>%
  mutate(Vaccination_date = dmy(Vaccination_date)) %>%
  mutate(first_dose = as.numeric(first_dose)) %>%
  mutate(second_dose = as.numeric(second_dose)) %>%
  mutate(first_dose = replace_na(first_dose, 15)) %>%
  mutate(second_dose = replace_na(second_dose, 15))

israel <- israel %>%
  select(-X, -X.1)

#Acomodamos las edades de Israel para que sean iguales a las de México
israel <- israel %>%
  mutate(age_group = case_when(
    age_group == "0-19" | age_group == "20-29" | age_group == "30-39"  ~  "Edad < 40",
    age_group == "40-49" ~ "Edad 40 - 49",
    age_group == "50-59" ~ "Edad 50 - 59",
    age_group == "60-69" ~ "Edad 60 - 69",
    age_group == "70-79" ~ "Edad 70 - 79",
    age_group == "80-89" | age_group =="90+" ~ "Edad 80 +")) 

israel <- israel %>%
  rename(EDAD_GRUPOS = age_group)

israel_first_dose <- israel %>%
  group_by(Vaccination_date, EDAD_GRUPOS) %>%
  summarise(sum(first_dose)) 

israel_second_dose <- israel %>%
  group_by(Vaccination_date, EDAD_GRUPOS) %>%
  summarise(sum(second_dose))

israel_pob <- left_join(israel_first_dose, israel_second_dose, 
                        by = c("Vaccination_date", "EDAD_GRUPOS"))

israel_pob <- israel_pob %>%
  rename(first_dose = `sum(first_dose)`) %>%
  rename(second_dose = `sum(second_dose)`)


# ------ Vamos a ver la base de datos que tiene informacion de hospitalizados 
# Fuente : https://data.gov.il/dataset/covid-19/resource/d07c0771-01a8-43b2-96cc-c6154e7fa9bd
Sys.setlocale("LC_ALL", "Hebrew")
israel_informacion <- read_csv(file = "~/GitHub/EstrategiaVacunacionMX/Otros_Paises_Datos/Israel/corona_hospitalization_ver_00205.csv") %>%
  rename(date = תאריך) %>%
  rename(hospitalized = מאושפזים) %>%
  rename(percentage_of_women_hospitalized = `אחוז נשים מאושפזות`) %>%
  rename(average_age_hospitalized =`גיל ממוצע מאושפזים`) %>% 
  #la que sigue (E) es standard deviation of hospitalized age que creo que no necesitamos
  #la que sigue (F) es respirators
  #despues percentage of women breathing (G)
  #despues average age of respirators (H)
  #deviation from the respiratory age standard (I)
  rename(patients_are_mild = `חולים קל`) %>%
  rename(percentage_of_sick_women_is_mild = `אחוז נשים חולות קל`) %>%
  rename(average_age_of_patients_is_mild = `גיל ממוצע חולים קל`) %>%
  #despues sigue stanrdad deviation of patients' age is slight ???
  rename(medium_patients = `חולים בינוני`) %>%
  rename(pecentage_of_women_is_moderately_ill = `אחוז נשים חולות בינוני`) %>%
  rename(average_age_of_middle_age_patients = `גיל ממוצע חולים בינוני`) %>%
  #despues sigue standard deviation of middle age patients (Q)
  rename(seriously_ill = `חולים קשה`) %>%
  rename(percentage_of_women_seriously_ill = `אחוז נשים חולות קשה`) %>%
  rename(average_age_of_patients_is_severe = `גיל ממוצע חולים קשה`) %>%
  #despues desviacion standard of patients age is severe (U)
  rename(patients_are_severely_cumulative =`חולים קשה מצטבר`)

israel_informacion <- israel_informacion %>%
  select(date, hospitalized, average_age_hospitalized) %>%
  mutate(date = dmy(date))

edadbreaks <- c(0, 40, 50, 60, 70, 80, Inf)
edadlabels <- c("Edad < 40", "Edad 40 - 49", "Edad 50 - 59", 
                "Edad 60 - 69", "Edad 70 - 79", "Edad 80 +") 

setDT(israel_informacion)[ , EDAD_GRUPOS := cut(average_age_hospitalized,
                                     breaks = edadbreaks,
                                     right = FALSE,
                                     labels = edadlabels)]

israel_informacion <- israel_informacion %>%
  select(-average_age_hospitalized)
  
max_fechas <- max(israel_informacion$date, na.rm = TRUE)

seq_fechas <- seq(from = dmy("01/01/2020"), max_fechas, by = 1)

#Una las combinaciones de entidad y municipio con fecha y edad_grupos
combinaciones <- crossing(data.frame(FECHA = seq_fechas)) %>% #OJO AQUI
  crossing(data.frame(EDAD_GRUPOS = edadlabels))

hosp_israel <- full_join(combinaciones, israel_informacion, 
                       by=c("FECHA" = "date", "EDAD_GRUPOS" = "EDAD_GRUPOS"))

hosp_israel <- hosp_israel %>%
  mutate(hospitalized = replace_na(hospitalized,0)) %>%
  rename(Hosp_Israel = hospitalized) 

ggplot(hosp_israel, aes(x = FECHA, y = Hosp_Israel, color = EDAD_GRUPOS)) +
  geom_line()


#### ---- Para los MUERTOS
# Fuente : https://data.gov.il/dataset/covid-19/resource/89f61e3a-4866-4bbf-bcc1-9734e5fee58e

israel_def <- read_csv("~/GitHub/EstrategiaVacunacionMX/Otros_Paises_Datos/Israel/corona_age_and_gender_weekly_deceased.csv") %>%
  select(first_week_day, last_week_day, age_group, weekly_deceased) %>%
  mutate(first_week_day = dmy(first_week_day)) %>%
  mutate(last_week_day = dmy(last_week_day))

#first_week_day es el domingo y last_week_day es el sabado

#Agregamos una variable que se llame semana
israel_def <- israel_def %>%
  mutate(week = if_else(first_week_day > ymd("2021/01/02"), 
                        epiweek(first_week_day) +53, 
                        epiweek(first_week_day)))
           

#Arreglamos para que los muertos sea numerico
israel_def <- israel_def %>%
  mutate(weekly_deceased = ifelse(weekly_deceased == "<15", 
                                  15, 
                                  weekly_deceased)) %>%
  mutate(weekly_deceased = as.numeric(weekly_deceased))

#Arreglamos los age_groups para que sean los que queremos
edadlabels <- c("Edad < 40", "Edad 40 - 49", "Edad 50 - 59", 
                "Edad 60 - 69", "Edad 70 - 79", "Edad 80 +") 
israel_def <- israel_def %>%
  mutate(age_group = case_when(
    age_group == "0-19" | age_group == "20-24" | age_group == "25-29" | 
      age_group == "30-34" | age_group == "35-39"  ~  "Edad < 40",
    age_group == "40-44" | age_group == "45-49" ~ "Edad 40 - 49",
    age_group == "50-54" | age_group == "55-59" ~ "Edad 50 - 59",
    age_group == "60-64" | age_group == "65-69" ~ "Edad 60 - 69",
    age_group == "70-74" | age_group == "75-79" ~ "Edad 70 - 79",
    age_group == "80+" ~ "Edad 80 +")) 
## Chance no hay que dropear lo que dicen NULL
israel_def <- israel_def %>%
  drop_na()

max_fechas_def <- max(israel_def$last_week_day, na.rm = TRUE)

seq_fechas_def <- seq(from = ymd("2020/01/01"), ymd(max_fechas_def), by = 1)

#Una las combinaciones de entidad y municipio con fecha y edad_grupos
combinaciones_def <- crossing(data.frame(FECHA = seq_fechas_def)) %>% #OJO AQUI
  crossing(data.frame(EDAD_GRUPOS = edadlabels)) 

combinaciones_def <- combinaciones_def %>%
    mutate(week = if_else(FECHA > ymd("2021/01/02"), 
                        epiweek(FECHA) +52, 
                        epiweek(FECHA)))


israel_def_totales <- israel_def %>%
  group_by(age_group, week) %>%
  summarise(sum(weekly_deceased)) 

israel_def_totales <- full_join(combinaciones_def, israel_def_totales, 
                       by=c("week" = "week", "EDAD_GRUPOS" = "age_group"))

israel_def_totales <- israel_def_totales %>%
  rename(totales_israel = `sum(weekly_deceased)`) %>%
  mutate(totales_israel = replace_na(totales_israel,0)) 

grafica <- ggplot(israel_def_totales, aes(x = week, y = totales_israel, color = EDAD_GRUPOS)) +
  geom_line()


# Vamos a ver los hospitalizaciones acumuladas por ciudad y no por grupo de edad



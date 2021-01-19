#**********************************************************************************************
#  @Nombre: Indicadores Programación  
#  @Autor: Sebastián Burgos
#  @Fecha: 20210118
#  @Cambios: 
#  @Ayudas: Fernando Roa. 
#**********************************************************************************************

# Cargue de Librerias -------------------------------------------------------------------------
library(tidyverse)    # Transformaciones de datos
library(data.table)   # Transformaciones de datos
library(lubridate)    # Tratamiento de fechas
library(httr)         # Publicacion de notificaciones
options(scipen = 999)

# Cargue de Documentos de referencia ----------------------------------------------------------
 
tabla_control <- fread("Z:/01 base_datos/23 Km Troncal/Tablas Control/TR/archivos separados/20210114_TR.csv") %>% 
  janitor::clean_names()

operadores <- tabla_control %>% 
  mutate(fecha = dmy(fecha)) %>% 
  # mutate(amplitud_del_servicio = hms::as_hms(amplitud_del_servicio),
  #        tiempo_de_produccion = hms::as_hms(tiempo_de_produccion)) %>% 
  group_by(fecha, compania, asignacion, codigo_de_conductor, conductor, parte_de_trabajo) %>%
  summarise(hora_inicio = first(hora_inicio),
            hora_fin = last(hora_fin)) %>% 
  mutate(hora_inicio = ymd_hms(paste0(fecha, "-", hora_inicio)),
         hora_fin = if_else(as.numeric(str_sub(hora_fin, -8L, -7L))>=24, 
                            ymd_hms(paste0(fecha + days(1), "-", paste0(as.numeric(str_sub(hora_fin, -8L, -7L))-24), str_sub(hora_fin, -6L, -1L))),
                            ymd_hms(paste0(fecha, "-", hora_fin)))) %>% 
  arrange(codigo_de_conductor, parte_de_trabajo) %>% 
  group_by(codigo_de_conductor) %>% 
  mutate(produccion = hms::as_hms(hora_fin - hora_inicio))

# Reunión con Fernando. 

#
   
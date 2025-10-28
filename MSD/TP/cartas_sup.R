rm(list=ls())
gc()

setwd("~/obser-hub/Sinop_Dinamica/TP/")


# LIBRERIAS ----
library(tidyverse)
library(raster)
library(metR)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggthemes)  
library(ncdf4)
library(reticulate)
library(keyring)
library(FNN)
library(devtools)



nc_sup <- nc_open("Superficie_ERA5/data_stream-oper_stepType-instant.nc")
nc_pp_sup <- nc_open("Superficie_ERA5/data_stream-oper_stepType-accum.nc")

LeerVariablesNetCDF <- function(archivo, variables) {
  library(ncdf4)
  
  nc <- nc_open(archivo)
  
  # Leer dimensiones comunes
  dims_comunes <- list()
  for (var in variables) {
    dims <- nc$var[[var]]$dim
    for (d in dims) {
      if (!(d$name %in% names(dims_comunes))) {
        dims_comunes[[d$name]] <- ncvar_get(nc, d$name)
      }
    }
  }
  
  # Crear grilla base
  grid <- expand.grid(dims_comunes)
  
  # Leer cada variable y agregarla al grid
  for (var in variables) {
    var_array <- ncvar_get(nc, var)
    grid[[var]] <- as.vector(var_array)
  }
  
  nc_close(nc)
  
  return(grid)
}

df_sup <- LeerVariablesNetCDF("Superficie_ERA5/data_stream-oper_stepType-instant.nc",c("msl","u10","v10"))

df_accum_sup <- LeerVariablesNetCDF("Superficie_ERA5/data_stream-oper_stepType-accum.nc",c("tp","sshf","slhf"))

# DATA FRAME SUPERFICIE ----
# - msl = presion media a nivel del mar [hPa]
# - u10 y v10 = viento a 10m [m/s]
# - pp_total = precipitacion total [mm]
# - sshf = flujo de calor sensible en sup [J/m**2]
# - slhf = flujo de calor latente en sup [J/m**2]


df_sup <- mutate(df_sup,"pp_total"=df_accum_sup$tp*1000,
                 "sshf"=df_accum_sup$sshf,
                 "slhf"=df_accum_sup$slhf,
                 "msl"=df_sup$msl/100) %>%
  rename(date = valid_time) %>%
  mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(date = format(date, "%Y-%m-%d %H"))


# TEMA DEL MAPA ----
theme_map <- function(...) {
  theme_minimal() +
    theme(
      legend.key = element_rect(fill = "white", size = 1),
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(2.5, "cm"),
      legend.margin = margin(5, 5, 5, 5),
      legend.text = element_text(
        size = 10,
        angle = 45,
        vjust = 1,
        hjust = 1),
      legend.position = "bottom",
      axis.line = element_blank(),
      # panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      text = element_text(family = "Montserrat", color = "grey30", size = 12),
      panel.border = element_blank(),
      plot.margin = margin(5, 0, 5, 0),
      axis.title.y = element_text(face = "bold", vjust = 10),
      axis.title.x = element_text(face = "bold", vjust = -2),
      panel.spacing = unit(1, "lines"),
      ...
    )
}


# CONTORNOS COSTEROS Y SUPERFICIES POLITICAS ----
land <- ne_states(
  # scale = 50,
  returnclass = "sf") # Se usa "sf" para devolver un objeto espacial

# Datos de superficie de Argentina:
arg_surface <- metR::surface



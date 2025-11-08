# Limpio los objetos del Global Env. y de la consola: 
rm(list = ls())
cat("\014")
gc()

# Cargo las librerías necesarias: 
library(tidyverse) 
library(raster)    
library(metR)      
library(rnaturalearth)
library(ggplot2)   
library(ggthemes)  
library(ncdf4)
library(reticulate)
library(keyring)
library(FNN)

# Configurar el directorio de trabajo:
setwd("/mnt/almacen/Nube/Facu/scripting/MSD/TP/")

# Archivo de las variables en superficie (que se descargan del set de datos 
# "ERA5 hourly data on single levels from 1940 to present"): 
archivos_var_sup <- "./Superficie_ERA5/pmsl_u10_v10_2020_04_27_00_2020_04_30_18.nc"
# Variables de superficie: 
# - presión a nivel medio del mar
# - viento zonal a 10 m
# - viento meridional a 10 m. 
# Archivo de los flujos de calor latente y sensible en superficie (que se descargan
# del set de datos "ERA5 hourly data on single levels from 1940 to present"):
archivos_heat_sup <- "./Superficie_ERA5/surface_latent_sensible_heat_flux_2020_04_27_00_2020_04_30_18.nc"
# Archivo de las variables de altura (de niveles de presión, 
# que se descargan del set de datos 
# "ERA5 hourly data on pressure levels from 1940 to present"): 
archivos_var_alt <- "./Niveles_ERA5/temp_geop_vort_rel_250_300_500_850_1000hPa_2020_04_27_00_2020_04_30_18.nc"
# Variables de altura: 
# - temperatura en 250, 300, 850 y 1000 hPa 
# - geopotencial en 250, 300, 850 y 1000 hPa 
# - vorticidad relativa en 250, 300, 850 y 1000 hPa

# Theme settings para el mapa:
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

# Contornos costeros para superponer en el mapa:
land <- ne_states(
  # scale = 50,
  returnclass = "sf") # Se usa "sf" para devolver un objeto espacial

# Datos de superficie de Argentina:
arg_surface <- metR::surface

# Funcion para convertir archivo netcdf en dataframe
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

# Leo los datos de las variables de superficie como data.frames: 
# df_var_sup <- metR::ReadNetCDF(paste0(archivos_var_sup),
#                                out = "data.frame")

df_var_sup <- LeerVariablesNetCDF(archivo = archivos_var_sup,
                                  variables = c("msl", "u10", "v10"))

# Cambio el nombre de la columna de las fecha y transformo los datos de presión
# del nivel medio del mar a hPa.
df_var_sup <- df_var_sup %>%
  rename("date" = "valid_time") %>%
  mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(date = format(date, "%Y-%m-%d %H")) %>%
  mutate(msl = msl / 100) # Convierto la presión de Pa a hPa.

# Leo los datos de los flujos de calor latente y sensible en superficie: 
# df_heat_sup <- metR::ReadNetCDF(paste0(archivos_heat_sup),
#                                 out = "data.frame")

df_heat_sup <- LeerVariablesNetCDF(archivo = archivos_heat_sup,
                                   variables = c("avg_slhtf", "avg_ishf"))
# c("avg_slhtf", "avg_ishf")
# c("avg_slhtf", "avg_ishf")
# Cambio el nombre de la columna de las fecha: 
# Cambio el sentido del flujo (porque la convención del ECMWF para los flujos 
# verticales es positiva hacia abajo). Quiero que los flujos verticales sean
# positivos hacia arriba.
# The ECMWF convention for vertical fluxes is positive downwards.
df_heat_sup <- df_heat_sup %>%
  rename("date" = "valid_time") %>%
  mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(date = format(date, "%Y-%m-%d %H")) %>%
  mutate(heat_flux = -avg_slhtf + (-avg_ishf))

# Leo los datos de las variables de altura como data.frames: 
# df_var_alt <- metR::ReadNetCDF(paste0(archivos_var_alt),
#                                out = "data.frame")

df_var_alt <- LeerVariablesNetCDF(archivo = archivos_var_alt,
                                  variables = c("z", "t", "vo"))

# Cambio el nombre de la columna de las fechas, multiplico los datos de vorticidad 
# relativa por 100000 y transformo los datos de geopotencial a m: 
df_var_alt <- df_var_alt %>%
  rename("date" = "valid_time") %>%
  mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC")) %>% 
  mutate(date = format(date, "%Y-%m-%d %H")) %>%
  mutate(vo = vo * 100000) %>%
  mutate(z = z / 9.8)

# Veo las fechas descargadas para las variables de superficie y de altura, 
# y chequeo que coincidan: 
fechas_var_sup <- unique(df_var_sup$date)
fechas_heat_sup <- unique(df_heat_sup$date)
fechas_var_alt <- unique(df_var_alt$date)

if (!all(fechas_var_sup == fechas_heat_sup) | !all(fechas_var_sup == fechas_var_alt)) {
  stop("Las fechas de las variables de superficie y de altura descargadas no coinciden")  
} else {
  print(paste("Las fechas analizadas son: ", paste(fechas_var_sup, collapse = ", ")))
  fechas <- fechas_var_sup
}
rm(fechas_var_sup, fechas_heat_sup, fechas_var_alt)

# Separo a "df_var_alt" en data.frame en tres data.frames (uno por cada nivel de presión):
df_var_250 <- df_var_alt %>%
  filter(pressure_level == 250)
df_var_300 <- df_var_alt %>%
  filter(pressure_level == 300)
df_var_500 <- df_var_alt %>%
  filter(pressure_level == 500)
df_var_850 <- df_var_alt %>%
  filter(pressure_level == 850)
df_var_1000 <- df_var_alt %>%
  filter(pressure_level == 1000)
rm(df_var_alt)

# Calculo el espesor 850/500 hPa:
espesor <- left_join(df_var_850, df_var_300, 
                     by = c("date", "longitude", "latitude")) %>%
  mutate(espesor = z.y - z.x) %>%
  dplyr::select("date", "longitude", "latitude", "espesor")

# Calculo tita en 250 hPa: 
df_var_500 <- df_var_500 %>%
  mutate(tita = t * (500 / 1000) ^ -0.28)

# Calculo las valores máximos y mínimos de las variables de superficie y
# altura para las figuras: 
max_espesor <- ceiling(max(espesor$espesor)/100) * 100
min_espesor <- floor(min(espesor$espesor)/100) * 100
media_espesor <- round(mean(espesor$espesor)/10) * 10

max_heat_flux <- ceiling(max(df_heat_sup$heat_flux)/50) * 50
min_heat_flux <- floor(min(df_heat_sup$heat_flux)/50) * 50
media_heat_flux <- round(mean(df_heat_sup$heat_flux)/50) * 50

max_avg_slhtf <- ceiling(max(df_heat_sup$avg_slhtf)/50) * 50
min_avg_slhtf <- floor(min(df_heat_sup$avg_slhtf)/50) * 50
media_avg_slhtf <- round(mean(df_heat_sup$avg_slhtf)/50) * 50

max_avg_ishf <- ceiling(max(df_heat_sup$avg_ishf)/50) * 50
min_avg_ishf <- floor(min(df_heat_sup$avg_ishf)/50) * 50
media_avg_ishf <- round(mean(df_heat_sup$avg_ishf)/50) * 50

max_vort_rel <- ceiling(max(df_var_300$vo)/5) * 5
min_vort_rel <- floor(min(df_var_300$vo)/5) *5

max_tita <- ceiling(max(df_var_500$tita)/5) * 5
min_tita <- floor(min(df_var_500$tita)/5) *5
media_tita <- round(mean(df_var_500$tita)/5) * 5

# Para las variables de superficie y el espesor, aplico una máscara que enmascare
# la cordillera de los Andes: 
# Busco el vecino más cercano usando el paquete "FNN":
nearest_neighbors <- get.knnx(arg_surface[, c("lon", "lat")],
                              df_var_sup[, c("longitude", "latitude")], 
                              k = 1)
nearest_indices <- nearest_neighbors$nn.index
df_var_sup$alt <- arg_surface$height[nearest_indices]

nearest_neighbors <- get.knnx(arg_surface[, c("lon", "lat")],
                              espesor[, c("longitude", "latitude")], 
                              k = 1)
nearest_indices <- nearest_neighbors$nn.index
espesor$alt <- arg_surface$height[nearest_indices]

for (i in 1:length(fechas)) {
  
  
  fecha <- fechas[i]
  
  # Mapa de presión al nivel medio del mar y espesores 850/300 hPa: 
  # Filtro los data.frames en la fecha correspondiente:
  df_var_sup_fecha <- df_var_sup %>%
    filter(date == fecha)
  espesor_fecha <- espesor %>%
    filter(date == fecha)
 #   
 # p <- ggplot() +
 #   geom_contour_fill(data = espesor_fecha, 
 #                     aes(longitude, latitude, z = espesor), 
 #                     breaks = MakeBreaks(binwidth = 100)) +
 #   geom_contour_fill(data = espesor_fecha %>% filter(alt > 2000), 
 #                     aes(longitude, latitude, z = espesor), 
 #                     fill = "grey90") +
 #   geom_contour2(data = df_var_sup_fecha %>% filter(alt < 2000), 
 #                 aes(longitude, latitude, z = msl), 
 #                 breaks = MakeBreaks(binwidth = 2),
 #                 col = "grey35") +
 #   geom_text_contour(data = df_var_sup_fecha %>% filter(alt < 2000), 
 #                     aes(longitude, latitude, z = msl), 
 #                     size = 3, skip = 0, rotate = FALSE,
 #                     stroke = 0.2,
 #                     breaks = MakeBreaks(binwidth = 2),
 #                     label.placer = label_placer_flattest(), check_overlap = TRUE) +
 #   geom_sf(data = land, color = "white", fill = NA, size = 1.2) +
 #   geom_sf(data = land, color = "grey50", fill = NA, size = 0.4) +
 #   # metR::geom_streamline(data = df_var_sup_fecha,
 #   #                       aes(x = longitude, y = latitude,
 #   #                           dx = u10,  # Desplazamiento este-oeste
 #   #                           dy = v10,  # Desplazamiento norte-sur
 #   #                           alpha = ..step..),  # Control de la transparencia de las líneas
 #   #                       color = "grey",
 #   #                       size = 0.4,
 #   #                       L = 2,  # Longitud de las líneas
 #   #                       res = 2, # Resolución
 #   #                       n = 30,  # Número de segmentos
 #   #                       arrow = NULL,
 #   #                       lineend = "round",
 #   #                       inherit.aes = FALSE) +
 # scale_alpha(guide = "none") +
 # scale_fill_divergent(name = "Espesores (m) \n \n ",
 #                      high = "orangered",
 #                      low = "deepskyblue2",
 #                      mid = "lightyellow",
 #                      midpoint = (7500 + 8200) / 2,
 #                      limits = c(7500, 8200),
 #                      # midpoint = media_espesor,
 #                      # limits = c(min_espesor, max_espesor),
 #                      breaks = MakeBreaks(binwidth = 100), 
 #                      oob = scales::squish) +
 #   labs(title = "Presión al nivel medio del mar (contornos) y espesores 850/300 hPa (sombreado)", 
 #        subtitle = fecha,
 #        x = "", y = "") +
 #   coord_sf(xlim = c(-88, -32), ylim = c(-60, -20), 
 #            expand = FALSE) +
 #   theme_map()
 # 
 # nombre_mapa <- paste0("mapa_pmsl_espesor_850_300", "_", fecha, ".png")
 # ggsave(nombre_mapa, p, height = 8, width = 9, dpi = 500)
 # graphics.off()
  
  # Mapas de flujo de calor latente y sensible: 
  # Filtro los data.frames en la fecha correspondiente:
#  df_heat_sup_fecha <- df_heat_sup %>%
#    filter(date == fecha)
#  
#  p1 <- ggplot() +
#    geom_contour_fill(data = df_heat_sup_fecha, 
#                      aes(longitude, latitude, z = heat_flux), 
#                      breaks = MakeBreaks(binwidth = 50, exclude = 0)) +
#    geom_contour_fill(data = espesor_fecha %>% filter(alt > 2000), 
#                      aes(longitude, latitude, z = espesor), 
#                      fill = "grey90") +
#    geom_contour2(data = df_var_sup_fecha %>% filter(alt < 2000), 
#                  aes(longitude, latitude, z = msl), 
#                  breaks = MakeBreaks(binwidth = 2),
#                  col = "grey35") +
#    geom_text_contour(data = df_var_sup_fecha %>% filter(alt < 2000), 
#                      aes(longitude, latitude, z = msl), 
#                      size = 3, skip = 0, rotate = FALSE,
#                      stroke = 0.2,
#                      breaks = MakeBreaks(binwidth = 2),
#                      label.placer = label_placer_flattest(), check_overlap = TRUE) +
#    geom_sf(data = land, color = "white", fill = NA, size = 1.2) +
#    geom_sf(data = land, color = "grey50", fill = NA, size = 0.4) +
#    scale_alpha(guide = "none") +
#    scale_fill_divergent(name = "Flujos de calor (W/m2) \n \n ",
#                         high = "darkred", 
#                         mid = "lightyellow",
#                         low = "darkblue",
#                         limits = c(min_heat_flux, max_heat_flux),
#                         breaks = MakeBreaks(binwidth = 50, exclude = 0), 
#                         oob = scales::squish) +
#    labs(title = paste0("Presión al nivel medio del mar (contornos) y", "\n", 
#    "flujos de calor latente más sensible (sombreado)"), 
#         subtitle = fecha,
#         x = "", y = "") +
#    coord_sf(xlim = c(-88, -32), ylim = c(-60, -20), 
#             expand = FALSE) +
#    theme_map()
#  
#  nombre_mapa <- paste0("mapa_pmsl_flujos_de_calor_latente_sensible_sup", "_", fecha, ".png")
#  ggsave(nombre_mapa, p1, height = 8, width = 9, dpi = 500)
#  graphics.off()
#  
#  p1_latent <- ggplot() +
#    geom_contour_fill(data = df_heat_sup_fecha, 
#                      aes(longitude, latitude, z = avg_slhtf), 
#                      breaks = MakeBreaks(binwidth = 50, exclude = 0)) +
#    geom_contour_fill(data = espesor_fecha %>% filter(alt > 2000), 
#                      aes(longitude, latitude, z = espesor), 
#                      fill = "grey90") +
#    geom_contour2(data = df_var_sup_fecha %>% filter(alt < 2000), 
#                  aes(longitude, latitude, z = msl), 
#                  breaks = MakeBreaks(binwidth = 2),
#                  col = "grey35") +
#    geom_text_contour(data = df_var_sup_fecha %>% filter(alt < 2000), 
#                      aes(longitude, latitude, z = msl), 
#                      size = 3, skip = 0, rotate = FALSE,
#                      stroke = 0.2,
#                      breaks = MakeBreaks(binwidth = 2),
#                      label.placer = label_placer_flattest(), check_overlap = TRUE) +
#    geom_sf(data = land, color = "white", fill = NA, size = 1.2) +
#    geom_sf(data = land, color = "grey50", fill = NA, size = 0.4) +
#    scale_alpha(guide = "none") +
#    scale_fill_divergent(name = "Flujos de calor latente (W/m2) \n \n ",
#                         high = "darkred", 
#                         mid = "lightyellow",
#                         low = "darkblue",
#                         limits = c(min_avg_slhtf, max_avg_slhtf),
#                         breaks = MakeBreaks(binwidth = 50, exclude = 0), 
#                         oob = scales::squish) +
#    labs(title = paste0("Presión al nivel medio del mar (contornos) y", "\n", 
#                        "flujos de calor latente (sombreado)"), 
#         subtitle = fecha,
#         x = "", y = "") +
#    coord_sf(xlim = c(-88, -32), ylim = c(-60, -20), 
#             expand = FALSE) +
#    theme_map()
#  
#  nombre_mapa <- paste0("mapa_pmsl_flujos_de_calor_latente_sup", "_", fecha, ".png")
#  ggsave(nombre_mapa, p1_latent, height = 8, width = 9, dpi = 500)
#  graphics.off()
#  
#  p1_sensible <- ggplot() +
#    geom_contour_fill(data = df_heat_sup_fecha, 
#                      aes(longitude, latitude, z = avg_ishf), 
#                      breaks = MakeBreaks(binwidth = 50, exclude = 0)) +
#    geom_contour_fill(data = espesor_fecha %>% filter(alt > 2000), 
#                      aes(longitude, latitude, z = espesor), 
#                      fill = "grey90") +
#    geom_contour2(data = df_var_sup_fecha %>% filter(alt < 2000), 
#                  aes(longitude, latitude, z = msl), 
#                  breaks = MakeBreaks(binwidth = 2),
#                  col = "grey35") +
#    geom_text_contour(data = df_var_sup_fecha %>% filter(alt < 2000), 
#                      aes(longitude, latitude, z = msl), 
#                      size = 3, skip = 0, rotate = FALSE,
#                      stroke = 0.2,
#                      breaks = MakeBreaks(binwidth = 2),
#                      label.placer = label_placer_flattest(), check_overlap = TRUE) +
#    geom_sf(data = land, color = "white", fill = NA, size = 1.2) +
#    geom_sf(data = land, color = "grey50", fill = NA, size = 0.4) +
#    scale_alpha(guide = "none") +
#    scale_fill_divergent(name = "Flujos de calor sensible (W/m2) \n \n ",
#                         high = "darkred", 
#                         mid = "lightyellow",
#                         low = "darkblue",
#                         limits = c(min_avg_slhtf, max_avg_slhtf),
#                         breaks = MakeBreaks(binwidth = 50, exclude = 0), 
#                         oob = scales::squish) +
#    labs(title = paste0("Presión al nivel medio del mar (contornos) y", "\n", 
#                        "flujos de calor sensible (sombreado)"), 
#         subtitle = fecha,
#         x = "", y = "") +
#    coord_sf(xlim = c(-88, -32), ylim = c(-60, -20), 
#             expand = FALSE) +
#    theme_map()
#  
#  nombre_mapa <- paste0("mapa_pmsl_flujos_de_calor_sensible_sup", "_", fecha, ".png")
#  ggsave(nombre_mapa, p1_sensible, height = 8, width = 9, dpi = 500)
#  graphics.off()
#  
#  # Mapa de geopotencial y vorticidad en 300 hPa: 
#  # Filtro los data.frames en la fecha correspondiente:
#  df_var_300_fecha <- df_var_300 %>%
#    filter(date == fecha)
#  
# # p2 <- ggplot() +
# #   geom_contour_fill(data = df_var_300_fecha, 
# #                     aes(longitude, latitude, z = vo), 
# #                     breaks = MakeBreaks(binwidth = 2, exclude = 0)) + 
# #   geom_contour2(data = df_var_300_fecha, 
# #                 aes(longitude, latitude, z = z), 
# #                 breaks = MakeBreaks(binwidth = 40),
# #                 col = "grey35") +
# #   geom_text_contour(data = df_var_300_fecha, 
# #                     aes(longitude, latitude, z = z), 
# #                     size = 3, skip = 0, rotate = FALSE,
# #                     stroke = 0.2,
# #                     breaks = MakeBreaks(binwidth = 40),
# #                     label.placer = label_placer_flattest(), check_overlap = TRUE) +
# #   geom_sf(data = land, color = "white", fill = NA, size = 1.2) +
# #   geom_sf(data = land, color = "grey50", fill = NA, size = 0.4) +
# #   scale_alpha(guide = "none") +
# #   scale_fill_divergent(name = "Vorticidad en 300 hPa \n \n ",
# #                        high = "firebrick2", 
# #                        low = "deepskyblue2",
# #                        limits = c(-14, 10),
# #                        # limits = c(min_vort_rel, max_vort_rel),
# #                        breaks = MakeBreaks(binwidth = 2, exclude = 0),
# #                        oob = scales::squish) +
# #   labs(title = "Geopotencial (contornos) y vorticidad relativa 1e5 (sombreado) en 300 hPa",
# #        subtitle = fecha, 
# #        x = "", y = "") +
# #   coord_sf(xlim = c(-88, -32), ylim = c(-60, -20), 
# #            expand = FALSE) +
# #   theme_map() 
# # 
# # nombre_mapa <- paste0("mapa_geop_vort_300hPa", "_", fecha, ".png")
# # ggsave(nombre_mapa, p2, height = 8, width = 9, dpi = 500)
# # graphics.off()
#  
  # Mapa de temperatura potencial (tita) en 250 hPa: 
  # Filtro los data.frames en la fecha correspondiente:
  df_var_500_fecha <- df_var_500 %>%
    filter(date == fecha) 
  
  p3 <- ggplot() +
    geom_contour_fill(data = df_var_500_fecha, 
                      aes(longitude, latitude, z = tita), 
                      breaks = MakeBreaks(binwidth = 5)) + 
    geom_contour2(data = df_var_500_fecha, 
                  aes(longitude, latitude, z = tita), 
                  breaks = MakeBreaks(binwidth = 5),
                  col = "grey35") +
    geom_text_contour(data = df_var_500_fecha, 
                      aes(longitude, latitude, z = tita), 
                      size = 3, skip = 0, rotate = FALSE,
                      stroke = 0.2,
                      breaks = MakeBreaks(binwidth = 5),
                      label.placer = label_placer_flattest(), check_overlap = TRUE) +
    geom_sf(data = land, color = "white", fill = NA, size = 1.2) +
    geom_sf(data = land, color = "grey50", fill = NA, size = 0.4) +
    scale_alpha(guide = "none") +
    scale_fill_divergent(name = "Tita en 500 hPa \n \n ",
                         high = "orangered",
                         low = "deepskyblue2",
                         mid = "lightyellow",
                         midpoint = (290 + 365) / 2,
                         limits = c(290, 365),
                         # midpoint = media_tita,
                         # limits = c(min_tita, max_tita),
                         breaks = MakeBreaks(binwidth = 5), 
                         oob = scales::squish) +
    labs(title = "Tita (contorno y sombreado) en 500 hPa",
         subtitle = fecha, 
         x = "", y = "") +
    coord_sf(xlim = c(-88, -32), ylim = c(-60, -20), 
             expand = FALSE) +
    theme_map() 
  
  nombre_mapa <- paste0("mapa_tita_500hPa", "_", fecha, ".png")
  ggsave(nombre_mapa, p3, height = 8, width = 9, dpi = 500)
  graphics.off()
}




rm(list=ls())
gc()

setwd("/mnt/almacen/Nube/Facu/scripting/MSD/TP/")


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

df_alt <- LeerVariablesNetCDF("geop_vort_w_div_1000_500_300hPa_2024-05-30-00z_21z.nc",c("z","vo"))
df_alt <- df_alt %>%
  rename(date = valid_time) %>%
  mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(date = format(date, "%Y-%m-%d %H")) %>%
  mutate(vo = vo * 100000) %>%
  mutate(z = z / 9.8)

df_var_300<-df_alt%>%filter(pressure_level==300)

max_vort_rel <- ceiling(max(df_var_300$vo)/5) * 5
min_vort_rel <- floor(min(df_var_300$vo)/5) *5


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

# FECHAS----
fechas <- unique(df_alt$date)

# MAPAS
for (i in 1:length(fechas)) {
  
  fecha <- fechas[i]

  # Mapa de geopotencial y vorticidad en 300 hPa: 
  # Filtro los data.frames en la fecha correspondiente:
  df_var_300_fecha <- df_var_300 %>%
    filter(date == fecha)
  
  p2 <- ggplot() +
    geom_contour_fill(data = df_var_300_fecha, 
                      aes(longitude, latitude, z = vo), 
                      breaks = MakeBreaks(binwidth = 2, exclude = 0)) + 
    geom_contour2(data = df_var_300_fecha, 
                  aes(longitude, latitude, z = z), 
                  breaks = MakeBreaks(binwidth = 40),
                  col = "grey35") +
    geom_text_contour(data = df_var_300_fecha, 
                      aes(longitude, latitude, z = z), 
                      size = 3, skip = 0, rotate = FALSE,
                      stroke = 0.2,
                      breaks = MakeBreaks(binwidth = 40),
                      label.placer = label_placer_flattest(), check_overlap = TRUE) +
    geom_sf(data = land, color = "white", fill = NA, size = 1.2) +
    geom_sf(data = land, color = "grey50", fill = NA, size = 0.4) +
    scale_alpha(guide = "none") +
    scale_fill_divergent(name = "Vorticidad en 300 hPa \n \n ",
                         high = "firebrick2", 
                         low = "deepskyblue2",
                         limits = c(-14, 10),
                         # limits = c(min_vort_rel, max_vort_rel),
                         breaks = MakeBreaks(binwidth = 2, exclude = 0),
                         oob = scales::squish) +
    labs(title = "Geopotencial (contornos) y vorticidad relativa 1e5 (sombreado) en 300 hPa",
         subtitle = fecha, 
         x = "", y = "") +
    coord_sf(xlim = c(-90, -30), ylim = c(-60, -20)) +
    theme_map() 
  
  nombre_mapa <- paste0("mapa_geop_vort_300hPa", "_", fecha, ".png")
  ggsave(nombre_mapa, p2, height = 8, width = 9, dpi = 500)
  graphics.off()
}

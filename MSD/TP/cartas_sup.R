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


nc_altura <- nc_open("Niveles_ERA5/data_stream-oper_stepType-instant.nc")
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



# DATA FRAME ALTURA ----
df_alt <- LeerVariablesNetCDF("Niveles_ERA5/data_stream-oper_stepType-instant.nc","z")
df_alt <- df_alt %>%
  rename(date = valid_time) %>%
  mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(date = format(date, "%Y-%m-%d %H")) %>%
  mutate(z = z / 9.8)


df_var_500 <- df_alt %>% filter(pressure_level==500)
df_var_1000 <- df_alt %>% filter(pressure_level==1000)

# Calculo el espesor 1000/500 hPa: ----
espesor <- left_join(df_var_1000, df_var_500, 
                     by = c("date", "longitude", "latitude")) %>%
  mutate(espesor = z.y - z.x) %>%
  dplyr::select("date", "longitude", "latitude", "espesor")


# Veo las fechas descargadas para las variables de superficie y de altura, 
# y chequeo que coincidan: 
fechas_var_sup <- unique(df_sup$date)
fechas_var_alt <- unique(df_alt$date)

if (!all(fechas_var_sup == fechas_var_alt)) {
  stop("Las fechas de las variables de superficie y de altura descargadas no coinciden")  
} else {
  print(paste("Las fechas analizadas son: ", paste(fechas_var_sup, collapse = ", ")))
  fechas <- fechas_var_sup
}
rm(fechas_var_sup, fechas_var_alt)


# Calculo las valores máximos y mínimos de las variables de superficie y
# altura para las figuras: 
max_espesor <- ceiling(max(espesor$espesor)/100) * 100
min_espesor <- floor(min(espesor$espesor)/100) * 100
media_espesor <- round(mean(espesor$espesor)/10) * 10

#max_vort_rel <- ceiling(max(df_var_300$vo)/5) * 5
#min_vort_rel <- floor(min(df_var_300$vo)/5) *5


# ENMASCARO LA CORDILLERA ----
# Para las variables de superficie y el espesor, aplico una máscara que enmascare
# la cordillera de los Andes: 
# Busco el vecino más cercano usando el paquete "FNN":
nearest_neighbors <- get.knnx(arg_surface[, c("lon", "lat")],
                              df_sup[, c("longitude", "latitude")], 
                              k = 1)
nearest_indices <- nearest_neighbors$nn.index
df_sup$alt <- arg_surface$height[nearest_indices]

nearest_neighbors <- get.knnx(arg_surface[, c("lon", "lat")],
                              espesor[, c("longitude", "latitude")], 
                              k = 1)
nearest_indices <- nearest_neighbors$nn.index
espesor$alt <- arg_surface$height[nearest_indices]


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


# MAPA PRESIÓN EN SUP Y ESPESORES 1000/500 ----
for (i in 1:length(fechas)) {
fecha <- fechas[i]

# Mapa de presión al nivel medio del mar y espesores 1000/500 hPa: 
# Filtro los data.frames en la fecha correspondiente:
df_sup_fecha <- df_sup %>%
  filter(date == fecha)
espesor_fecha <- espesor %>%
  filter(date == fecha)

p<-ggplot() +
  geom_contour_fill(data = espesor_fecha, 
                    aes(longitude, latitude, z = espesor), 
                    breaks = MakeBreaks(binwidth = 20)) +
  geom_contour_fill(data = espesor_fecha %>% filter(alt > 2000), 
                    aes(longitude, latitude, z = espesor), 
                    fill = "grey90") +
  geom_contour2(data = df_sup_fecha %>% filter(alt < 2000), 
                aes(longitude, latitude, z = msl), 
                breaks = MakeBreaks(binwidth = 2),
                col = "grey35") +
  geom_text_contour(data = df_sup_fecha %>% filter(alt < 2000), 
                    aes(longitude, latitude, z = msl), 
                    size = 3, skip = 0, rotate = FALSE,
                    stroke = 0.2,
                    breaks = MakeBreaks(binwidth = 2),
                    label.placer = label_placer_flattest(), check_overlap = TRUE) +
  geom_sf(data = land, color = "white", fill = NA, size = 1.2) +
  geom_sf(data = land, color = "grey50", fill = NA, size = 0.4) +
  scale_alpha(guide = "none") +
  scale_fill_gradientn(
    name = "Espesor 1000–500 hPa (m)\n\n",
    colours = c(
      "#40004b", # violeta oscuro - aire muy frío
      "#542788", # violeta
      "#8073ac", # lavanda
      "#b2abd2", # celeste violáceo
      "#d8daeb", # celeste claro
      "#f7f7f7", # neutro (~5600)
      "#fee0b6", # amarillo claro
      "#fdb863", # naranja
      "#e08214", # naranja oscuro
      "#b35806", # marrón cálido
      "#7f3b08"  # marrón rojizo - aire muy cálido
    ),
    limits = c(5400, 5800),
    breaks = seq(5400, 5800, by = 20),
    oob = scales::squish
  )+
  
  labs(title = "Presión al nivel medio del mar (contornos) y espesores 1000/500 hPa (sombreado)", 
       subtitle = fecha,
       x = "", y = "") +
  coord_sf(xlim = c(-80, -30), ylim = c(-60, -20)) +
  theme_map()

nombre_mapa <- paste0("mapa_pmsl_espesor_1000_500", "_", fecha, ".png")
ggsave(nombre_mapa, p, height = 8, width = 9, dpi = 500)
graphics.off()
}

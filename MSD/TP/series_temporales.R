rm(list=ls())
gc()

library(tidyverse)

setwd("/mnt/almacen/Nube/Facu/scripting/MSD/TP/")

df_mdq<-read_csv("./datos_mdq.csv")

fecha1<-"2020-04-28 00:00:00"
fecha2<-"2020-04-29 00:00:00"

df_mdq <- read.csv("datos_mdq.csv") %>% 
  mutate(date = as.POSIXct(date, format="%Y-%m-%d %H", tz="UTC"))

# GRAFICO PRESION
df_mdq %>% ggplot(aes(x=date,y=pmsnm))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = df_mdq$date[25],linetype=2)+
  geom_vline(xintercept = df_mdq$date[49],linetype=2)+
  scale_x_datetime(
    name = "Fecha",
    #breaks = "12 hours",       # cada 6 horas
    date_labels = "%d- %HZ",
    #lmits = c(df_mdq$date[1], df_mdq$date[93])
  ) +
  labs(title = "Presión reducida al nivel medio del mar en Mar del Plata AERO",
       x = "Fecha",
       y = "Presión [hPa]")+
  theme_bw()

# GRAFICO VIENTOS
df_mdq %>% ggplot(aes(x=date,y=int_v))+
  geom_line(color="darkgreen")+
  geom_point(color="darkgreen")+
  geom_vline(xintercept = df_mdq$date[25],linetype=2)+
  geom_vline(xintercept = df_mdq$date[49],linetype=2)+
  scale_x_datetime(
    name = "Fecha",
    #breaks = "12 hours",       # cada 6 horas
    date_labels = "%d- %HZ",
    #lmits = c(df_mdq$date[1], df_mdq$date[93])
  ) +
  labs(title = "Intensidad del viento a 10m en Mar del Plata AERO",
       x = "Fecha",
       y = "Intensidad de viento [kt]")+
  theme_bw()

# GRAFICO DIRECCION VIENTOS
df_mdq %>% ggplot(aes(x=date,y=dir_v))+
  geom_line(color="#222222")+
  geom_point(color="#222222")+
  geom_vline(xintercept = df_mdq$date[25],linetype=2)+
  geom_vline(xintercept = df_mdq$date[49],linetype=2)+
  scale_x_datetime(
    name = "Fecha",
    #breaks = "12 hours",       # cada 6 horas
    date_labels = "%d- %HZ",
    #lmits = c(df_mdq$date[1], df_mdq$date[93])
  ) +
  scale_y_continuous(breaks=seq(0,360,45))+
  labs(title = "Dirección del viento a 10m en Mar del Plata AERO",
       x = "Fecha",
       y = "Dirección del viento [°]")+
  theme_bw()

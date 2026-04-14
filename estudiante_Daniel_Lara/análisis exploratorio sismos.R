datos<-readRDS("input/sismicevents.rds")
library(tidyverse)
library(sf)
library(ggplot2)

datos<-datos%>%
  mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))%>%
  arrange(time)

#Análisis de Frencuencia y Magnitud
ggplot(datos, aes(x = time, y = magnitude)) +
  geom_line(color = "steelblue", alpha = 0.5) +
  labs(title = "Evolución de la Magnitud Sísmica", x = "Año", y = "Magnitud (mw)") +
  theme_minimal()
####ANUAL
datos_anual<-datos%>%
  mutate(año=year(time))%>%
  group_by(año)%>%
  summarise(
    conteo=n(),
    mag_promedio=mean(magnitude,na.rm=TRUE),
    mag_max=max(magnitude, na.rm = TRUE)
  )
#Análisis de conteo anual, mágnitud máxima y promedio
ggplot(datos_anual, aes(x = año, y = conteo)) + #se puede cambiar a promedio o max
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Frecuencia anual de eventos sísmicos", y = "Número de sismos")
#Análisis de Estacionaridad y Autorcorrelación
acf(datos$magnitude, main="Autocorrelación de la magnitud")
#Relación de Profundidad vs Magnitud
ggplot(datos, aes(x = depth, y = magnitude)) +
  geom_point(alpha = 0.4, color = "darkred") +
  labs(
    title = "Relación entre Profundidad y Magnitud",
    x = "Profundidad (km)",
    y = "Magnitud (Mw)"
  ) +
  geom_hex() +
  scale_fill_viridis_c() +
  theme_minimal()

######### TIC
datos<-readRDS("input/sismos.rds")
library(tidyverse)
library(sf)
library(ggplot2)
library(lubridate)
library(patchwork)
datos<-datos%>%
  mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))%>%
  arrange(time)


#Análisis de extremos: el máximo por cada año
datos_extremos <- datos %>%
  mutate(periodo = floor_date(time, "year")) %>% 
  group_by(periodo) %>%
  summarise(
    minimo = min(magnitude, na.rm = TRUE),
    media  = mean(magnitude, na.rm = TRUE),
    maximo = max(magnitude, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.infinite(minimo) & !is.infinite(maximo))

p1 <- ggplot(datos_extremos, aes(x = minimo)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Mínimo Anual", x = "Valor", y = "Densidad") +
  theme_minimal()
p2 <- ggplot(datos_extremos, aes(x = media)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  labs(title = "Media Anual", x = "Valor", y = "Densidad") +
  theme_minimal()
p3 <- ggplot(datos_extremos, aes(x = maximo)) +
  geom_density(fill = "coral", alpha = 0.5) +
  labs(title = "Máximo Anual", x = "Valor", y = "Densidad") +
  theme_minimal()
#Densidades de los extremos
p1 + p2 + p3


library(evd)
attach(datos_extremos) 
fit_max <-fgev(datos_extremos$maximo)
#TENEMOS UN MODELO GEV CON PARÁMETROS:
fit_max$estimate #colas no tan pesadas
#CON EL GRÁFICO P-PLOT
par(mfrow = c(2, 2))
plot(fit_max) 
#Gráfico de Probabilidades: Podemos notar que se llega adaptar bien las probabilidades reales a las teóricas predichas por el modelo GEV i/(N+1)
#Gráfico de Quantil:Las magnitudes van de 6 a 9, notamos que ajusta bien los valores más probables, sin embargo en los extremos predichos ya se alejan de los datos reales
#y aumenta la incertidumbre (esto también se da por el signo del shape <0)
#Gráfico de los niveles de retorno: Nos muestra que los sismos entre 6 a 7.5, ocurren en periodos de 0.5 a 5 años, sin embargo, los sismos de por ejemplo de 8,
#ocurren cada 50 años
par(mfrow = c(1, 1))

#Máxima de estabilidad
#por los hablado anteriomente, los datos se llegan a ajustar bien a un modelo GEV, por lo tanto, decimos que es la única distribución de los datos que es máxima estable
# Extremal Types Theorem
#y dado que el valor de xi<0, estamos en una GEV reversed  Weibull

mrlplot(datos$magnitude, main="Gráfico de Vida Media Remanente para Sismos")
limites_umbral <- quantile(datos$magnitude, probs = c(0.50, 0.95))
par(mfrow=c(2,1))
tcplot(datos$magnitude, tlim=limites_umbral)
par(mfrow=c(1,1))
################################################################################

##Excedencias
#parámetros iniciales loc, scale, shape aleatorios
fit_rd<-fpot(datos$magnitude,threshold=5.8, model="pp", start=list(loc=10,scale=1,shape=0.1),
             npp=365.25*24)
fit_rd$estimate
fit_rd$std.err
par(mfrow=c(2,2))
plot(fit_rd)
par(mfrow=c(1,1))
     
#parámetros iniciales loc, scale, shape obtenidos por EGV
p_egv<-fit_max$estimate
fit_ex<-fpot(datos$magnitude,threshold=5.8, model="pp", start=as.list(p_egv),
             npp=365.25*24)
fit_ex$estimate
fit_ex$std.err
par(mfrow=c(2,2))
plot(fit_ex)
par(mfrow=c(1,1))

#sin parámetros iniciales y modelo="gdp"
fit_np<-fpot(datos$magnitude, threshold = 5.8)
fit_np$estimate
fit_np$std.err
par(mfrow=c(2,2))
plot(fit_np)
par(mfrow=c(1,1))

#PPP
indices_excedencias <- which(datos$magnitude > 5.8)
tiempos_llegada <- diff(indices_excedencias)
qqplot(qexp(ppoints(length(tiempos_llegada))), tiempos_llegada,
       main = "Diagnóstico de Poisson: Tiempos entre Sismos Extremos",
       xlab = "Cuantiles Teóricos Exponenciales",
       ylab = "Tiempo Observado entre Excedencias (Índices)",
       pch = 19, col = "blue")

# 4. Añadir la línea de referencia
qqline(tiempos_llegada, distribution = qexp, col = "red", lwd = 2)

library(spatstat.geom)
library(spatstat.random)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(leaflet)
################################################################################
##############################EJERCICIO_1#######################################

#ventana
W<-owin(c(0,1),c(0,1))

#patrón aleatorio
X1<-rpoispp(lambda = 1000, win=W) #lambda la intensidad del Proceso de Poisson
plot(X1,main="Poisson homogeneo")

#Patrón agregado
X2<-rThomas(kappa=5,scale=0.01,mu=100,win=W) #kappa la intensidad de los puntos padre
#scale la desviación estándar al punto padre del cluster, #mu la media de puntos ubicados en cada cluster
plot ( X2 , main = " Agregado ")

#Patrón regular
X3 <- rSSI ( r = 0.001 , n = 100 , win = W ) #r es la distancia entre los puntos (debe respetar la ventana)
#n el número máximo de puntos permitidos
plot ( X3 , main = " Regular ")

#Responder:
#1)El patrón que más se asemeja a delitos en el agregado
#2)La posible clusterización en zonas peligrosas aumentan la intensidad de futuros crimenes
#3)Ver el intervalo de tiempo que transcurre entre eventos, ubicaciones más precisas

################################################################################
##############################EJERCICIO_2#######################################
datos<-readRDS("base_original.rds")
quito<-datos%>%filter(canton=="QUITO")
#Graficar los puntos sobre la región de estudio.
mapa<-leaflet(quito) %>%
  addTiles() %>%  
  addCircleMarkers(
    lng = ~longitud, 
    lat = ~latitud,
    radius = 3,       # Tamaño de los puntos
    color = "#130f40", # Color del contorno
    fillOpacity = 0.6, # Transparencia del relleno
    stroke = FALSE     
  )
mapa

#Definir claramente la ventana de observación W
W <- ripras(quito$longitud, quito$latitud) #nos aproxima una ventana que se ajuste mejor los datos
patron <- ppp(quito$longitud, quito$latitud, window = W)

#Calcular N(W) y conteos por subregión.
conteo_eventos <- npoints(patron)

#Describir visualmente el patrón observado.
plot(patron, main="Datos de Quito con una ventana convexa")
#existe una agregación de eventos en el eje vertica, más en específico al norte y al sur de la misma

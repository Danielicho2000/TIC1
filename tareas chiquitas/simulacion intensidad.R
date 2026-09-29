library(spatstat)
set.seed(123)
X <- rpoispp(function(x, y) 200 * exp(-3*x))
# Conteos por cuadrantes
Q <- quadratcount(X, nx = 4, ny = 4)
Q
plot(X, main = "Patron puntual")
plot(Q, add = TRUE, col = "red")

#KERNEL
X<- rpoispp(function(x, y) 200 * exp(-3*x))
lambda_hat <- density(X, sigma = 0.05)
plot(lambda_hat, main = "Intensidad estimada") #para un objeto PPP calcula una estimación
#del kernel de la intensidad
plot(X, add = TRUE, pch = 16, cex = 0.5)
par(mfrow = c(1, 3))
plot(density(X, sigma = 0.02), main = "sigma = 0.02")
plot(X, add = TRUE, pch = 16, cex = 0.4)
plot(density(X, sigma = 0.08), main = "sigma = 0.08")
plot(X, add = TRUE, pch = 16, cex = 0.4)
plot(density(X, sigma = 0.20), main = "sigma = 0.20")
plot(X, add = TRUE, pch = 16, cex = 0.4)

#Valores de sigma propuestos por el paquete
bw1 <- bw.diggle(X)
bw1

bw2 <- bw.ppl(X)
bw2

plot(density(X, sigma = bw1), main = "bw.diggle")
plot(density(X, sigma = bw2), main = "bw.ppl")

lambda_hat <- density(X, sigma = 0.08)
class(lambda_hat)
summary(lambda_hat)
lambda_hat[0.5, 0.5]
#Intensidad estimada esperanza del número de eventos en la región
integral(lambda_hat)
#debería ser cercano a:
npoints(X)


#######################################################
#SIMULACIÓN
W <- owin(c(0, 1), c(0, 1))
lambda_true <- function(x, y) {
  200 * exp(-3*x)
}
set.seed(2026)
X <- rpoispp(lambda_true, win = W)
#lambda estimado
lambda_est <- density(X, sigma = 0.08)
plot(lambda_est, main = "Intensidad estimada")
plot(X, add = TRUE, pch = 16, cex = 0.5)
#comparación
lambda_im <- as.im(lambda_true, W = W)
par(mfrow = c(1, 2))
plot(lambda_im, main = "Intensidad verdadera")
plot(lambda_est, main = "Intensidad estimada")

#mass function
lambda_hat <- density(X, sigma = 0.08)
# Superficie que integra aproximadamente 1
f_hat <- lambda_hat / integral(lambda_hat)
integral(f_hat)
plot(f_hat, main = "Densidad espacial relativa")


#######################################################
#EJEMPLO CON VENTANA POLIGONAL
px <- c(0, 1, 1, 0.6, 0, 0)
py <- c(0, 0, 0.8, 1, 0.7, 0)
Wpoly <- owin(poly = list(x = px, y = py))
Xpoly <- rpoispp(150, win = Wpoly)
plot(Xpoly)
lambda_poly <- density(Xpoly, sigma = 0.08)
plot(lambda_poly, main = "Intensidad en ventana irregular")
plot(Xpoly, add = TRUE, pch = 16, cex = 0.5)


#######################################################
#EJERCICIO 1
W1<-owin(c(0,1),c(0,1))
lambda_t<-function(x,y){
  150*(1+2*x)
}
set.seed(123)
X1<-rpoispp(lambda_t,win=W1)
Q <- quadratcount(X1, nx = 4, ny = 4)
print(Q)
plot(X1, main = "Proceso de Poisson No Homogéneo", pch = 20)
plot(Q, add = TRUE, col = "red", cex = 1.5, textargs=list(col="red", font=2))

#EJERCICIO 2
par(mfrow = c(1, 3))
plot(density(X1, sigma = 0.03), main = "sigma = 0.03")
plot(X1, add = TRUE, pch = 16, cex = 0.4)
plot(density(X1, sigma = 0.1), main = "sigma = 0.1")
plot(X1, add = TRUE, pch = 16, cex = 0.4)
plot(density(X1, sigma = 0.25), main = "sigma = 0.25")
plot(X1, add = TRUE, pch = 16, cex = 0.4)
#La respuesta debe discutir sesgo, varianza y escala espacial.
#En el primer gráfico se evidencia un sobreajuste de la intensidad (podría ser muiy sensible)
#se podría decir que su varianza es muy alta (por el rango de la escala de valores)
#y existe un sesgo alto en cuanto a que un punto aumente la probabilidad
#de generar otro muy cercano, y no respeta la generación de puntos del proceso puntual

#En el segundo gráfico ya no existe un sobreajuste de la intensidad
#y su varianza sea menor al anterior gráfico, se evidencia un sesgo 
#en los picos intermedios, los puntos a su alrededor generan

#El tercer gráfico, tiene la menor varianza y el sesgo no es tan evidente
#en cuanto a que la presencia de un punto genere otros más muy cercanos
#Además es el gráfico que mejor se ajusta al proceso puntual X, crecimiento
#de izq a derecha

#EJERCICIO 3
bw1 <- bw.diggle(X1)
bw1

bw2 <- bw.ppl(X1)
bw2
par(mfrow=c(1,2))
plot(density(X1, sigma = bw1), main = "bw.diggle")
plot(X1, add = TRUE, pch = 16, cex = 0.4)
plot(density(X1, sigma = bw2), main = "bw.ppl")
plot(X1, add = TRUE, pch = 16, cex = 0.4)
#No, dependiendo de que método automático se use, la escala será
#distinta y no necesariamente interprete el fenómeno

################################################################################
#SIMULACIÓN DE LA INTENSIDAD CON LOS DATOS DE HOMICIDIOS
#Se trabajarán con los datos obtenidos en la parroquia de Quitumbe
library(dplyr)
datos<-base_original%>%filter(distrito=="QUITUMBE")
w <- owin(
  xrange = range(datos$longitud, na.rm = TRUE),
  yrange = range(datos$latitud, na.rm = TRUE) 
  )
datos_ppp <- ppp(
  x = datos$longitud,
  y = datos$latitud,
  window = w 
)
sigma_diggle <- bw.diggle(datos_ppp)
sigma_cv <- bw.ppl(datos_ppp)
print(sigma_diggle)
print(sigma_cv) #grados
lambda_quitumbe_d<-density(datos_ppp, sigma = sigma_diggle)
plot(lambda_quitumbe_d, main = "Intensidad de asesinatos")
plot(datos_ppp, add = TRUE, pch = 16, cex = 0.5)


lambda_quitumbe_c<-density(datos_ppp, sigma = sigma_cv)
plot(lambda_quitumbe_c, main = "Intensidad de asesinatos")
plot(datos_ppp, add = TRUE, pch = 16, cex = 0.5)


################################################################################
################################################################################
################################################################################
#Sesión 5
set.seed(123)
X_pois<-rpoispp(lambda = 100) #CSO
plot(X_pois, main = "Poisson homogeneo")
#F_pois: teórica
#F_bord: estimador de F itulizando corrección de borde
#F_cs: estimador de F mediante Chiu-Stoyan
plot(Fest(X_pois)) #Cumple con el CSR
#G_pois (azul): teorica
#G_km: añade los puntos censurados en el cálculo global
#G_bord: Estimador con corrección de borde
#G_han
plot(Gest(X_pois))
plot(Jest(X_pois))
#K_pois: teórica
#K_iso: correción isoptrópica de Ripley, da más peso a los puntos 
#dentro del mapa y de un círuclo de radio r
#K_tras: corrección de traslación
#K_bord: correción de borde clásica
plot(Kest(X_pois))
plot(Lest(X_pois))

X_clust <- rThomas(kappa = 10, scale = 0.04, mu = 10)
plot(X_clust, main = "Patron agrupado")
plot(Kest(X_clust)) #>pi*r^2
plot(Lest(X_clust), . - r ~ r) #>0
plot(Jest(X_clust)) #<1

A <- allstats(X_pois)
plot(A)


lambda_hat <- density(X_clust)
K_inhom <- Kinhom(X_clust, lambda = lambda_hat)
plot(K_inhom)

fit <- ppm(X_clust ~ x + y)
K_inhom_fit <- Kinhom(X_clust, lambda = predict(fit)) #evidencia de intensidad no
#constante en este caso la intensidad sigue una funciónj lineal
plot(K_inhom_fit)


################################################################################
#EJERCICIO
set.seed(321)
X1 <- rpoispp(100)
plot(X1, main="CSR")
plot(Fest(X1))
plot(Gest(X1))
plot(Jest(X1))
plot(Kest(X1))
plot(Lest(X1))
#Se evidencia CSR
X2 <- rThomas(kappa = 8, scale = 0.04, mu = 12)
plot(X2, main="agrupado")
plot(Fest(X2))
plot(Gest(X2))
plot(Jest(X2))
plot(Kest(X2))
plot(Lest(X2))
#Se evidencia agrupamiento
X3 <- rSSI(r = 0.055, n = 90)
plot(X3, main="regular")
plot(Fest(X3))
plot(Gest(X3))
plot(Jest(X3))
plot(Kest(X3))
plot(Lest(X3))
#Se evidencia regularidad

################################################################################
#Prueba naive contra CSR
X<-rpoispp(lambda = 100)
plot(X, main="Observado")

# X <- ppp(x = datos$x,
#          y = datos$y,
#          window = W)

lambda_hat<-intensity(X)
Y1 <- rpoispp(lambda_hat, win = Window(X))
Y2 <- rpoispp(lambda_hat, win = Window(X))
Y3 <- rpoispp(lambda_hat, win = Window(X))
par(mfrow = c(1, 3))
plot(Y1, main = "Simulacion 1")
plot(Y2, main = "Simulacion 2")
plot(Y3, main = "Simulacion 3")
par(mfrow = c(1, 1))

#2da prueba mejorada
E_K<-envelope(X, fun=Kest, nsim=99)
plot(E_K, main="Envolvente Monte Carlo para K")
E_l<-envelope(X,fun=Lest, nsim=99)
plot(E_l, main="Envolvente Monte Carlo para L")
#Para L(r)-r
L_X<-Lest(X)
plot(L_X,.-r~r,main="Función L centrada: L(r)-r",ylab="L(r)-r")
E_L<-envelope(X,Lest,nsim=99)
plot(E_L, .-r~r, main="Envolvente para L(r)-r")


#Envolventes globales
E_g<-envelope(X, fun=Lest, nsim = 99, global = TRUE)
plot(E_g, .-r~r, main="Envolvente global para L(r)-r")  


#patrón agrupado
X_cluster <- rThomas(kappa = 10,
                     scale = 0.04,
                     mu = 10)
plot(X_cluster,
     main = "Proceso de Thomas")
E_cluster <- envelope(X_cluster,
                      Lest,
                      nsim = 99,
                      global = TRUE)
plot(E_cluster, . - r ~ r,
     main = "Agrupamiento: L(r) - r") 

#patrón regular
X_regular <- rSSI(r = 0.06, n = 100)
plot(X_regular,
     main = "Patron regular")
E_regular <- envelope(X_regular,
                      Lest,
                      nsim = 99,
                      global = TRUE)
plot(E_regular, . - r ~ r,
     main = "Regularidad: L(r) - r")

#Patrón inhomogéneo
#H0 Poisson no homogéneo con intensidad lambda(u)
lambda_fun <- function(x, y) 200 * exp(-3 * x)
X_inhom <- rpoispp(lambda_fun)
plot(X_inhom,
     main = "Poisson no homogeneo")
E_wrong <- envelope(X_inhom,
                    Lest,
                    nsim = 99,
                    global = TRUE)
plot(E_wrong, . - r ~ r)

lambda_est <- density(X_inhom)
K_inhom <- Kinhom(X_inhom,
                  lambda = lambda_est)
plot(K_inhom,
     main = "Funcion K inhomogenea") #existe evidencia de repulsión
#otra alternativa
Z <- as.im(function(x, y) x, W = Window(X_inhom)) #covariable exógena
fit <- ppm(X_inhom ~ Z) #relación log-lineal B0=200, B1=-3, modelamos
#la inhomogeneidad, la intensidad de X-inhom cambia a lo largo de Z
summary(fit)
E_fit <- envelope(fit,
                  fun = Kinhom,
                  nsim = 99,
                  global = TRUE) #genera 99 mapas simulados
#siguiendo estricamente la ecuación log-lineal
plot(E_fit,
     main = "Envolvente bajo modelo ajustado")




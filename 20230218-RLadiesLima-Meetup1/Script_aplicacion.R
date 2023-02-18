rm(list=ls(all=TRUE))  # Clean workspace

setwd("C:/Users/Luisa Ames/Desktop/TALLER PANEL RSTUDIO/CLASE 6/sesion6")

install.packages("Ecdat") # instalamos el paquete (solo una vez)
library(Ecdat) # cargamos el paquete

head(Cigar)
attach(Cigar) # incorporamos los datos a R para referenciar cada variable por el nombre anterior
# state = identificador del estado
# year = año
# price = precio del paquete de cigarros
# pop = población
# pop16 = población con más de 16 años
# cpi = índice de precios del consumo (1983=100)
# ndi = ingreso disponible per cápita
# sales = venta de cigarrillos en paquetes per capita
# pimin = precio mínimo en estados adyacentes por paquete de cigarrillos


# A continuación vamos a analizar el modelo que estudia las ventas del paquete 
# de cigarrillos (sales) en función de su precio (price), de la población con más 
# de 16 años (pop16) y el ingreso disponible per cápita (ndi). 
# Puesto que se disponen de datos de panel con 46 individuos (estados) y 30 periodos (años):
dim(table(state,year))


# Heterogeneidad a traves de estados y años
#--------------------------------------------
install.packages("gplots")
library(gplots)
plotmeans(sales ~ state, main="Heterogeineity across states", data=Cigar)
plotmeans(sales ~ year, main="Heterogeineity across years", data=Cigar, n.label=F)


#---------------------------
# Regresion lineal multiple
#---------------------------
#  En este caso, se considera que el término independiente es el mismo para 
# todos los periodos y/o tiempos:
reg.mco = lm(sales~price+pop16+ndi)
summary(reg.mco)

install.packages("stargazer")
library(stargazer)
stargazer(reg.mco, type="text", out="ols.text")
# Aumenta precio del paquete, disminuye la venta de cigarrillos
# Aumenta el ingreso disponible per capita, aumenta la venta de cigarrillos.

yhat <- reg.mco$fitted
yhat

plot(price, sales, pch=19, xlab="Prices", ylab="Sales")
abline(lm(sales~price), lwd=3, col="red")
# Esta regresion no considera heterogeneidad a traves de estados o tiempo

#-----------------------------
# Regresion con efectos fijos
#-----------------------------
# Si se considera que el término indpendiente puede ser distinto para cada 
# individuo y/o periodo debe considerarse un enfoque de efectos fijos. 
# Así, la estimación intra grupos (o within) puede realizarse mediante el 
# uso de variables binarias que recojan el efecto temporal o transversal:
reg.fijos.ind = lm(sales~price+pop16+ndi+factor(state))
summary(reg.fijos.ind)

# factor(year) esto para agregar dummies
reg.fijos.peri = lm(sales~price+pop16+ndi+factor(year))
summary(reg.fijos.peri)

reg.fijos.periodo = lm(sales~price+pop16+ndi+factor(year)-1) 
summary(reg.fijos.periodo)

stargazer(reg.mco, reg.fijos.ind , type="text", out="logit.text")
stargazer(reg.mco, reg.fijos.peri , type="text", out="logit.text")


# Otra opcion disponible es usar el paquete plm:
install.packages("plm") 
library(plm) 

# Modelo within:
#----------------
reg.fijos.within = plm(sales~price+pop16+ndi, 
                       index=c("state", "year"), 
                       model="within", 
                       data=Cigar)
summary(reg.fijos.within)
# 1. Los coeficientes estimados indican como varía la variable dependiente sobre 
# el periodo, en media por individuo, cuando las independientes aumentan una unidad.
# 2. El término independiente no aparece ya que el modelo estimado es el de desviaciones
# con respecto a la media. De hecho, elimina todas las variables constantes en cada individuo.

# efectos individuales de cada individuo
fixef(reg.fijos.within)

stargazer(reg.fijos.ind, reg.fijos.within, type="text", out="fixed.text")

# H0: modelo agrupado (MCO) vs H1: efectos fijos
pFtest(reg.fijos.within, reg.mco) 
# Mediante pFtest se realiza el contraste con H0: OLS es mejor que efectos fijos. 
# Se rechaza H0 (p-valor menor que 0.05), luego los efectos fijos son preferibles para este caso


# Modelo between:
#----------------
# Los modelos entre grupos o between analizan la variabilidad entre unidades de seccion 
# cruzada, por tanto, usan las medias de los datos temporales de cada individuo:
reg.fijos.between = plm(sales~price+pop16+ndi, 
                        index=c("state", "year"), 
                        model="between", 
                        data=Cigar)
summary(reg.fijos.between)
# En la practica, se utilizan poco porque los modelos con efectos aleatorios son superiores 
# y el estimador between ignora la informacion temporal existente dentro de los individuos.


#----------------------------------
# Regresion con efectos aleatorios
#----------------------------------
# Si se considera que el termino independiente es una variable aleatoria, 
# debe considerarse un enfoque de efectos aleatorios:
reg.aleatorios = plm(sales~price+pop16+ndi, 
                     index=c("state", "year"), 
                     model="random", 
                     data=Cigar)
summary(reg.aleatorios)
# El valor estimado de theta: si es próximo a 0 efectos aleatorios se asemeja más a los resultados 
# obtenidos por el modelo agrupado mientras si lo está a 1 a los de efectos fijos.
# En este caso es 0.8686.
# Para interpretar los coeficientes se ha de tener en cuenta que se analiza el efecto de las variables 
#independientes sobre la dependiente cuando las primeras aumentan una unidad controlando las diferencias del individuo y periodo.

#----------------------
# Seleccion de modelos
#----------------------
# H0: efectos aleatorios vs H1: efectos fijos
phtest(reg.fijos.within, reg.aleatorios)
# Mediante phtest se realiza el contraste de Hausman con hipótesis nula el modelo de efectos aleatorios es mejor que el de efectos fijos. 
# Se rechaza H0 (p-valor<0.05), luego los efectos fijos son preferibles.

# Resultados
#------------
# Se tiene que el enfoque idóneo es el de efectos fijos. En tal caso:
# El precio influye negativamente en la ventas de cigarrillos.
# La población con más de 16 años y el ingreso disponible per cápita influyen positivamente en las ventas de cigarrillos.
# El modelo es válido conjuntamente, ya que el p-valor es menor que 0.05.


#--------------------
# Otros diagnosticos
#--------------------

# Testing for time-fixed effects 
#--------------------------------
fixed = plm(sales~price+pop16+ndi, index=c("state", "year"), model="within", data=Cigar)
fixed.time =plm(sales~price+pop16+ndi+factor(year), index=c("state", "year"), model="within", data=Cigar)
summary(fixed.time)

# H0: no necesita efectos fijos-tiempo
pFtest(fixed.time, fixed)
# Si p.value<0.05, se necesita efectos fijos-tiempo.
plmtest(fixed, c("time"), type=("bp"))

# Testing for cross-sectional dependence 
#----------------------------------------
# Dependencia cross-sectional es un problema en paneles macro con series de largo tiempo.
# Este no es un problema en paneles micro (pocos años y gran numeros de casos)

# H0 en BP/LM y Pasaran CD tests de independencia es que residuos a traves de 
# estados no estan correlacionados. 
pcdtest(fixed, test=c("lm"))
pcdtest(fixed, test=c(("cd")))
# Hay dependencia cross-sectional
# *Source: Hoechle, Daniel, “Robust Standard Errors for
# Panel Regressions with Cross-Sectional Dependence”,
# http://fmwww.bc.edu/repec/bocode/x/xtscc_paper.pdf


# Testing for serial correlation
#--------------------------------
# Aplica para paneles macro con series de tiempo largo. No es un problema micro (pocos años)
# H0: no hay correlacion serial
pbgtest(fixed)
# Hay correlacion serial


# Testing for unit roots / stationarity
#---------------------------------------
# Dickey-Fuller test para revisar tendencias estocasticas. 
# H0: series tienen raiz unitaria (ejemplo: no estacionariedad)
# Si raiz unitaria esta presente, puedes tomar primeras diferencias.
Panel.set<-plm.data(Cigar, index=c("state","year"))
install.packages("tseries")
library(tseries)
adf.test(Panel.set$sales, k=2)
# Si p-value es <0.05, entonces no hay presencia de raiz unitaria.


# Testing for heteroskedasticity
#--------------------------------
# H0: Test Breusch-Pagan test es homocedasticidad está presente ( residuos se distribuyen con la misma varianza)
library(lmtest)
bptest(sales~price+pop16+ndi + factor(year), data=Cigar, studentize=F)
# Si p<0.05, hay presencia de heterocedasticidad

# Si heterocedasticidad es detectada, se puede usar matriz de covarianzas robustos


# Controlando para heterocedasticidad: Estimacion matriz de covarianza robust (Estimador sandwich)
#--------------------------------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/plm/vignettes/plm.pdf
# https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf

# The --vcovHC– function estimates three heteroskedasticity-consistent covariance estimators:
# "white1" - for general heteroskedasticity but no serial correlation. Recommended for random effects.
# "white2" - is "white1" restricted to a common variance within groups. Recommended for random effects.
# "arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.

# The following options apply*:
# HC0 - heteroskedasticity consistent. The default.
# HC1,HC2, HC3 – Recommended for small samples. HC3 gives less weight to influential observations.
# HC4 - small samples with influential observations
# HAC - heteroskedasticity and autocorrelation consistent (type ?vcovHAC for more details)

# Controlling for heteroskedasticity: Fixed effects
#----------------------------------------------------
fixed <- plm(sales~price+pop16+ndi, data=Cigar, index=c("state", "year"), model="within")
coeftest(fixed)         # Original coefficients
coeftest(fixed, vcovHC) # Heteroskedasticity consistent coefficients
coeftest(fixed, vcovHC(fixed, method = "arellano")) # Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3

# The following shows the HC standard errors of the coefficients
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed, type = x)))))

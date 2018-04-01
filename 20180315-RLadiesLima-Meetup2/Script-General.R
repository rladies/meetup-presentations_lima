### Entorno de RStudio y Configuración ###

# Ejemplo en la presentación del entorno
x<-1:10
y<-rnorm(10)
plot(x,y)

## Configuración de Directorio de Trabajo

# Directorio de trabajo actual
getwd()

# Cambio de directorio de trabajo
setwd("")

# Verificación de nuevo directorio de trabajo
getwd()

### Instalación y Uso de Paquetes ###

## Instalación

# Desde CRAN
install.packages("nombre_del_paquete")

# Desde GitHub
install.packages("devtools")
library(devtools)
install_github("ubicación del paquete dentro de GitHub")

## Uso

library("nombre_del_paquete")
?nombre_del_paquete

### Creación de Objetos ###

## Vectores

# Vector numérico
x<-c(1.5,2.2,-5)
x

# Vector de caracteres
a<-c("norte","sur","este","oeste")
a

# Vector lógico escalar
b<-F
b

# Vector lógico
d<-c(T,F,F,F,T,F,T)
d

# Vector con dato complejo
e<-2+3i
e

# Vector "sec1" con una secuencia de 0.2 en 0.2 desde 1 hasta 3
sec1<-seq(1,3,0.2)
sec1

# Vector "sec2" con una secuencia de 12 valores entre 1 y 14
sec2<-seq(length=12,from=1,to=14)
sec2

# Vector "v1" con una secuencia de 1 hasta 6
v1<-c(1:6)
v1

# Vector "v2" con una secuencia de pi hasta 6
v2<-pi:6
v2

# Vector "v3" con una secuencia de 6 hasta pi (orden descendente)
v3<-6:pi
v3

# Vector "v4" con una secuencia de 5 hasta -1 (orden descendente)
v4<-5:-1
v4

# Vector "v5" que repite '1, 2, 3' 3 veces
v5<-rep(c(1,2,3),3)
v5

# Vector "v6" que repite 'A, B, C' 3 veces
v6<-rep(c("A","B","C"),3)
v6

# Vector "y"
y<-c(1,2,4,1,4,12,13,23,34,54,56)
y

# Selecciona el 2do valor del vector "y"
y[2]

# Selecciona el 1er y 3er valor del vector "y"
y[c(1,3)]

# Selecciona todos los valores excepto el 1er y 3er valor del vector "y"
y[-c(1,3)]

# Selecciona los 4 primeros valores del vector "y"
y[1:4]

# Selecciona el 5to valor del vector "y" y lo almacena en el objeto "w"
w<-y[5]
w

## Matrices

# Matriz "m1" de 3 filas y 4 columnas con una secuencia de 1 a 12 llenada por columnas.
m1<-matrix(1:12,3,4)
m1

# Matriz "m2" de 3 filas y 4 columnas con una secuencia de 1 a 12 llenada por filas.
m2<-matrix(1:12,3,4,byrow=TRUE)
m2

# Matriz "m3" de 3 filas y 3 columnas con una secuencia de 1 a 9 llenada por columnas.
m3<-matrix(1:9,3,3)
m3

# Selecciona el elemento de la fila 1 - columna 1
m3[1,1]

# Selecciona los elementos de la columna 1
m3[,1]

# Selecciona los elementos de la fila 1
m3[1,]

# Selecciona los elementos de las filas 1 y 2
m3[c(1,2),]

# Selecciona los elementos de las columnas 1 y 2
m3[,c(1,2)]

# Selecciona todos los elementos de la matriz, excepto las filas 1 y 2
m3[-c(1,2),]

# Selecciona todos los elementos de la matriz, excepto las columnas 1 y 2
m3[,-c(1,2)]

## Data Frames

# Data frame "df1" creado por 3 vectores diferentes
df1<-data.frame(nombres=c("Clase1","Clase2","Clase3"),numeros=c(3,4,6),
                numeros1=c(3,4,1))
df1

# Selecciona los elementos de "nombres" en "df1"
df1$nombres

# Selecciona los elementos de "numeros" en "df1"
df1$numeros

# Selecciona los elementos de "numeros1" en "df1"
df1$numeros1

## Listas

# Creación de una lista a partir de 4 objetos diferentes
vector<-1:10
m<-matrix(2:13,4,3)
datos<-iris[10:16,3:5] # usando el dataset "iris"
lista<-list(vector, m, datos, mes="marzo")
lista

# Selecciona el 1er objeto de "lista"
lista[[1]]

# Selecciona el objeto "mes" de "lista"
lista$mes

# Selecciona la fila 1 del 2do objeto de "lista"
lista[[2]][1,]

## Inspección de objetos

# Clase de un objeto
class(sec1)
class(m3)
class(df1)
class(lista)

# Longitud de un objeto
length(sec1)
length(m3)
length(df1)
length(lista)

# Dimensión de un objeto
dim(m3)
dim(df1)
iris
dim(iris)

# Primeras filas o elementos del objeto
head(seq(1,100,2))
head(m3,2)
head(iris)

# Estructura del objeto
str(sec1)
str(m3)
str(df1)
str(iris)

# Nombres de los objetos creados
ls()

# Borrar un objeto
rm(sec1)
sec1

# Borrar todos los objetos creados
rm(list=ls())

### Importación y Exportación de Datos ###

# Guardar un objeto con extensión .rdata 
write.table(df1, file = "df1taller.rdata")

# Leer un objeto guardado con extensión .rdata
df1taller<-read.table("df1taller.rdata")
df1taller

# Leer un archivo con extensión .csv

fiscal<-read.csv2("Fiscalizacion.csv", sep=";", header = TRUE)
fiscal
str(fiscal)

# Leer un archivo Excel
# Instalar paquete gdata
install.packages("gdata")
library(gdata)
fisc<-read.xls("Fiscalizacion.xlsx", sheet = 1)
fisc

### Operaciones Matemáticas Básicas y Estadística Descriptiva ###

# Suma
3+5

# Resta
2-6

# Multiplicación
2*15

# División
3/4

# Potencia
2^5

# Operaciones combinadas
(3-2)+(2/3)+2*3^4

# Módulo
5%%2

# División de enteros
5%/%2

# Creando vector "g"
g<-seq(1,20,0.5)
g
# Media aritmética
mean(g)

# Variancia
var(g)

# Desviación estándar
sd(g)

# Valor mínimo
min(g)

# Valor máximo
max(g)

# Valor mínimo y máximo
range(g)

# Correlación
# Usando dataset "rock"
rock
cor(rock)

# Mediana
median(g)

# Percentil
quantile(g, 0.25)

# Estadísticas de resumen
# Usando dataset "rock"
summary(rock)
# Usando dataset "iris"
summary(iris)

### Gráficos estadísticos ###

# Usando los datos del vector "peso"
peso<-c(67.6, 69.8, 68.5, 70.6, 68.1, 66.5, 65.1, 68.8, 70.3, 65.7, 66.2, 67.1, 67.7,
        68, 65.9, 65, 70.3, 68, 70, 65.2, 69.8, 68.5, 67.5, 66.2, 66.3, 68.1, 67.1, 66.2, 
        71.5, 66.4, 68.8, 67.5, 65.6, 68, 67, 71.3, 66.3, 62.4, 64, 65.4, 72, 72.4)

## Histogramas

h1<-hist(peso)

# Especificando el número aproximado de clases
h2<-hist(peso,breaks=9)

# Especificando que la frecuencia será relativa
h3<-hist(peso,breaks=5,freq=F)

# Añadiendo especificaciones para el título, etiquetas de los ejes y color.
h4<-hist(peso,breaks=5,freq=F,main="Histograma de pesos",xlab="peso",ylab="Frecuencias relativas",col="green")

# Especificando los límites de los ejes x e y.
h5<-hist(peso,breaks=5,freq=F,main="Histograma de pesos",xlab="peso",ylab="Frecuencias relativas",col="green",
         xlim=c(60,74),ylim=c(0,0.25))

# Para eliminar el borde
h6<-hist(peso,breaks=5,freq=F,main="Histograma de pesos",xlab="peso",ylab="Frecuencias relativas",col="green",
         xlim=c(60,74),ylim=c(0,0.25), border=F)

## Gráficas de Cajas

boxplot(peso)

# Especificaciones en la extensión de los bigotes, título, color y etiquetas de los ejes
boxplot(peso,range=1.5,main="Gráfica de cajas",col="yellow",xlab="Gráfica",ylab="valores",
        col.main=12)

# Especificación en la escala
boxplot(peso,range=1.5,main="Gráfica de cajas",col="yellow",xlab="Gráfica",ylab="valores",
        col.main=12,boxwex=0.3)

# Boxplot horizontal
boxplot(peso,range=1.5,main="Gráfica de cajas",col="yellow",xlab="Gráfica",ylab="valores",
        col.main=12,boxwex=0.3,horizontal=T)

## Gráficas de Barras

# Usando los datos del vector "grupo"
grupo<-c("A","A","B","B","B","B","AB", "AB","AB","O","O","AB","AB","A","A","A","B","B","B")

barplot(table(grupo))

# Especifica los colores
barplot(table(grupo),col=c("blue","green","red","yellow"))

VADeaths
?VADeaths
# Usando los datos de VADeaths
barplot(VADeaths)

# Especificación de barras yuxtapuestas
barplot(VADeaths,beside=T)

# Barras apiladas con especificación en la densidad de líneas de sombreado
barplot(VADeaths,beside=F,col=c(1,2,3,4,5),density=c(5,15,25,35))

## Gráficas Circulares

pie(c(0.1,0.6,0.3))

# Especificación de etiquetas
pie(c(0.1,0.6,0.3),labels=c("A","B","C"))

# Especificación de colores
pie(c(0.1,0.6,0.3),labels=c("A","B","C"),col=c("yellow","green","blue"))

# Especificación en la densidad de líneas de sombreado
pie(c(0.1,0.6,0.3),labels=c("A","B","C"),col=c("yellow","green","blue"),density=c(15,24,38))

# Otros ejemplos de gráficos en el demo del paquete "graphics"
demo(graphics)

#################################################
### Ejercicio Práctico

sec3<-seq(1,50,2)
sec3

length(sec3)
mean(sec3)
median(sec3)
sec3[4]

str(cars)
cor(cars)

velocidad<-cars$speed
distancia<-cars$dist

mean(velocidad)
sd(velocidad)
var(velocidad)

mean(distancia)
sd(distancia)
var(distancia)

hist(velocidad)
boxplot(distancia)
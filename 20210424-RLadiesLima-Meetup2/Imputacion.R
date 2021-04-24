
#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

#---------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#                #    
#   CASO CENSUS  #

# Conjunto de 32561 observaciones provenientes de un censo 
# poblacional.
# El objetivo es poder predecir el salario de una persona de 
# manera categorica : <=50K o >50K

# Cargando los datos con la funci?n read.csv
census.csv <- read.csv("censusn.csv",sep=";")
str(census.csv) #podemos ver los tipos de datos

# Cargando los datos con la funcion fread() de data.table
library(data.table)
censusn <-fread("censusn.csv",
                header=T, 
                verbose =FALSE, 
                stringsAsFactors=TRUE,
                showProgress =TRUE) #sólo se usa cuando el volumen de la data es muy grande, baja de peso al datasets



str(censusn)

###########################
#  DETECCION DE MISSINGS  #
###########################
#Completitud de la información
#--------------------------------------------------------
# 1. Deteccion de valores perdidos

# Deteccion de valores perdidos con el paquete DataExplorer
library(DataExplorer)
plot_missing(censusn) 

# Para ver las variables con valores perdidos 
which(colSums(is.na(censusn))!=0)

# Para ver cuantas filas tienen valores perdidos
rmiss <- which(rowSums(is.na(censusn))!=0,arr.ind=T)
length(rmiss)

# Para ver el porcentaje de filas con valores perdidos
length(rmiss)*100/dim(censusn)[1]

# Para graficar la cantidad de valores perdidos
library(VIM)
windows()
valores.perdidos <- aggr(censusn,numbers=T)
valores.perdidos
summary(valores.perdidos)

matrixplot(censusn,
           main="Matrix Plot con Valores Perdidos",
           cex.axis = 0.6,
           ylab = "registro")


#----------------------------------------------------
# 2. Eliminacion de datos perdidos
library(funModeling)
census.cl <- na.omit(censusn)
plot_missing(census.cl) 

# Analisis Descriptivos Univariados
#antes de eliminar o imputar y dsps
#Cambia la distribución de las variables/Estabilidad de la poblacion/PSI
df_ad<- profiling_num(df_gdp) 

#---------------------------------------------
# 3. Imputacion con el paquete DMwR/PAramétrica o Univariada
#Computacionalmente poco constoso
library(DMwR)

# Funcion centralImputation()
# Si la variable es numerica (numeric o integer) reemplaza los valores 
# faltantes con la mediana.
# Si la variable es categorica (factor) reemplaza los valores faltantes con 
# la moda. 

census.ci <-centralImputation(censusn)
plot_missing(census.ci) 

#----------------------------------------------------------------------
# 4. Imputar los datos usando mice para las cualitativas

library(mice)
set.seed(123)
mice_imputes = mice(censusn, m=2, maxit = 1, method = "cart")

# Datos imputados
censusn_transformado2 <- complete(mice_imputes,1)

# Verificando la cantidad de valores perdidos
sum(is.na(censusn_transformado2))

plot_missing(censusn_transformado2)

#----------------------------------------------------------------
# 5. Imputando los valores perdidos cuantitativos usando k-nn
# y estandarizando las variables numericas
library(caret)
set.seed(123)
# Definimos el preprocesamiento
preProcValues1 <- preProcess(censusn,
                             method=c("knnImpute","center","scale"))

# Otras opciones: range , bagImpute, medianImpute

preProcValues1

censusn_caret1 <- predict(preProcValues1, censusn)
summary(censusn_caret1)


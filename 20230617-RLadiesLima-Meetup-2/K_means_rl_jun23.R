#RLADIES - SEGMENTACION

#-------- KMEANS -------#


##---EJEMPLO 1

#LECTURA DE ARCHIVO

data=read.csv("C:/Users/jflorb/Desktop/Otros/SHE_LEADS_DATA/Workshop_RFBA/Material_complementario/RFBA/bases/CLIENTES_TC.csv", 
               sep = ",", dec = ",", header = T)
head(data)

#Analisis exploratorio
summary(data)

boxplot(data$EDAD)
boxplot(data$ANTIGUEDAD)
boxplot(data$CONSUMO)


#Antiguedad
#iqr=q3-q1

#LI=q1-1.5*iqr
#Ls=q3+1.5*iqr


LI=((quantile(data$ANTIGUEDAD,.25))-1.5*IQR(data$ANTIGUEDAD))
LS=((quantile(data$ANTIGUEDAD,.75))+1.5*IQR(data$ANTIGUEDAD))
r=c(LI,LS)

r

range(data$ANTIGUEDAD)

data$ANTIGUEDAD=ifelse(data$ANTIGUEDAD>9.5,quantile(data$ANTIGUEDAD,.75),data$ANTIGUEDAD)

boxplot(data$ANTIGUEDAD)

#Estadarizacion de datos
zdata=scale(data[,2:4])
head(zdata)

head(data)

data1=cbind(data,ZEDAD=zdata[,1],
            ZCONSUMO=zdata[,2],
            ZANTIGUEDAD=zdata[,3])

head(data1)

#---Clustering
modeloKMEANS=kmeans(data1[,6:8],3)
modeloKMEANS

#cantidad de elementos en cada cluster
modeloKMEANS$size

(modeloKMEANS$size/nrow(data1))*100

#oPCION2
modeloKMEANS2=kmeans(data1[,6:8],4)
modeloKMEANS2

(modeloKMEANS2$size/nrow(data1))*100


#oPCION3
modeloKMEANS3=kmeans(data1[,6:8],5)
modeloKMEANS3

(modeloKMEANS3$size/nrow(data1))*100


#Añadiendo variable de grupos a data

data1$grupo1=modeloKMEANS$cluster
data1$grupo2=modeloKMEANS2$cluster
data1$grupo3=modeloKMEANS3$cluster


head(data1)

#--Caracterizacion

aggregate(data1[,2:4], by=list(data1$grupo1), FUN=mean)

aggregate(data1[,2:4], by=list(data1$grupo2), FUN=mean)

aggregate(data1[,2:4], by=list(data1$grupo3), FUN=mean)

#-- Nro teorico de grupos (Codo de Jambu)

Errores=NULL
K_MAX=20

for ( i in 1:K_MAX)
 {
  Errores[i]=sum(kmeans(data1[,c(6:8)], centers = i)$withinss)
}
Errores


plot(1:K_MAX, Errores, type="b",
     xlab="Cantidad de cluster",
     ylab="Suma de error",main="Kteorico")



aggregate(data1[,2:4], by=list(data1$grupo2), FUN=mean)

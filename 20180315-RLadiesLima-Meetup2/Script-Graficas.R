###### Registro del peso de 42 personas #######
peso<-c(67.6, 69.8, 68.5, 70.6, 68.1, 66.5, 65.1, 68.8, 70.3, 65.7, 66.2, 67.1, 67.7,
        68, 65.9, 65, 70.3, 68, 70, 65.2, 69.8, 68.5, 67.5, 66.2, 66.3, 68.1, 67.1, 66.2, 
        71.5, 66.4, 68.8, 67.5, 65.6, 68, 67, 71.3, 66.3, 62.4, 64, 65.4, 72, 72.4)

###### HISTOGRAMAS ########
h1<-hist(peso)
h2<-hist(peso,breaks=5)
h3<-hist(peso,breaks=5,freq=F)
h4<-hist(peso,breaks=5,freq=F,main="Histograma de pesos",xlab="peso",ylab="Frecuencias relativas",col="green")
h5<-hist(peso,breaks=5,freq=F,main="Histograma de pesos",xlab="peso",ylab="Frecuencias relativas",col="green",
         xlim=c(60,74),ylim=c(0,0.25))
#Para eliminar el borde
h6<-hist(peso,breaks=5,freq=F,main="Histograma de pesos",xlab="peso",ylab="Frecuencias relativas",col="green",
         xlim=c(60,74),ylim=c(0,0.25), border=F)


##### GRÁFICA DE CAJAS ######
boxplot(peso)
boxplot(peso,range=1.5,main="Gráfica de cajas",col="yellow",xlab="Gráfica",ylab="valores",
        col.main=12)
boxplot(peso,range=1.5,main="Gráfica de cajas",col="yellow",xlab="Gráfica",ylab="valores",
        col.main=12,boxwex=0.3)
boxplot(peso,range=1.5,main="Gráfica de cajas",col="yellow",xlab="Gráfica",ylab="valores",
        col.main=12,boxwex=0.3,horizontal=T)

##### GRÁFICA DE BARRAS ######

grupo<-c("A","A","B","B","B","B","AB", "AB","AB","O","O","AB","AB","A","A","A","B","B","B")

barplot(table(grupo))
barplot(table(grupo),col=c("blue","green","red","yellow"))
#Dataset VADeaths
VADeaths
?VADeaths
#Usando los datos de VADeaths
barplot(VADeaths)
barplot(VADeaths,beside=T)
barplot(VADeaths,beside=F,col=c(1,2,3,4,5),density=c(5,15,25,35))

##### GRÁFICA CIRCULAR #######
pie(c(0.1,0.6,0.3),labels=c("A","B","C"))
pie(c(0.1,0.6,0.3),labels=c("A","B","C"),col=c("yellow","green","blue"))
pie(c(0.1,0.6,0.3),labels=c("A","B","C"),col=c("yellow","green","blue"),density=c(15,24,38))

###### DEMO DEL PAQUETE graphics #######

demo(graphics)
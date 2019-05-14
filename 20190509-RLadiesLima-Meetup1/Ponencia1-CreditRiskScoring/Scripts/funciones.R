######Análisis Univariado ##########

library(XLConnect)
library(lattice)
install.packages("StatMeasures")
library(StatMeasures)
install.packages("agricolae")
library(agricolae)

cruce=function(base1,base2)
{
n=length(base1)
m=nrow(base2)
c=ncol(base2)
tabla_var<-data.frame(CODIGO=numeric(),NOMBRE=character(),TIPO = character())
for(i in 1:m)
{
for(j in 1:n)
  {
  if(base1[j]==base2$NOMBRE[i])
     {
     tablai = base2[i,]
     tabla_var=rbind(tabla_var,tablai)
}  }
}
return(tabla_var)
}


Tabla_Res_Corte=function(vector)
{
tabla_corte=matrix(0,0,4)
colnames(tabla_corte) = c("Valor","Max","Obs","Freq")
per=quantile(vector, probs = seq(0, 1, by = 0.1),na.rm = T)
p1=per[1]
p2=per[2]
p3=per[3]
p4=per[4]
p5=per[5]
p6=per[6]
p7=per[7]
p8=per[8]
p9=per[9]
p10=per[10]
p11=per[11]
tabla_res=rep(0,length(vector))
if (p2 > p1 && p3 > p2 && p4 > p3 && p5 > p4 && p6 > p5 && p7 > p6 && p8 > p7 && p9 > p8 && p9 > p8 && p10 > p9 && p11 > p10)
 {tabla_res=decile(vector)}
else
 {
   if(p3> p1 && p5 > p3 && p7 > p5 && p9 > p7 && p11 > p9)
   {tabla_res=pentile(vector)}    
 }
tabla_res2=table(tabla_res,useNA="ifany")
freq=tabla_res2*100/sum(tabla_res2)
max=tapply(vector,tabla_res,max)
tam_res=length(tabla_res2)
 categorias=names(tabla_res2)
               	if(is.na(categorias[tam_res]))
               	{categorias=c(categorias[-tam_res],"MValues")
                   max=c(max,0)}
tabla_freq=rbind(tabla_corte,cbind(categorias,max,tabla_res2,freq))
return(tabla_freq)
}
####Función general: base: Base total importada; variables:tabla de variables a usar, wb: nombre de libro,nombrehoja: Nombre de la hoja excel para estadisticas,nombrehojafreq: Nombre de hoja excel para frecuencias

Cuanti_Gen2_1=function(base,variables,wb,nombrehoja,nombrehojafreq)
{
lista=variables$NOMBRE
summary1<-data.frame(Codigo=character(),Variable=character(),Obs=numeric(),MValues=numeric(),Min=numeric(),Max=numeric(),Media=numeric(),Moda=numeric(),Var=numeric(),Desv=numeric(),Q1=numeric(),Q3=numeric(),LI=numeric(),LS=numeric(),P1=numeric(),OUTI=numeric(),OUTS=numeric(),P2=numeric(),P3=numeric(),P4=numeric(),P5=numeric(),P6=numeric(),P7=numeric(),P8=numeric(),P9=numeric(),P10=numeric(),P20=numeric(),P30=numeric(),P40=numeric(),P50=numeric(),P60=numeric(),P70=numeric(),P80=numeric(),P90=numeric(),P91=numeric(),P92=numeric(),P93=numeric(),P94=numeric(),P95=numeric(),P96=numeric(),P97=numeric(),P98=numeric(),P99=numeric(),P100=numeric())
frecuencias<-matrix(0,0,3)
colnames(frecuencias) = c("Valor","Obs","Freq")
nv=length(lista)
iniciograf=5
k=0
l=3
for (i in lista)
{
codigo=variables[variables$NOMBRE==i,]$CODIGO
variable=i
obs=nrow(base)-length(which(is.na(base[,i])))
mv =length(which(is.na(base[,i])))
if (max(base[,i],na.rm=T) ==1)
{base[,i]=base[,i]*100}
mini=min(base[,i],na.rm=T)
maxi=max(base[,i],na.rm=T)
media=mean(base[,i],na.rm=T)
##a=table(base[,i])
moda=0
vari=var(base[,i],na.rm=T)
desv=sd(base[,i],na.rm=T)
q1=quantile(base[,i],na.rm=T,0.25)
q3=quantile(base[,i],na.rm=T,0.75)
RIC=q3-q1
li=q1-RIC*1.5
ls=q3+RIC*1.5
base2=base[-mv,i]
outi=length(base2[base2[]<li])*100/obs
outs=length(base2[base2[]>ls])*100/obs
p1=quantile(base[,i],na.rm=T,0.01)
p2=quantile(base[,i],na.rm=T,0.02)
p3=quantile(base[,i],na.rm=T,0.03)
p4=quantile(base[,i],na.rm=T,0.04)
p5=quantile(base[,i],na.rm=T,0.05)
p6=quantile(base[,i],na.rm=T,0.06)
p7=quantile(base[,i],na.rm=T,0.07)
p8=quantile(base[,i],na.rm=T,0.08)
p9=quantile(base[,i],na.rm=T,0.09)
p10=quantile(base[,i],na.rm=T,0.10)
p20=quantile(base[,i],na.rm=T,0.20)
p30=quantile(base[,i],na.rm=T,0.30)
p40=quantile(base[,i],na.rm=T,0.40)
p50=quantile(base[,i],na.rm=T,0.50)
p60=quantile(base[,i],na.rm=T,0.60)
p70=quantile(base[,i],na.rm=T,0.70)
p80=quantile(base[,i],na.rm=T,0.80)
p90=quantile(base[,i],na.rm=T,0.90)
p91=quantile(base[,i],na.rm=T,0.91)
p92=quantile(base[,i],na.rm=T,0.92)
p93=quantile(base[,i],na.rm=T,0.93)
p94=quantile(base[,i],na.rm=T,0.94)
p95=quantile(base[,i],na.rm=T,0.95)
p96=quantile(base[,i],na.rm=T,0.96)
p97=quantile(base[,i],na.rm=T,0.97)
p98=quantile(base[,i],na.rm=T,0.98)
p99=quantile(base[,i],na.rm=T,0.99)
p100=quantile(base[,i],na.rm=T,1)
tablares=data.frame(Codigo=codigo,Variable=variable,Obs=obs,MValues=mv,Min=mini,Max=maxi,Media=media,P1=p1,P2=p2,P3=p3,P4=p4,P5=p5,P95=p95,P96=p96,P97=p97,P98=p98,P99=p99,P100=p100)
summary1=rbind(summary1,tablares)
}
#### Almacenas Estadisticas en Excel ########################
createName(wb, name = "Estadisticas", formula = paste(nombrehoja,"!$B$5",sep=""),overwrite=T)
writeNamedRegion(wb,summary1 , name = "Estadisticas")
saveWorkbook(wb)
}

Cuanti_Gen2_2=function(base,variables,wb,nombrehoja,nombrehojafreq)
{
lista=variables$NOMBRE
summary1<-data.frame(Variable=character(),Obs=numeric(),MValues=numeric(),Min=numeric(),Max=numeric(),Media=numeric(),Moda=numeric(),Var=numeric(),Desv=numeric(),Q1=numeric(),Q3=numeric(),LI=numeric(),LS=numeric(),P1=numeric(),OUTI=numeric(),OUTS=numeric(),P2=numeric(),P3=numeric(),P4=numeric(),P5=numeric(),P6=numeric(),P7=numeric(),P8=numeric(),P9=numeric(),P10=numeric(),P20=numeric(),P30=numeric(),P40=numeric(),P50=numeric(),P60=numeric(),P70=numeric(),P80=numeric(),P90=numeric(),P91=numeric(),P92=numeric(),P93=numeric(),P94=numeric(),P95=numeric(),P96=numeric(),P97=numeric(),P98=numeric(),P99=numeric(),P100=numeric())
frecuencias<-matrix(0,0,3)
colnames(frecuencias) = c("Valor","Obs","Freq")
nv=length(lista)
iniciograf=5
k=0
l=3
for (i in lista)
{
codigo=variables[variables$NOMBRE==i,]$CODIGO
variable=i
if (max(base[,i],na.rm=T) ==1)
{base[,i]=base[,i]*100}
#Almacena nombre y grafico #
 createName(wb, name = "Codigo", formula = paste(nombrehojafreq,"!$A$",l,sep=""),overwrite=T)
 writeNamedRegion(wb,codigo, name = "Codigo")
 createName(wb, name = "NombreVar", formula = paste(nombrehojafreq,"!$B$",l,sep=""),overwrite=T)
 writeNamedRegion(wb,variable, name = "NombreVar")


##h1=hist(base[,i],main=paste("HIST_",i,sep=""),plot=F,xlab=paste(i,sep=""),ylab="FreqRelativa")
h1=hist(base[,i],breaks="Scott",plot=F)
conteo=h1$counts
counts=(h1$counts/sum(h1$counts))

require(lattice)
        png(filename = paste(i,".png",sep=""), width = 500, height = 300)
        devAskNewPage(ask = FALSE)
        ##hist(base[,i],main=paste("HIST_",i,sep=""),plot=T,xlab=paste(i,sep=""))
	  graph.freq(h1,frequency=2,main=paste("HIST_",i,sep=""),xlab=paste(i,sep=""),ylab="FreqRelativa")
        dev.off()
        createName(wb, name = paste("HIST",i,sep=""), formula = paste(nombrehojafreq,"!$P$",l+3,sep=""),overwrite=T)
        addImage(wb, filename = paste(i,".png",sep=""), name = paste("HIST",i,sep=""),
        originalSize = TRUE)
        saveWorkbook(wb)
        tablafreqhist=rbind(h1$breaks[-1],conteo,counts*100)
	  tablafreqhist=cbind(c("Limite Superior","Cantidad","Proporción"),tablafreqhist)
	  colnames(tablafreqhist)=c("TablaFreq",paste("Intervalo",seq(1,ncol(tablafreqhist)-1),sep=""))
	
	   createName(wb, name = "Tabla_Freq_Hist", formula = paste(nombrehojafreq,"!$AJ$",l+3,sep=""),overwrite=T)
         writeNamedRegion(wb,tablafreqhist, name = "Tabla_Freq_Hist")
         
require(lattice)
        png(filename = paste(i,"bp",".png",sep=""), width = 500, height = 300)
        devAskNewPage(ask = FALSE)
        boxplot(base[,i],main=paste("BP_",i,sep=""),plot=T,xlab=paste(i,sep=""))
        dev.off()
        createName(wb, name = paste("BP",i,sep=""), formula = paste(nombrehojafreq,"!$Z$",l+3,sep=""),overwrite=T)
        addImage(wb, filename = paste(i,"bp",".png",sep=""), name = paste("BP",i,sep=""),
        originalSize = TRUE)
        saveWorkbook(wb)


 a=table(base[,i],useNA="ifany")
 tam_freq=length(a)
     if(tam_freq <= 200)
        {
        	tabla_freq=as.matrix(a)
       	 freque=a*100/sum(a)
        	  categorias=names(a)
               	if(is.na(categorias[tam_freq]))
               	{categorias=c(categorias[-tam_freq],"MValues")}
         	tabla_freq=cbind(categorias,tabla_freq,freque)
         	tabla_freq=rbind(frecuencias,tabla_freq)
            tabla_freq_corte=Tabla_Res_Corte(base[,i])
            dist1=nrow(tabla_freq)
            dist2=nrow(tabla_freq_corte)
            distf=ifelse(dist1 > dist2,dist1,dist2) + 2
         }
      else {
          tabla_freq_corte=Tabla_Res_Corte(base[,i])
          tabla_freq = tabla_freq_corte
          distf = nrow(tabla_freq) + 2
         }
         
        createName(wb, name = "Frecuencias", formula = paste(nombrehojafreq,"!$B$",l+3,sep=""),overwrite=T)
        createName(wb, name = "Corte", formula = paste(nombrehojafreq,"!$H$",l+3,sep=""),overwrite=T)
        writeNamedRegion(wb,tabla_freq, name = "Frecuencias")
        writeNamedRegion(wb,tabla_freq_corte, name = "Corte")
        saveWorkbook(wb)
          distf=ifelse(distf > 18,distf,18)
       l = l + distf + 3
    }

}


#####Análisis Univariado Cualitativo


Cualitativo1=function(base,variables,wb,nombrehoja)
{
lista=variables$NOMBRE
summary<-matrix(0,0,3)
colnames(summary)=c("Categorias","Obs","Prop")
nv=length(lista)
n=nrow(base)
or_vec = 0
j = 0
k = 0
l = 0
for(i in lista)
  {
  codigo=variables[variables$NOMBRE==i,]$CODIGO
  variable = i
  obs=t(t(table(base[,i],useNA="ifany")))
  rn=rownames(obs)
  prop=t(t(table(base[,i],useNA="ifany")*100/n))
  tabla1 = cbind(Obs=obs,Prop=prop)
  tam=nrow(tabla1)
  cat=rownames(tabla1)
if(is.na(cat[tam]))
{
cat=c(rn[-tam],"MValues")
}
rownames(tabla1)=cat
tabla2=cbind(cat,tabla1)
tablares=rbind(summary,tabla2)
createName(wb, name = "Codigo", formula = paste(nombrehoja,"!$A$",k+1,sep=""),overwrite=T)
writeNamedRegion(wb,codigo, name = "Codigo")
createName(wb, name = "nombre", formula = paste(nombrehoja,"!$B$",k+1,sep=""),overwrite=T)
writeNamedRegion(wb,variable, name = "nombre")
createName(wb, name = "Frecuencias", formula = paste(nombrehoja,"!$B$",k+3,sep=""),overwrite=T)
writeNamedRegion(wb,tablares, name = "Frecuencias")
createName(wb, name = "Grafico", formula = paste(nombrehoja,"!$G$",k+3,sep=""), overwrite = TRUE)


png(filename = paste(i,".png",sep=""), width = 400, height = 300)
devAskNewPage(ask = FALSE)
barplot(tabla1[,2],legend.text=T,beside=F,main=i,xlab=i,ylab="FreqRelativa",ylim=c(0,100),xlim=c(0,tam+12),col=c("lightblue3","lightcoral","lavenderblush3","cornflowerblue","lemonchiffon3","lightgoldenrod2","burlywood3","darkseagreen4","lightpink4","olivedrab4" , "mistyrose3","seagreen3","tan3", "tan2"))
dev.off()
addImage(wb, filename = paste(i,".png",sep="") , name = "Grafico",originalSize = TRUE)
saveWorkbook(wb)
if(tam>18)
{k=k+tam+5}
else
{k=k+16+5}
  }
}

############Bivariados

library(XLConnect)
library(lattice)
library(StatMeasures)
library(agricolae)

cruce=function(base1,base2)
{
n=length(base1)
m=nrow(base2)
c=ncol(base2)
tabla_var<-data.frame(CODIGO=numeric(),NOMBRE=character(),TIPO = character())
for(i in 1:m)
{
for(j in 1:n)
  {
  if(base1[j]==base2$NOMBRE[i])
     {
     tablai = base2[i,]
     tabla_var=rbind(tabla_var,tablai)
}  }
}
return(tabla_var)
}


calculo_stat=function(base,malo,tipo)
{
tabla=as.matrix(table(base,malo,useNA="ifany"))
tam_tab=nrow(tabla)
categorias=rownames(tabla)
if(is.na(categorias[tam_tab]))
               	{categorias=c(categorias[-tam_tab],"MValues")}

tablab=tabla[,"0"]
tablam=tabla[,"1"]
tablat=tablab+tablam
pd=tablam*100/tablat
por_tot=tablat*100/sum(tablat)
por_malo=tablam*100/sum(tablam)
por_bueno=tablab*100/sum(tablab)
tabla_resumen1=cbind(categorias,tablab,tablam,tablat,pd,por_tot,por_malo,por_bueno)
if(tipo=="CUALITATIVO")
{tabla_resumen1=tabla_resumen1[order(pd),]}
acu_tot=cumsum(tabla_resumen1[,4])*100/max(cumsum(tabla_resumen1[,4]))
acu_malo=cumsum(tabla_resumen1[,3])*100/max(cumsum(tabla_resumen1[,3]))
acu_bueno=cumsum(tabla_resumen1[,2])*100/max(cumsum(tabla_resumen1[,2]))
ks=abs(acu_malo-acu_bueno)
WOE=log((as.numeric(tabla_resumen1[,8])/100)/(as.numeric(tabla_resumen1[,7])/100))*100
WOE[WOE=="Inf"]=0
WOE[WOE=="-Inf"]=0
IV=((as.numeric(as.numeric(tabla_resumen1[,8])/100)-(as.numeric(tabla_resumen1[,7])/100))*(WOE/100)*100)
iv_ind=sum(IV[IV!="Inf"])
ks_ind=max(ks)
gini=c((acu_malo[1]/100)*(acu_bueno[1]/100),rep(0,nrow(tabla)-1))
  for(i in 2:nrow(tabla))
   { gini[i] = ((acu_malo[i]/100+acu_malo[i-1]/100)*(acu_bueno[i]/100-acu_bueno[i-1]/100))
    }
gini_ind=abs(round((1-sum(gini))*100,4))
gini=round(gini*100,4)
tabla_resumen=cbind(tabla_resumen1,acu_tot,acu_malo,acu_bueno,ks,gini,WOE,IV)
colnames(tabla_resumen)=c("Categorias","Bueno","Malo","Total","PD","%Tot","%Malo","%Bueno","%Acum_Total","%Acum_Malo","Acum_Bueno","Ks","Gini","WOE","IV")
return(list("Tabla_Estadisticos" = tabla_resumen,"GINI_IND"=gini_ind,"KS_IND"=ks_ind,"IV_IND"=iv_ind))
}


Calculo_Stat_Gen_Cuali=function(base,malo,variables,wb,nombrehojacuali,tipo)
{
lista=variables$NOMBRE
m=4
for (i in lista)
{
codigo=variables[variables$NOMBRE==i,]$CODIGO
variable=i
indicadores=data.frame(KS=numeric(),GINI=numeric(),IV=numeric())
	estadisticos=calculo_stat(base[,i],base[,malo],tipo) 
	dist=nrow(estadisticos$Tabla_Estadisticos)
	summary=data.frame(KS = estadisticos$KS_IND, GINI=estadisticos$GINI_IND, IV = estadisticos$IV_IND)
      indicadores=rbind(indicadores,summary)
      createName(wb, name = "Codigo", formula = paste(nombrehojacuali,"!$A$",m,sep=""),overwrite=T)
      writeNamedRegion(wb,codigo, name = "Codigo")
      createName(wb, name = "NombreVar", formula = paste(nombrehojacuali,"!$B$",m,sep=""),overwrite=T)
      writeNamedRegion(wb,variable, name = "NombreVar")
      createName(wb, name = "Tabla_Resumen", formula = paste(nombrehojacuali,"!$B$",m+2,sep=""),overwrite=T)
      writeNamedRegion(wb,estadisticos$Tabla_Estadisticos, name = "Tabla_Resumen")
      
      createName(wb, name = "indicadores", formula = paste(nombrehojacuali,"!$Q$",m+5+dist,sep=""),overwrite=T)
      writeNamedRegion(wb,indicadores, name = "indicadores")
     saveWorkbook(wb)

       createName(wb, name = "Grafico", formula = paste(nombrehojacuali,"!$V$",m+2,sep=""),overwrite=T)
      png(filename = paste(i,".png",sep=""), width = 400, height = 300)
      devAskNewPage(ask = FALSE)
      plot(estadisticos$Tabla_Estadisticos[,5],type="b",xlab=i,ylab="PD",main=paste("PD por ",i,sep=""))
       dev.off()
       addImage(wb, filename = paste(i,".png",sep="") , name = "Grafico",originalSize = TRUE)
      saveWorkbook(wb)
     if(dist>18)
     {m=m+dist+8}
      else
     {m=m+16+5}
     }         
 }

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

###### Calculo de WOE ######


Calculo_WOE=function(base,malo,tipo)
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
por_tot=tablat/sum(tablat)
por_malo=tablam/sum(tablam)
por_bueno=tablab/sum(tablab)
tabla_resumen1=cbind(categorias,tablab,tablam,tablat,pd,por_tot,por_malo,por_bueno)
if(tipo=="CUALITATIVO")
{tabla_resumen1=tabla_resumen1[order(pd),]}
WOE=log((as.numeric(tabla_resumen1[,8]))/(as.numeric(tabla_resumen1[,7])))
WOE[WOE=="Inf"]=0
WOE[WOE=="-Inf"]=0
##Tabla_WOE = cbind(categorias,WOE)
##colnames(Tabla_WOE)=c("Categorias","WOE")
##Tabla_WOE = as.data.frame(Tabla_WOE)
return(list("Categorias"=categorias,"WOE" = WOE))
}

########## Reemplazo de WOE



Reem_WOE_2 = function(base_cal,base_reem,malo,variables,tipo)
{
lista = variables$NOMBRE
for (i in lista)
{
baser=rep(0, nrow(base_reem))
WOE_C = Calculo_WOE(base_cal[,i],base_cal[,malo],tipo=tipo)
Categorias=WOE_C$Categorias
WOE=WOE_C$WOE
nc=length(Categorias)
 for(j in 1:nc)
     { 
     baser[base_reem[,i] == Categorias[j]] = WOE[j]
     }
base_reem[,paste(i,"WOE",sep="_")]=baser
}
return(base_reem)
}

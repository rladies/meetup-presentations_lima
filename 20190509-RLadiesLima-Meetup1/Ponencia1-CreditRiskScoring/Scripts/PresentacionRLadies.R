##-------------------------------------------------------------------------##
##------------------------- Meetup - RLadies ------------------------------##
##-------------------------------------------------------------------------##

##Identificación de Data Train y Data Test
##Llamar a la Base
load("D:/RLadies/Bases/1.Base_Modelo_Par.RData")
View(head(Base_Modelo))

##Validación de PD

round(table(Base_Modelo$FLG_MALO)*100/nrow(Base_Modelo))


library(caTools)

muestra=sample.split(Base_Modelo$FLG_MALO ,SplitRatio=0.75)
data.train=subset(Base_Modelo,muestra==TRUE)
data.test=subset(Base_Modelo,muestra==FALSE)
##Validación
round(nrow(data.train)*100/nrow(Base_Modelo))
round(nrow(data.test)*100/nrow(Base_Modelo))
##Validación de PD

round(table(data.train$FLG_MALO)*100/nrow(data.train))
round(table(data.test$FLG_MALO)*100/nrow(data.test))




######Análisis Univariado ########################

load("D:/RLadies/Bases/2.Base_Modelo_Fin.RData")

data.train = subset(Base_Modelo, TIPO_BASE ==1)
data.test = subset(Base_Modelo, TIPO_BASE == 2)
colnames(data.train)
###1. Cuantitativo

variables_cuanti = read.delim(file = "clipboard")
variables_cuali = read.delim(file = "clipboard")
variables_biv = read.delim(file = "clipboard")


#--- Cuantitativo

LIBRO_DES  <- loadWorkbook(file="D:/RLadies/Reportes Excel/1.Variables_Continuas.xlsx", create = TRUE)
##Crear las hojas en el libro cargado
createSheet(LIBRO_DES , name = "Estadisticas")
createSheet(LIBRO_DES , name = "Graficos")
saveWorkbook(LIBRO_DES)

Cuanti_Gen2_1(data.train,variables_cuanti,LIBRO_DES ,"Estadisticas","Graficos")
Cuanti_Gen2_2(data.train,variables_cuanti,LIBRO_DES ,"Estadisticas","Graficos")

#--Cualitativo


LIBRO_DES <- loadWorkbook(file="D:/RLadies/Reportes Excel/2.Variables_Categoricas.xlsx", create = TRUE)
##Crear las hojas en el libro cargado
createSheet(LIBRO_DES , name = "VariablesCategoricas")
saveWorkbook(LIBRO_DES)

Cualitativo1(data.train,variables_cuali,LIBRO_DES,"VariablesCategoricas")

#---Bivariados



LIBRO_DES <- loadWorkbook(file="D:/RLadies/Reportes Excel/3.Variables_Bivariadas.xlsx", create = TRUE)
##Crear las hojas en el libro cargado
createSheet(LIBRO_DES, name = "variables_biv")
saveWorkbook(LIBRO_DES)

cuanti=Calculo_Stat_Gen_Cuali(data.train ,"FLG_MALO",variables_biv,LIBRO_DES,"variables_biv",tipo="Discretas")

#----Reemplazo de WOE

Base_Modelo2 = Reem_WOE_2(data.train,Base_Modelo,"FLG_MALO",variables_biv,tipo="Discretas")

dim(Base_Modelo)
dim(Base_Modelo2)
table(Base_Modelo2$EDAD_CAT_NUM,Base_Modelo2$EDAD_CAT_NUM_WOE)

#---Modelo

Modelo1=glm(FLG_MALO~V1_WOE + V2_WOE + V3_WOE + V4_WOE + V5_WOE + V6_WOE +
V7_WOE + V8_WOE + V9_WOE + V10_WOE +
V11_WOE, data = data.train,family=binomial(link=logit))

summary(Modelo1)

#-----Validación de Modelo

install.packages("ROCR")
library(ROCR)

Validacion= function(modelo,data,malo)
{
  MI_score <- predict(modelo,type='response',data)
  MI_pred <- prediction(MI_score, data[,malo])
  MI_perf <- performance(MI_pred,"tpr","fpr")
  MI_KS <- round(max(attr(MI_perf,'y.values')[[1]]-attr(MI_perf,'x.values')[[1]])*100, 0)
  MI_AUROC <- round(performance(MI_pred, measure = "auc")@y.values[[1]]*100, 0)
  MI_Gini <- (2*MI_AUROC - 100)
indicadores=cbind(MI_AUROC,MI_KS,MI_Gini)
return(indicadores)
}

Val.train=Validacion(Modelo1,data.train,"FLG_MALO")
Val.test=Validacion(Modelo1,data.test,"FLG_MALO")
Val.mod=Validacion(Modelo1,Base_Modelo,"FLG_MALO")



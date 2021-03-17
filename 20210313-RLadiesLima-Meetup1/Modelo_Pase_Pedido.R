################################################################################################################################################
#####################################  Modelo Nuevas ###########################################################################################
################################################################################################################################################

#Configurando carpeta
setwd("D:/Karin/Modelo Fuga Consultoras/Modelo")
getwd()

#limpieza
rm(list = ls())

#Requirements: Iniciacion de librerias
library(data.table)
library(xgboost)
library(RODBC)
library(stringr)

#Cargando Input
canal<-odbcConnect("XXXXXXXX_SQL", uid="XXXXX", pwd="XXXXXXXX")
ScripPerf<-sprintf("select *from KR_MDL_MatrizNPP_Input_MOD_3 where Consultora_Nueva=1")
print(ScripPerf)
data_mat_camp<-sqlQuery(canal,ScripPerf)
data_mat<-data_mat_camp
str(data_mat)

#Limpieza base
remove<-c(grep("id", colnames(data_mat)),grep("AnioCampana", colnames(data_mat)))
data_mat_M_new <- data_mat[-remove]

library(caret)
library(lattice)
library(ggplot2)

#% Target
Tasa_Target = sum(data_mat_M_new$Target)/nrow(data_mat_M_new)
Tasa_Target
#0.3391027

#ajustes
library(data.table)
dataset1 <- as.data.table(data_mat_M_new)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dataset <- as.data.table(lapply(dataset1, normalize))

#Definiendo Training y Test
crsnobs <- nrow(dataset); 
print(paste("Número TOTAL de observaciones ", crsnobs))
crssample <- crstrain <- sample(nrow(dataset), 0.8*crsnobs) 
print(paste("Número de observaciones ENTRENAMIENTO ", length(crssample)))
crsvalidate <- NULL
crstest <- setdiff(setdiff(seq_len(nrow(dataset)), crstrain), crsvalidate) 
print(paste("Número de observaciones TEST ", length(crstest)))

#Definiendo Target
X_target <- dataset[crstrain]$Target


#---------------------------------------------------------------------------
#Definiendo Variables TOP
#------------------------------------------------------------------------------

#names(dataset)
X_features<-c("U2C_MIN_RealVtaMNNeto","Rat21","U4C_RAT_VTA_UNI_CV","UC_RAT_AHOR_UNI_CV","U4C_MIN_OportunidadAhorroMN","Pc_Inconstancia_new_6C","U3C_VTA_CV","UC_OportunidadAhorroMN","U6C_AHORR_CV","Rat41","U2C_AHORR_CV","U4C_UVEN_CV","TEND_UVEN_U5C","U4C_DESV_RealUUVendidas","U2C_UVEN_CV")

#Corriendo xgboost
#install.packages("xgboost",dependencies = TRUE)
#install.packages("drat",dependencies = TRUE)
library(xgboost)
xgtrain <- xgb.DMatrix(data = as.matrix(dataset[crstrain][, X_features, with = FALSE]), label = X_target, missing = NA)
xgtest <- xgb.DMatrix(data = as.matrix(dataset[crstest][, X_features, with = FALSE]),  missing = NA)

params <- list()
params$objective <- "binary:logistic"
params$eta <- 0.1
params$max_depth <- 10
params$subsample <- 0.9
params$colsample_bytree <- 0.9
params$min_child_weight <- 2
params$eval_metric <- "logloss" 

model_xgb <- xgb.train(params = params, xgtrain, nrounds = 100)

# importancia de variables
vimp <- xgb.importance(model = model_xgb, feature_names = X_features)
View(vimp)
print(vimp)
vimp

# generamos la matriz de variables importantes
importance_matrix = xgb.importance(X_features, model=model_xgb)
# generamos el grafico de importancia
xgb.plot.importance(importance_matrix)


# Guardando Modelo para Predicción
#------------------------------------------------------------------------------
pred <- predict(model_xgb, xgtest)
xgb.save(model_xgb,'xgb.modelo_churn_new_C1715')
model_xgb<-xgb.load('xgb.modelo_churn_new_C1715')

#size of the prediction vector
#print(length(pred))
#limit display of predictions to the first 10
#print(head(pred))

#Cross Validation
#------------------------------------------------------------------------------
cv.res <- xgb.cv(data = as.matrix(dataset[crstrain][, X_features, with = FALSE]), label = X_target, nfold = 5,
                 nrounds = 5, objective = "binary:logistic")

#[1]	train-error:0.149556+0.000572	test-error:0.150775+0.001987 
#[2]	train-error:0.149711+0.000948	test-error:0.151823+0.001984 
#[3]	train-error:0.142536+0.002898	test-error:0.145516+0.005264 
#[4]	train-error:0.131402+0.003042	test-error:0.133950+0.003934 
#[5]	train-error:0.126435+0.001441	test-error:0.128603+0.001209 

#Estableciendo clases 1,0
#------------------------------------------------------------------------------

pred_clas <- as.numeric(pred > 0.3)
pred_clas_ <- data.frame(pred_clas)
test<-dataset[crstest,Target]
test_<-data.frame(test)

#Matriz de confusión
#------------------------------------------------------------------------------
test_veri<-cbind(test_,pred_clas_)
MC<- table(test_veri) # Matriz de ConfusiÃ³n
MC

#Porcentaje acierto total
Pred_tot<-prop.table(MC)
Pred_tot

#Porcentaje acierto por categoría
Pred_clase<-prop.table(MC, 1)
Pred_clase

#auc
#------------------------------------------------------------------------------
#install.packages("pROC",dependencies = TRUE)
#install.packages("scales",dependencies = TRUE)
library(pROC)
plot.roc(dataset[crstest,Target], pred, print.auc=T, print.auc.y=0.5) 


#Prediccion Total por Deciles
#------------------------------------------------------------------------------

#install.packages('BurStMisc')
#install.packages('dplyr')
#install.packages('stats')
#install.packages('base')
#install.packages('data.table')
library(BurStMisc)
library(dplyr)
library(stats)
library(base)
library(data.table)

decil_Prob<-as.matrix(ntile(desc(pred), 10))
test_veri$decil_Prob<-decil_Prob[,1]
#names(test_veri) 

Dec<- table(test_veri) 
ftable(Dec)

################################################################################################################################################
#----------------------------------------Resultado Nueva Campaña

rm(list = ls())

library(data.table)
library(xgboost)
library(RODBC)
library(stringr)


#Cargando tabla al cierre de data

canal<-odbcConnect("XXXXXXXX_SQL", uid="XXXXX", pwd="XXXXXXX")
ScripPerf<-sprintf("select * from MDL_MatrizNPP_Input_Replica_201715_N")
print(ScripPerf)
data_mat_camp<-sqlQuery(canal,ScripPerf)
data_mat<-data_mat_camp
str(data_mat)


#Limpieza base
dataset1 <- as.data.table(data_mat)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dataset <- as.data.table(lapply(dataset1, normalize))
model_xgb_churn_new<-xgb.load('xgb.modelo_churn_new_C1715')
model_xgb<-xgb.load('xgb.modelo_churn_new_C1715')


X_features_churn_new<-c("U2C_MIN_RealVtaMNNeto","Rat21","U4C_RAT_VTA_UNI_CV","UC_RAT_AHOR_UNI_CV","U4C_MIN_OportunidadAhorroMN","Pc_Inconstancia_new_6C","U3C_VTA_CV","UC_OportunidadAhorroMN","U6C_AHORR_CV","Rat41","U2C_AHORR_CV","U4C_UVEN_CV","TEND_UVEN_U5C","U4C_DESV_RealUUVendidas","U2C_UVEN_CV")

xg_rep_churn_new <- xgb.DMatrix(data = as.matrix(dataset[, X_features_churn_new, with = FALSE]), missing = NA)

pred <- predict(model_xgb_churn_new, xg_rep_churn_new)

Rep <- data.table(PKEbelista = dataset1$PKEBELISTA,AnioCampana = dataset1$ANIOCAMPANA,
                  Prob_NPP = pred)

write.csv(Rep, "Rep_new_c1715.csv", row.names = T)





#1.- Trabajar en proyecto

##Paquetes

install.packages("foreign", dependencies = T)
install.packages("dplyr", dependencies = T)
install.packages("ggplot2", dependencies = T)
install.packages("DMwR", dependencies = T)
install.packages("ROSE", dependencies = T)
install.packages("rpart", dependencies = T)
install.packages("randomForest", dependencies = T)
install.packages("pROC", dependencies = T)
install.packages("caret", dependencies = T)
install.packages("adabag", dependencies = T)
install.packages("tidyr", dependencies = T)
install.packages("fastDummies", dependencies = T)
install.packages("reshape", dependencies = T)

## Librer�as
library(foreign)
library(dplyr)
library(ggplot2)
#library(mlr)
library(DMwR)
library(ROSE)
library(rpart)
library(randomForest)
library(pROC)
library(caret)
library(adabag)
#library(Rccp)
#library(nnet)
library(tidyr)
library(fastDummies)


######################
#  Lectura de datos  #
######################

# Accidente de Tr�nsito (AT) - Censo Nacional de Comisar�as 2017
# Tener presente que los datos que figuran en el INEI son una muestra.
# El censo fue de las comisar�as, mas no de los accidentes.

 #abrimos el paquete foreign

data_at_0<- read.spss("A_T_Capitulo 100.sav",use.value.labels=TRUE, max.value.labels=TRUE, to.data.frame=TRUE) #abrimos el archivo en SPSS, activamos los argumentos escribiendo "=TRUE".

data_at<-data_at_0
ncol(data_at) #Nro caracter�sticas
nrow(data_at) #Nro registros de accidentes de tr�nsito a nivel nacional
head(data_at,3)
str(data_at)

## Cambiando de nombre a las variables:
#Solo a las variables asociadas al AT

data_at$at_UBIGEO <- data_at$UBIGEO
data_at$nombre_en_libro <- data_at$AT102
data_at$at_dia <- data_at$AT103_D
data_at$at_mes <- data_at$AT103_M
data_at$at_anio <- data_at$AT103_A
data_at$at_hora <- data_at$AT103_HOR
data_at$at_minuto <- data_at$AT103_MIN
data_at$at_tipo_via <- data_at$AT104A
data_at$at_tipo_via_otro <- data_at$AT104A_O
data_at$at_tramo_via <- data_at$AT104B
data_at$at_tramo_via_otro <- data_at$AT104B_O
data_at$at_nombre_via_v1 <- data_at$AT104C_V1
data_at$at_nombre_via_r1 <- data_at$AT104C_R1
data_at$at_nombre_via_v2 <- data_at$AT104C_V2
data_at$at_nombre_via_r2 <- data_at$AT104C_R2
data_at$at_departamento <- data_at$AT104D_DPTO
data_at$at_provincia <- data_at$AT104D_PROV
data_at$at_distrito <- data_at$AT104D_DIST
data_at$at_tipo_de_accidente <- data_at$AT105
data_at$at_tipo_de_accidente_otro <- data_at$AT105_O
data_at$at_veh_mayor_involucrado_automovil <- data_at$AT106_1
data_at$at_veh_mayor_involucrado_automovil_cant <- data_at$AT106_1_CANT
data_at$at_veh_mayor_involucrado_stationwagon <- data_at$AT106_2
data_at$at_veh_mayor_involucrado_stationwagon_cant <- data_at$AT106_2_CANT
data_at$at_veh_mayor_involucrado_camionetapickup <- data_at$AT106_3
data_at$at_veh_mayor_involucrado_camionetapickup_cant <- data_at$AT106_3_CANT
data_at$at_veh_mayor_involucrado_camionetarural <- data_at$AT106_4
data_at$at_veh_mayor_involucrado_camionetarural_cant <- data_at$AT106_4_CANT
data_at$at_veh_mayor_involucrado_furgoneta <- data_at$AT106_5
data_at$at_veh_mayor_involucrado_furgoneta_cant <- data_at$AT106_5_CANT
data_at$at_veh_mayor_involucrado_omnibusurbano <- data_at$AT106_6
data_at$at_veh_mayor_involucrado_omnibusurbano_cant <- data_at$AT106_6_CANT
data_at$at_veh_mayor_involucrado_omnibusinterprov <- data_at$AT106_7
data_at$at_veh_mayor_involucrado_omnibusinterprov_cant <- data_at$AT106_7_CANT
data_at$at_veh_mayor_involucrado_camion <- data_at$AT106_8
data_at$at_veh_mayor_involucrado_camion_cant <- data_at$AT106_8_CANT
data_at$at_veh_mayor_involucrado_remolcador <- data_at$AT106_9
data_at$at_veh_mayor_involucrado_remolcador_cant <- data_at$AT106_9_CANT
data_at$at_veh_mayor_involucrado_trailer <- data_at$AT106_10
data_at$at_veh_mayor_involucrado_trailer_cant <- data_at$AT106_10_CANT
data_at$at_veh_mayor_involucrado_noidentificado <- data_at$AT106_11
data_at$at_veh_mayor_involucrado_noidentificado_cant <- data_at$AT106_11_CANT
data_at$at_veh_mayor_involucrado_otro <- data_at$AT106_12
data_at$at_veh_mayor_involucrado_otro_nombre <- data_at$AT106_12_O
data_at$at_veh_mayor_involucrado_otro_cant <- data_at$AT106_12_CANT
data_at$at_veh_menor_involucrado_motolineal <- data_at$AT106_13
data_at$at_veh_menor_involucrado_motolineal_cant <- data_at$AT106_13_CANT
data_at$at_veh_menor_involucrado_mototaxi <- data_at$AT106_14
data_at$at_veh_menor_involucrado_mototaxi_cant <- data_at$AT106_14_CANT
data_at$at_veh_menor_involucrado_triciclo <- data_at$AT106_15
data_at$at_veh_menor_involucrado_triciclo_cant <- data_at$AT106_15_CANT
data_at$at_veh_menor_involucrado_bicicleta <- data_at$AT106_16
data_at$at_veh_menor_involucrado_bicicleta_cant <- data_at$AT106_16_CANT
data_at$at_veh_menor_involucrado_noidentificado <- data_at$AT106_17
data_at$at_veh_menor_involucrado_noidentificado_cant <- data_at$AT106_17_CANT
data_at$at_veh_menor_involucrado_otro <- data_at$AT106_18
data_at$at_veh_menor_involucrado_otro_nombre <- data_at$AT106_18_O
data_at$at_veh_menor_involucrado_otro_cant <- data_at$AT106_18_CANT
data_at$at_tipo_transporte_publico <- data_at$AT107_1
data_at$at_tipo_transporte_particular <- data_at$AT107_2
data_at$at_tipo_transporte_noidentificado <- data_at$AT107_3
data_at$at_consecuencia <- data_at$AT108
data_at$at_num_fallecidos <- data_at$AT108_1
data_at$at_num_heridos <- data_at$AT108_2
data_at$at_num_ilesos <- data_at$AT108_3
data_at$at_factor_excesovelocidad <- data_at$AT109_1
data_at$at_factor_desobediencia_a_senal_de_transito <- data_at$AT109_2
data_at$at_factor_faltailuminacion_en_la_via <- data_at$AT109_3
data_at$at_factor_excesodecarga <- data_at$AT109_4
data_at$at_factor_ebriedad_conductor <- data_at$AT109_5
data_at$at_factor_invasion_carril <- data_at$AT109_6
data_at$at_factor_fallamecanica <- data_at$AT109_7
data_at$at_factor_via_en_mal_estado <- data_at$AT109_8
data_at$at_factor_imprudenciadel_peaton <- data_at$AT109_9
data_at$at_factor_ebriedad_peaton <- data_at$AT109_10
data_at$at_factor_climatico <- data_at$AT109_11
data_at$at_factor_senalizacion_defectuosa <- data_at$AT109_12
data_at$at_factor_casanciodel_conductor <- data_at$AT109_13
data_at$at_factor_uso_de_dispositivoselectronicos <- data_at$AT109_14
data_at$at_factor_uso_de_impericia <- data_at$AT109_15
data_at$at_factor_deslizamiento_de_lodopiedras <- data_at$AT109_16
data_at$at_factor_desobedencia_a_la_policia <- data_at$AT109_17
data_at$at_factor_no_identifica <- data_at$AT109_18
data_at$at_factor_otro <- data_at$AT109_19
data_at$at_factor_otro_nombra <- data_at$AT109_19_O
data_at$factor_expansion_2016 <- data_at$Factor_2016
data_at$factor_expansion_2017 <- data_at$Factor_2017

## Seleccionando variables a trabajar
ncol(data_at)
head(data_at[,100:ncol(data_at)],1)

data_at_1 <- data_at[,100:ncol(data_at)]


# Definici�n de variable dependiente

data_at_1$at_fatal<-ifelse(data_at_1$at_consecuencia==1,1,0)
round(prop.table(table(data_at_1$at_fatal))*100,0)

data_at_1$at_fatal<-as.factor(data_at_1$at_fatal)


# Recodificar UBIGEO donde ocurri� el AT

data_at_1$ubigeo_at_nuevo<- paste(data_at_1$at_departamento,
                        data_at_1$at_provincia,
                        data_at_1$at_distrit,sep = "")


data_at_1$ubigeo_at_nuevo<-as.numeric(data_at_1$ubigeo_at_nuevo)

#Lectura de datos de ubigeo 

ubigeo_inei<-read.csv("geodir-ubigeo-inei.csv",sep=";")
head(ubigeo_inei)
ubigeo_inei$ubigeo_at_nuevo=ubigeo_inei$Ubigeo

#Cruce con la tabla Ubigeo
data_at_2<-merge(data_at_1,ubigeo_inei,by="ubigeo_at_nuevo")

tot=arrange(as.data.frame(table(data_at_2$Departamento))
        , desc(Freq))

prop<-function(x){ 
      round(x/sum(x),3)
      }
#Distribuci�n de AT por Departamento
cbind(tot,prop(tot$Freq))

##############################
#    Alcance del estudio     #
##############################

#El an�lisis abarcar� solo a Lima metropolitana ya que concentra cerca al 50% de registros de AT (Aplicando factor de expansi�n)

data_lima<-subset(data_at_2,data_at_2$Departamento=='Lima' & data_at_2$Provincia=='Lima')
nrow(data_lima)

#Distribuci�n del target

porcentaje <- round(prop.table(table(data_lima$at_fatal)) * 100,2)
cbind(freq=table(data_lima$at_fatal), percentage=porcentaje)


##############################
# Tratamiento de variables   #
##############################

as.data.frame(sapply(data_lima,class))

# Eliminando las variables que no afectan al modelo:
# Ejm: at_anio,at_nombre_via_v1
# Criterio: 

data_lima_2<-data_lima[,c(
  "at_dia",
  "at_mes",
  "at_hora",
  "at_tipo_via",
  "at_tramo_via",
  "Distrito",
  "at_distrito",
  "at_tipo_de_accidente",
  "at_veh_mayor_involucrado_automovil",
  "at_veh_mayor_involucrado_stationwagon",
  "at_veh_mayor_involucrado_camionetapickup",
  "at_veh_mayor_involucrado_camionetarural",
  "at_veh_mayor_involucrado_furgoneta",
  "at_veh_mayor_involucrado_omnibusurbano",
  "at_veh_mayor_involucrado_omnibusinterprov",
  "at_veh_mayor_involucrado_camion",
  "at_veh_mayor_involucrado_remolcador",
  "at_veh_mayor_involucrado_trailer",
  "at_veh_mayor_involucrado_noidentificado",
  "at_veh_mayor_involucrado_otro",
  "at_veh_menor_involucrado_motolineal",
  "at_veh_menor_involucrado_mototaxi",
  "at_veh_menor_involucrado_triciclo",
  "at_veh_menor_involucrado_bicicleta",
  "at_veh_menor_involucrado_noidentificado",
  "at_tipo_transporte_publico",
  "at_tipo_transporte_particular",
  "at_tipo_transporte_noidentificado",
  "at_factor_excesovelocidad",
  "at_factor_desobediencia_a_senal_de_transito",
  "at_factor_faltailuminacion_en_la_via",
  "at_factor_excesodecarga",
  "at_factor_ebriedad_conductor",
  "at_factor_invasion_carril",
  "at_factor_fallamecanica",
  "at_factor_via_en_mal_estado",
  "at_factor_imprudenciadel_peaton",
  "at_factor_ebriedad_peaton",
  "at_factor_climatico",
  "at_factor_senalizacion_defectuosa",
  "at_factor_casanciodel_conductor",
  "at_factor_uso_de_dispositivoselectronicos",
  "at_factor_uso_de_impericia",
  "at_factor_deslizamiento_de_lodopiedras",
  "at_factor_desobedencia_a_la_policia",
  "at_factor_no_identifica",
  "at_factor_otro",
  "at_fatal"
)]

dim(data_lima_2)
head(data_lima_2)

# Categorizaci�n de variables

data_lima_2$at_dia_group<-ifelse(as.numeric(data_lima_2$at_dia)<=7,"dia_s1",
                     ifelse(as.numeric(data_lima_2$at_dia)<=14,"dia_s2",
                            ifelse(as.numeric(data_lima_2$at_dia)<=21,"dia_s3",
                                   ifelse(as.numeric(data_lima_2$at_dia)>21,"dia_s4",0)
                                                                     )
                                          ))

levels(data_lima_2$at_mes)<-c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic","99")


data_lima_2$at_hora_group<-ifelse(as.numeric(data_lima_2$at_hora)>=0 & as.numeric(data_lima_2$at_hora)<=5,"madrugada",
                                 ifelse(as.numeric(data_lima_2$at_hora)>5 & as.numeric(data_lima_2$at_hora)<=11,"dia",
                                        ifelse(as.numeric(data_lima_2$at_hora)>11 & as.numeric(data_lima_2$at_hora)<=18,"tarde",
                                               ifelse(as.numeric(data_lima_2$at_hora)>18 & as.numeric(data_lima_2$at_hora)<=23,"noche",0)
                                        )
                                 ))

data_lima_2$at_tipo_via<-as.factor(data_lima_2$at_tipo_via)
data_lima_2$at_tramo_via<-as.factor(data_lima_2$at_tramo_via)
data_lima_2$at_tipo_de_accidente<-as.factor(data_lima_2$at_tipo_de_accidente)


levels(data_lima_2$at_tipo_via)<-c("Autopista","Carretera","Via_Expresa","Avenida","Calle_jr","Trocha","Otro","NoIden")
levels(data_lima_2$at_tramo_via)<-c("Interseccion","Recta","Curva","Rotonda","Bifurcacion","Otro","NoIden")
levels(data_lima_2$at_tipo_de_accidente)<-c(
                        "Atropello","AtropelloYfuga","CaidaPasajero",
                              "Colision","ColisionYfuga","Choque","ChoqueYAtropello",
                                    "ChoqueYFuga","Despiste","DespisteYVolcadura","Volvadura","Otro")

data_lima_2$at_dia_group<-as.factor(data_lima_2$at_dia_group)
data_lima_2$at_hora_group<-as.factor(data_lima_2$at_hora_group)

#Agrupaci�n de distritos de Lima
#Lectura de datos de zonas 

zonas_lima<-read.csv("Zonas_Lima.csv",sep=";")
zonas_lima$at_distrito<-as.factor(zonas_lima$at_distrito)
levels(zonas_lima$at_distrito)[1:9]<-c("01","02","03","04","05","06","07","08","09")

data_lima_3<-merge(data_lima_2,zonas_lima,by="at_distrito")
data_lima_3$at_fatal
head(data_lima_3)

# Generando variables dummies


data_lima_4<-dummy_cols(data_lima_3, c("at_tramo_via","at_tipo_via","at_tipo_de_accidente","at_dia_group",
                                       "at_mes","at_hora_group","Zona"))

dim(data_lima_4)


as.data.frame(sapply(data_lima_4,class))


data_lima_5<-data_lima_4[,c(
  "at_veh_mayor_involucrado_automovil",
  "at_veh_mayor_involucrado_stationwagon",
  "at_veh_mayor_involucrado_camionetapickup",
  "at_veh_mayor_involucrado_camionetarural",
  "at_veh_mayor_involucrado_furgoneta",
  "at_veh_mayor_involucrado_omnibusurbano",
  "at_veh_mayor_involucrado_omnibusinterprov",
  "at_veh_mayor_involucrado_camion",
  "at_veh_mayor_involucrado_remolcador",
  "at_veh_mayor_involucrado_trailer",
  "at_veh_mayor_involucrado_noidentificado",
  "at_veh_mayor_involucrado_otro",
  "at_veh_menor_involucrado_motolineal",
  "at_veh_menor_involucrado_mototaxi",
  "at_veh_menor_involucrado_triciclo",
  "at_veh_menor_involucrado_bicicleta",
  "at_veh_menor_involucrado_noidentificado",
  "at_tipo_transporte_publico",
  "at_tipo_transporte_particular",
  "at_tipo_transporte_noidentificado",
  "at_factor_excesovelocidad",
  "at_factor_desobediencia_a_senal_de_transito",
  "at_factor_faltailuminacion_en_la_via",
  "at_factor_excesodecarga",
  "at_factor_ebriedad_conductor",
  "at_factor_invasion_carril",
  "at_factor_fallamecanica",
  "at_factor_via_en_mal_estado",
  "at_factor_imprudenciadel_peaton",
  "at_factor_ebriedad_peaton",
  "at_factor_climatico",
  "at_factor_senalizacion_defectuosa",
  "at_factor_casanciodel_conductor",
  "at_factor_uso_de_dispositivoselectronicos",
  "at_factor_uso_de_impericia",
  "at_factor_deslizamiento_de_lodopiedras",
  "at_factor_desobedencia_a_la_policia",
  "at_factor_no_identifica",
  "at_factor_otro",
  "at_tramo_via_Interseccion",
  "at_tramo_via_Recta",
  "at_tramo_via_Curva",
  "at_tramo_via_Rotonda",
  "at_tramo_via_Bifurcacion",
  "at_tramo_via_Otro",
  "at_tipo_via_Autopista",
  "at_tipo_via_Carretera",
  "at_tipo_via_Via_Expresa",
  "at_tipo_via_Avenida",
  "at_tipo_via_Calle_jr",
  "at_tipo_via_Trocha",
  "at_tipo_via_Otro",
  "at_tipo_via_NoIden",
  "at_tipo_de_accidente_Atropello",
  "at_tipo_de_accidente_AtropelloYfuga",
  "at_tipo_de_accidente_CaidaPasajero",
  "at_tipo_de_accidente_Colision",
  "at_tipo_de_accidente_ColisionYfuga",
  "at_tipo_de_accidente_Choque",
  "at_tipo_de_accidente_ChoqueYAtropello",
  "at_tipo_de_accidente_ChoqueYFuga",
  "at_tipo_de_accidente_Despiste",
  "at_tipo_de_accidente_DespisteYVolcadura",
  "at_tipo_de_accidente_Volvadura",
  "at_tipo_de_accidente_Otro",
  "at_dia_group_dia_s1",
  "at_dia_group_dia_s2",
  "at_dia_group_dia_s3",
  "at_dia_group_dia_s4",
  "at_mes_Ene",
  "at_mes_Feb",
  "at_mes_Mar",
  "at_mes_Abr",
  "at_mes_May",
  "at_mes_Jun",
  "at_mes_Jul",
  "at_mes_Ago",
  "at_mes_Set",
  "at_mes_Oct",
  "at_mes_Nov",
  "at_mes_Dic",
  "at_hora_group_dia",
  "at_hora_group_madrugada",
  "at_hora_group_noche",
  "at_hora_group_tarde",
  "Zona_Lima_Centro",
  "Zona_Lima_este",
  "Zona_Lima_moderna",
  "Zona_Lima_norte",
  "Zona_Lima_sur",
  "at_fatal"
)]

#Convirtiendo a tipo de variable factor:

data_lima_5$at_veh_mayor_involucrado_automovil<-as.factor(data_lima_5$at_veh_mayor_involucrado_automovil)
data_lima_5$at_veh_mayor_involucrado_stationwagon<-as.factor(data_lima_5$at_veh_mayor_involucrado_stationwagon)
data_lima_5$at_veh_mayor_involucrado_camionetapickup<-as.factor(data_lima_5$at_veh_mayor_involucrado_camionetapickup)
data_lima_5$at_veh_mayor_involucrado_camionetarural<-as.factor(data_lima_5$at_veh_mayor_involucrado_camionetarural)
data_lima_5$at_veh_mayor_involucrado_furgoneta<-as.factor(data_lima_5$at_veh_mayor_involucrado_furgoneta)
data_lima_5$at_veh_mayor_involucrado_omnibusurbano<-as.factor(data_lima_5$at_veh_mayor_involucrado_omnibusurbano)
data_lima_5$at_veh_mayor_involucrado_omnibusinterprov<-as.factor(data_lima_5$at_veh_mayor_involucrado_omnibusinterprov)
data_lima_5$at_veh_mayor_involucrado_camion<-as.factor(data_lima_5$at_veh_mayor_involucrado_camion)
data_lima_5$at_veh_mayor_involucrado_remolcador<-as.factor(data_lima_5$at_veh_mayor_involucrado_remolcador)
data_lima_5$at_veh_mayor_involucrado_trailer<-as.factor(data_lima_5$at_veh_mayor_involucrado_trailer)
data_lima_5$at_veh_mayor_involucrado_noidentificado<-as.factor(data_lima_5$at_veh_mayor_involucrado_noidentificado)
data_lima_5$at_veh_mayor_involucrado_otro<-as.factor(data_lima_5$at_veh_mayor_involucrado_otro)
data_lima_5$at_veh_menor_involucrado_motolineal<-as.factor(data_lima_5$at_veh_menor_involucrado_motolineal)
data_lima_5$at_veh_menor_involucrado_mototaxi<-as.factor(data_lima_5$at_veh_menor_involucrado_mototaxi)
data_lima_5$at_veh_menor_involucrado_triciclo<-as.factor(data_lima_5$at_veh_menor_involucrado_triciclo)
data_lima_5$at_veh_menor_involucrado_bicicleta<-as.factor(data_lima_5$at_veh_menor_involucrado_bicicleta)
data_lima_5$at_veh_menor_involucrado_noidentificado<-as.factor(data_lima_5$at_veh_menor_involucrado_noidentificado)
data_lima_5$at_tipo_transporte_publico<-as.factor(data_lima_5$at_tipo_transporte_publico)
data_lima_5$at_tipo_transporte_particular<-as.factor(data_lima_5$at_tipo_transporte_particular)
data_lima_5$at_tipo_transporte_noidentificado<-as.factor(data_lima_5$at_tipo_transporte_noidentificado)
data_lima_5$at_factor_excesovelocidad<-as.factor(data_lima_5$at_factor_excesovelocidad)
data_lima_5$at_factor_desobediencia_a_senal_de_transito<-as.factor(data_lima_5$at_factor_desobediencia_a_senal_de_transito)
data_lima_5$at_factor_faltailuminacion_en_la_via<-as.factor(data_lima_5$at_factor_faltailuminacion_en_la_via)
data_lima_5$at_factor_excesodecarga<-as.factor(data_lima_5$at_factor_excesodecarga)
data_lima_5$at_factor_ebriedad_conductor<-as.factor(data_lima_5$at_factor_ebriedad_conductor)
data_lima_5$at_factor_invasion_carril<-as.factor(data_lima_5$at_factor_invasion_carril)
data_lima_5$at_factor_fallamecanica<-as.factor(data_lima_5$at_factor_fallamecanica)
data_lima_5$at_factor_via_en_mal_estado<-as.factor(data_lima_5$at_factor_via_en_mal_estado)
data_lima_5$at_factor_imprudenciadel_peaton<-as.factor(data_lima_5$at_factor_imprudenciadel_peaton)
data_lima_5$at_factor_ebriedad_peaton<-as.factor(data_lima_5$at_factor_ebriedad_peaton)
data_lima_5$at_factor_climatico<-as.factor(data_lima_5$at_factor_climatico)
data_lima_5$at_factor_senalizacion_defectuosa<-as.factor(data_lima_5$at_factor_senalizacion_defectuosa)
data_lima_5$at_factor_casanciodel_conductor<-as.factor(data_lima_5$at_factor_casanciodel_conductor)
data_lima_5$at_factor_uso_de_dispositivoselectronicos<-as.factor(data_lima_5$at_factor_uso_de_dispositivoselectronicos)
data_lima_5$at_factor_uso_de_impericia<-as.factor(data_lima_5$at_factor_uso_de_impericia)
data_lima_5$at_factor_deslizamiento_de_lodopiedras<-as.factor(data_lima_5$at_factor_deslizamiento_de_lodopiedras)
data_lima_5$at_factor_desobedencia_a_la_policia<-as.factor(data_lima_5$at_factor_desobedencia_a_la_policia)
data_lima_5$at_factor_no_identifica<-as.factor(data_lima_5$at_factor_no_identifica)
data_lima_5$at_factor_otro<-as.factor(data_lima_5$at_factor_otro)
data_lima_5$at_tramo_via_Interseccion<-as.factor(data_lima_5$at_tramo_via_Interseccion)
data_lima_5$at_tramo_via_Recta<-as.factor(data_lima_5$at_tramo_via_Recta)
data_lima_5$at_tramo_via_Curva<-as.factor(data_lima_5$at_tramo_via_Curva)
data_lima_5$at_tramo_via_Rotonda<-as.factor(data_lima_5$at_tramo_via_Rotonda)
data_lima_5$at_tramo_via_Bifurcacion<-as.factor(data_lima_5$at_tramo_via_Bifurcacion)
data_lima_5$at_tramo_via_Otro<-as.factor(data_lima_5$at_tramo_via_Otro)
data_lima_5$at_tipo_via_Autopista<-as.factor(data_lima_5$at_tipo_via_Autopista)
data_lima_5$at_tipo_via_Carretera<-as.factor(data_lima_5$at_tipo_via_Carretera)
data_lima_5$at_tipo_via_Via_Expresa<-as.factor(data_lima_5$at_tipo_via_Via_Expresa)
data_lima_5$at_tipo_via_Avenida<-as.factor(data_lima_5$at_tipo_via_Avenida)
data_lima_5$at_tipo_via_Calle_jr<-as.factor(data_lima_5$at_tipo_via_Calle_jr)
data_lima_5$at_tipo_via_Trocha<-as.factor(data_lima_5$at_tipo_via_Trocha)
data_lima_5$at_tipo_via_Otro<-as.factor(data_lima_5$at_tipo_via_Otro)
data_lima_5$at_tipo_via_NoIden<-as.factor(data_lima_5$at_tipo_via_NoIden)
data_lima_5$at_tipo_de_accidente_Atropello<-as.factor(data_lima_5$at_tipo_de_accidente_Atropello)
data_lima_5$at_tipo_de_accidente_AtropelloYfuga<-as.factor(data_lima_5$at_tipo_de_accidente_AtropelloYfuga)
data_lima_5$at_tipo_de_accidente_CaidaPasajero<-as.factor(data_lima_5$at_tipo_de_accidente_CaidaPasajero)
data_lima_5$at_tipo_de_accidente_Colision<-as.factor(data_lima_5$at_tipo_de_accidente_Colision)
data_lima_5$at_tipo_de_accidente_ColisionYfuga<-as.factor(data_lima_5$at_tipo_de_accidente_ColisionYfuga)
data_lima_5$at_tipo_de_accidente_Choque<-as.factor(data_lima_5$at_tipo_de_accidente_Choque)
data_lima_5$at_tipo_de_accidente_ChoqueYAtropello<-as.factor(data_lima_5$at_tipo_de_accidente_ChoqueYAtropello)
data_lima_5$at_tipo_de_accidente_ChoqueYFuga<-as.factor(data_lima_5$at_tipo_de_accidente_ChoqueYFuga)
data_lima_5$at_tipo_de_accidente_Despiste<-as.factor(data_lima_5$at_tipo_de_accidente_Despiste)
data_lima_5$at_tipo_de_accidente_DespisteYVolcadura<-as.factor(data_lima_5$at_tipo_de_accidente_DespisteYVolcadura)
data_lima_5$at_tipo_de_accidente_Volvadura<-as.factor(data_lima_5$at_tipo_de_accidente_Volvadura)
data_lima_5$at_tipo_de_accidente_Otro<-as.factor(data_lima_5$at_tipo_de_accidente_Otro)
data_lima_5$at_dia_group_dia_s1<-as.factor(data_lima_5$at_dia_group_dia_s1)
data_lima_5$at_dia_group_dia_s2<-as.factor(data_lima_5$at_dia_group_dia_s2)
data_lima_5$at_dia_group_dia_s3<-as.factor(data_lima_5$at_dia_group_dia_s3)
data_lima_5$at_dia_group_dia_s4<-as.factor(data_lima_5$at_dia_group_dia_s4)
data_lima_5$at_mes_Ene<-as.factor(data_lima_5$at_mes_Ene)
data_lima_5$at_mes_Feb<-as.factor(data_lima_5$at_mes_Feb)
data_lima_5$at_mes_Mar<-as.factor(data_lima_5$at_mes_Mar)
data_lima_5$at_mes_Abr<-as.factor(data_lima_5$at_mes_Abr)
data_lima_5$at_mes_May<-as.factor(data_lima_5$at_mes_May)
data_lima_5$at_mes_Jun<-as.factor(data_lima_5$at_mes_Jun)
data_lima_5$at_mes_Jul<-as.factor(data_lima_5$at_mes_Jul)
data_lima_5$at_mes_Ago<-as.factor(data_lima_5$at_mes_Ago)
data_lima_5$at_mes_Set<-as.factor(data_lima_5$at_mes_Set)
data_lima_5$at_mes_Oct<-as.factor(data_lima_5$at_mes_Oct)
data_lima_5$at_mes_Nov<-as.factor(data_lima_5$at_mes_Nov)
data_lima_5$at_mes_Dic<-as.factor(data_lima_5$at_mes_Dic)
data_lima_5$at_hora_group_dia<-as.factor(data_lima_5$at_hora_group_dia)
data_lima_5$at_hora_group_madrugada<-as.factor(data_lima_5$at_hora_group_madrugada)
data_lima_5$at_hora_group_noche<-as.factor(data_lima_5$at_hora_group_noche)
data_lima_5$at_hora_group_tarde<-as.factor(data_lima_5$at_hora_group_tarde)
data_lima_5$Zona_Lima_Centro<-as.factor(data_lima_5$Zona_Lima_Centro)
data_lima_5$Zona_Lima_este<-as.factor(data_lima_5$Zona_Lima_este)
data_lima_5$Zona_Lima_moderna<-as.factor(data_lima_5$Zona_Lima_moderna)
data_lima_5$Zona_Lima_norte<-as.factor(data_lima_5$Zona_Lima_norte)
data_lima_5$Zona_Lima_sur<-as.factor(data_lima_5$Zona_Lima_sur)
data_lima_5$at_fatal<-as.factor(data_lima_5$at_fatal)


#####################
###    Modelado   ###
####################

#Distribuci�n del target

porcentaje <- round(prop.table(table(data_lima_5$at_fatal)) * 100,2)
cbind(freq=table(data_lima_5$at_fatal), percentage=porcentaje)

#Partici�n de la muestra: Training & Testing
# 70% Training; 30%: Testing 

indice<-sample(nrow(data_lima_5),(nrow(data_lima_5)*.7))
training<-data_lima_5[indice,]
testing<-data_lima_5[-indice,]

#Distribuci�n: Training
porcentaje_tr <- round(prop.table(table(training$at_fatal)) * 100,2)
cbind(freq=table(training$at_fatal), percentage=porcentaje_tr)

#Distribuci�n: Testing
porcentaje_te <- round(prop.table(table(testing$at_fatal)) * 100,2)
cbind(freq=table(testing$at_fatal), percentage=porcentaje_te)

str(training)
training$at_fatal <- as.factor(training$at_fatal)
testing$at_fatal <- as.factor(testing$at_fatal)

table(data_lima_5$at_fatal)

########################
## Balanceo de datos
########################

#Se considera la data total solo para probar con cual algoritmo de balanceo quedarnos
#over sampling
data_balanced_over <- ovun.sample(at_fatal ~ ., data = data_lima_5, method = "over",N =31398 )$data
table(data_balanced_over$at_fatal)

#under sampling
data_balanced_under <- ovun.sample(at_fatal ~ ., data = data_lima_5, method = "under", N = 284, seed = 1)$data
table(data_balanced_under$at_fatal)

#Both
data_balanced_both <- ovun.sample(at_fatal ~ ., data = data_lima_5, method = "both", p=0.5, N=31398, seed = 1)$data
table(data_balanced_both$at_fatal)

#Datos sint�ticos Rose
data.rose <- ROSE(at_fatal ~ ., data = data_lima_5, seed = 1)$data
table(data.rose$at_fatal)

#Datos sint�ticos Smote
data.smote <- SMOTE(at_fatal ~ ., data_lima_5, perc.over = 2000, perc.under=100)
table(data.smote$at_fatal)

#Escojamos con cual m�todo de balanceo quedarnos

#build decision tree models
tree.rose <- rpart(at_fatal ~ ., data = data.rose)
tree.over <- rpart(at_fatal ~ ., data = data_balanced_over)
tree.under <- rpart(at_fatal ~ ., data = data_balanced_under)
tree.both <- rpart(at_fatal ~ ., data = data_balanced_both)
tree.smote <- rpart(at_fatal ~ ., data = data.smote)

#make predictions on unseen data
pred.tree.rose <- predict(tree.rose, newdata = testing)
pred.tree.over <- predict(tree.over, newdata = testing)
pred.tree.under <- predict(tree.under, newdata = testing)
pred.tree.both <- predict(tree.both, newdata = testing)
pred.tree.smote <- predict(tree.smote, newdata = testing)

#AUC ROSE
roc.curve(testing$at_fatal, pred.tree.rose[,2])

#AUC Oversampling
roc.curve(testing$at_fatal, pred.tree.over[,2])

#AUC Undersampling
roc.curve(testing$at_fatal, pred.tree.under[,2])

#AUC Both
roc.curve(testing$at_fatal, pred.tree.both[,2])

#AUC Smote
roc.curve(testing$at_fatal, pred.tree.smote[,2])


# M�todo de selecci�n N� 1 - podr�a usarse

prueba_chi1=function(data)
{
  k<-length(data)
  pval=rep(0,k-1) #creo el array
  for(i in 1:(k-1))
    pval[i] <- chisq.test(data[,k],data[,i])$p.value
  Variables<-names(data[-k])
  p.value<-pval*100
  print(data.frame(Variables,p.value))
}

prueba_chi1(data_lima_5)

#Se excluyen a las variables que est�n relacionadas directamente con la fatalidad
# Ejemplo: Tipo de accidente choque, atropello, colisi�n y sus derivados
#As� mismo, se exclueyn variables que no dan informaci�n como Tramo de v�a otro,
#Tipo de veh�culo no identificado.

data.rose_1<-data.rose[,-c(88,53,54,55,56,57,58,59,60,61,62,63,64,
                           39,17,20,45,11 )]
names(data.rose_1)


# Codigo para el cross-validation

n <- 10 # N�mero de r�plicas
k <- 5 # K-Fold
m <- nrow(data.rose) # N�mero de registros

v.error <- rep(0,n)
v.sens <- rep(0,n)
v.esp <- rep(0,n)
v.Precision <- rep(0,n)
v.VPN <- rep(0,n)
v.ROC <- rep(0,n)

for (i in 1:n){
  
  errori <- 0
  sensi <- 0
  espi <- 0
  Precisioni <- 0
  VPNi <- 0
  ROCi <- 0
  
  grupos <- createFolds(1:m,k)
  
  for(j in 1:k){
    
    muestra   <- grupos[[j]]
    training <- data.rose_1[-muestra,]
    testing  <- data.rose_1[muestra,]
    
    #modelo    <- boosting(at_fatal~.,data= training,boos=TRUE, mfinal=9,coeflearn='Breiman')
    #predict_m <- predict(modelo,testing[,-73])
    #predict_m <-(predict_m$prob)[,2]
    
    modelo <- randomForest(at_fatal~.,data=training,importance = TRUE, maxnodes=10,mtry=6,ntree=100)
    predict_m <- predict(modelo,testing[,-73],type="prob") # Quitamos la variable de Y
    predict_m <-(as.data.frame(predict_m))[,2]
     
    #modelo <- rpart(at_fatal ~ .,data=training)
    #predict_m <- predict(modelo,testing[,-74])[,2] # Quitamos la variable de Y del testing
   
    MC      <- table(testing[,73],predict_m>0.5) # Matriz de confusi�n
    acierto <- sum(diag(MC))/sum(MC)
    error   <- 1 - acierto
    errori  <- errori+error
    sensi   <- MC[2,2]/sum(MC[2,])+sensi
    espi    <- MC[1,1]/sum(MC[1,])+espi
    Precisioni    <- MC[2,2]/sum(MC[,2])+Precisioni
    #VPNi    <- MC[1,1]/sum(MC[,1])+VPNi
    ROCi    <- auc(testing[,23],as.numeric(predict_m>0.5))
  }
  
  v.error[i] <- errori/k
  v.sens[i] <- sensi/k
  v.esp[i] <- espi/k
  v.Precision[i] <- Precisioni/k
  v.VPN[i] <- VPNi/k
  v.ROC[i] <- 1-ROCi/k
}

resultado <- data.frame(Error=v.error,Sensibilidad=v.sens,Especificidad=v.esp,Precision=v.Precision,AreaROC=v.ROC)
resultado <- mutate(resultado,Gini=(AreaROC-0.5)*2,F1.Score=2*(Sensibilidad*Precision)/(Sensibilidad+Precision))
resultado


#RF
importancia=data.frame(importance(modelo))
library(reshape)
importancia<-sort_df(importancia,vars='MeanDecreaseGini')
varImpPlot(modelo,n.var = 20)
resultado_rf


#Boosting
importancia=data.frame(modelo$importance)
library(reshape)
importancia<-sort_df(importancia,vars='modelo.importance')


#Arbol
as.data.frame(modelo$variable.importance)



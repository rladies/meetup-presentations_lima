################################################################################
## Developed by Leda Basombrio April 2017
## Target variable: TARGET_FGTC_BCP  
## Analysis window: 3 months Twitter Data

###############################################################
### Settings

### Setting working directory

  setwd("C:/Users/Leda/Documents/Leda/Charlas/20180427 Charla UP/r-final")

### Use character strings rather than factors 

  options(stringsAsFactors=F)

### Loading libraries
  
  library(neuralnet)
  library(ROSE)
  library(xgboost)
  library(data.table)
  library(RWeka)
  library(adabag)
  library(UBL)
  library(mlbench)
  library(klaR)
  library(dplyr)
  library(jsonlite)
  library(Matrix)
  library(NLP)
  library(openNLP)
  library(SnowballC)
  library(tm)
  library(performanceEstimation)
  library(wordcloud)
  library(Hmisc)
  library(lubridate)
  library(FSelector)
  library(Information)
  library(cowplot)
  library(rpart)
  library(e1071)
  library(randomForest)
  library(DMwR)
  library(AUC)
  library(qdap)
  
  # install.packages("neuralnet")
  # install.packages("ROSE")
  # install.packages("xgboost")
  # install.packages("data.table")
  # install.packages("RWeka")
  # install.packages("adabag")
  # install.packages("UBL")
  # install.packages("mlbench")
  # install.packages("klaR")
  # install.packages("dplyr")
  # install.packages("jsonlite")
  # install.packages("Matrix")
  # install.packages("NLP")
  # install.packages("openNLP")
  # install.packages("SnowballC")
  # install.packages("tm")
  # install.packages("performanceEstimation")
  # install.packages("wordcloud")
  # install.packages("Hmisc")
  # install.packages("lubridate")
  # install.packages("FSelector")
  # install.packages("Information")
  # install.packages("cowplot")
  # install.packages("rpart")
  # install.packages("e1071")
  # install.packages("randomForest")
  # install.packages("DMwR")
  # install.packages("AUC")
  # install.packages("qdap")

###############################################################
### Loading Twitter dataset and creating term features
  
### Loading Twitter dataset
  
  twitter_analisis = read.csv("tweets.csv",header=T)
  
  View(head(twitter_analisis))
  
### Building Term Frequency from messages
  
  var_3m = term_features(twitter_analisis$MENSAJE_3M,200,"_3m")
  var_1m = term_features(twitter_analisis$MENSAJE_1M,200,"_1m")
  var_u3m = term_features(twitter_analisis$MENSAJE_USER_3M,200,"_u3m")
  var_u1m = term_features(twitter_analisis$MENSAJE_USER_1M,200,"_u1m")
  
  var_term = cbind(twitter_analisis$CODMES,twitter_analisis$USERID,var_3m,var_1m,var_u3m,var_u1m)
  dim(var_term) # 3776   160
  View(head(var_term))
  str(var_term)
  names(var_term)[1] = "CODMES"
  names(var_term)[2] = "USERID"
  
  write.csv(var_term,"var_term_fgtc_3m.csv")
  
###############################################################
### Word Cloud
  
  freq = term_wordcloud(twitter_analisis$MENSAJE_USER_1M,50,"_u1m")
  wordcloud(names(freq), freq, min.freq=50, colors=brewer.pal(6, "Dark2"))

  rm(var_3m,var_1m,var_u3m,var_u1m,twitter_analisis)
  
###############################################################
### Loading Extra Variables

### Loading extra variables
  
  var_extra = read.csv("var_extra.csv",header=T)
  dim(var_extra) # 3776  293
  View(head(var_extra))


###############################################################
### Joining Term and Extra Variables
  
### Joining variables
  
  d_fgtc_3m = left_join(var_extra, var_term, by=c("CODMES","USERID"))
  dim(d_fgtc_3m) #  3776  451
  
  write.csv(d_fgtc_3m,"d_fgtc_3m.csv")
  rm(var_extra)
  rm(var_term)
  
  save.image("rdata.RData")
  load("rdata.RData")

###############################################################
### Preprocessing vars

### List of variables to include
  var_include = names(d_fgtc_3m[,5:ncol(d_fgtc_3m)])

### Applying preprocess function
 # socketConnection("localhost", port = 11161, server = TRUE, blocking = FALSE, open = "a+b", timeout = 10000)
  
  process = preprocess_numvars(data=d_fgtc_3m,  # Name of table
                               target_name="TARGET_FGTC_BCP",  # Name of target variable
                              fields_include=var_include , # Variables list
                              perzero=0.98,  # Max % of 0큦 allowed in a variable
                              outratio=3,  # Ratio Max/P99 to cap outliers of variables at p99
                              corrcut = 0.8, # Max correlation allowed between variables
                              tag = "_fgtc_3m")  # Tag of export csvs
  
### Extracting variable importance and ordered list by importance
  
  var_importance_vf = process[[7]]$var_importance_vf
  View(var_importance_vf)  
  
  lista_top= var_importance_vf[order(-var_importance_vf$IV),c("Variable")]
  length(lista_top) #  209
  
### Extracting clean data set
  
  tabla_clean = process[[9]]$tabla_out
  dim(tabla_clean) # 3776  405
  
### Table with clean variables  

  tabla_vf = tabla_clean[,c("TARGET_FGTC_BCP",lista_top)]
  tabla_vf$TARGET_FGTC_BCP = as.factor(tabla_vf$TARGET_FGTC_BCP)
  
  save.image("rdata.RData")
  load("rdata.RData")
  
###############################################################
### Models Scenarios
  
  load("rdata.RData")
  
### Total variables
  nvars = length(lista_top)
  nvars
  
### Loop  
  
  target_var = "TARGET_FGTC_BCP"
  nrep = 5 # 20

  for(v in seq(5,15,by=5) ) { # seq(5,100,by=5)
    
    for(i in 1:nrep) {
      
      print(paste("NVars:",v,"- Iteration:",i)) 
      
      ## Variables list
      
      lista = lista_top[1:v]
      
      ## Formula
      
      form = as.formula(paste0(target_var,"~."))
      
      ## Random Sample 
      
      tablon = tabla_vf[,c(target_var,lista)]
      n = nrow(tablon)
      sp = sample(1:n,0.3*n)
      train = tablon[-sp,]
      test = tablon[sp,]
      
      ## Doubling 1큦    
      
      target1 = train[train[,target_var]==1,]
      target0 = train[train[,target_var]==0,]
      
      n1 = nrow(target1) 
      n0 = nrow(target0) 
      sp = sample(1:n0,n1*2/0.1*0.9)
      
      train_b = rbind(target1,target1,target0[sp,])
      
      ## Tripling 1큦    
      
      sp = sample(1:n0,n1*3/0.1*0.9)
      
      train_b3 = rbind(target1,target1,target1,target0[sp,])      
      
      ## Smote
      
      train_s <- SmoteClassif(form,train,C.perc= "balance",dist="Euclidean") # HEOM
      
      # ROSE
      
      train_r <- ROSE(form,train, seed = 1)$data
      
      # Modelos for each sample  
      
      true = test[,target_var]
      result_0 = models(form,train,test,true,i,v,"Unbalance")
      result_b = models(form,train_b,test,true,i,v,"Double 1")
      result_b3 = models(form,train_b3,test,true,i,v,"Triple 1")
      result_s = models(form,train_s,test,true,i,v,"Smote")
      result_r = models(form,train_r,test,true,i,v,"Rose")
      
      results=rbind(result_0,result_b,result_b3,result_s,result_r) # result_b,result_b3,
      
      #write.csv(results,paste0("results_i",i,"_v",v,".csv"))
      
      # Appending results
      
      if((i==1)&(v==5)) { 
        results_vf = results
      } else {
        results_vf = rbind(results_vf,results)
      }
      
      #write.csv(results_vf,paste0("results_vf_v",v,".csv"))	
      
    }
    write.csv(results_vf,paste0("results_vf_v",v,".csv"))	
  }
  
  View(results_vf)
  
  gini_prom = data.frame(results_vf) %>% group_by(NVar, Scenario, Sample) %>% 
              summarise(gini = mean(as.numeric(gini)))
  
  View(gini_prom)
  
#######################################################################
### FINAL MODEL
  
  
  ## Random Forest ntree = 400, v = 10, Double 1, 3m  
  
  load("rdata.RData")
  
  ## Total variables
  v = 10
  lista = lista_top[1:v]
  
  ## Target var
  target_var = "TARGET_FGTC_BCP"

  ## Formula
  form = as.formula(paste0(target_var,"~."))
  
  ## Data
  tablon = tabla_vf[,c(target_var,lista)]
  
  ## Doubling 1큦    
  target1 = tablon[tablon[,target_var]==1,]
  target0 = tablon[tablon[,target_var]==0,]
  
  n1 = nrow(target1) 
  n0 = nrow(target0) 
  sp = sample(1:n0,n1*2/0.1*0.9)
  
  train_b = rbind(target1,target1,target0[sp,])

  
  # RF ntree 400
  scenario = "RF ntree 400"
  #print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
  rf4 = randomForest(form,data=train_b,ntree=400)
  
  pred = predict(rf4,test,type="prob")[,2]
  
  # Importance of variables
  VarImp = importance(rf4)
  View(VarImp)
  varImpPlot(rf4,main="Feature Relevance Scores")
  
  
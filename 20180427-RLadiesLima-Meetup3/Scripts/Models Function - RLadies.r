###############################################################
## Developed by Leda Basombrio April 2017
## Models Function (parameters are not optimal)

models = function(form,train,test,true,i,v,s) {

	##### Performance metrics function
	
		perf_metrics = function(true,pred,cut=NA) {  
		  # 31 metrics
		  
		  #require(AUC, quietly =T)
		  #require(dplyr, quietly =T)

		  ## Converting factor to binary
		  true2 = as.numeric(as.character((true)))
		  
		  ## AUC and Gini
		  auc = auc(roc(pred,true))
		  gini = 2*auc-1
		  if(is.na(cut)) cut = mean(pred)
		  pred_cl = rep(0,length(pred))
		  pred_cl[pred>cut] = 1
		  pred_cl = factor(pred_cl,levels=c("0","1"))
		  
		  ## Confusion Matrix Metrics
		  confMatrix = table(true,pred_cl)
		  acc = sum(diag(confMatrix))/sum(confMatrix)
		  rec =  confMatrix[2,2] / sum(confMatrix[2,])
		  prec = confMatrix[2,2] / sum(confMatrix[,2])
		  f =  2*rec*prec/(prec+rec)
		  
		  ## Lifts
		  dta = data.frame(cbind(true2, quant = ntile(pred, 10), pred))
		  truecount_q = aggregate(true2 ~ quant , data = dta, FUN = length)
		  truecount_q = truecount_q[order(truecount_q$quant,decreasing=T),2]
		  truesum_q = aggregate(true2 ~ quant , data = dta, FUN = sum)
		  truesum_q = truesum_q[order(truesum_q$quant,decreasing=T),2]
		  truerate_q = truesum_q/truecount_q
		  truecum_q = truesum_q
		  truecum_q[2] = truecum_q[1] + truecum_q[2]
		  truecum_q[3] = truecum_q[2] + truecum_q[3]
		  truecum_q[4] = truecum_q[3] + truecum_q[4]
		  truecum_q[5] = truecum_q[4] + truecum_q[5]
		  truecum_q[6] = truecum_q[5] + truecum_q[6]
		  truecum_q[7] = truecum_q[6] + truecum_q[7]
		  truecum_q[8] = truecum_q[7] + truecum_q[8]
		  truecum_q[9] = truecum_q[8] + truecum_q[9]
		  truecum_q[10] = truecum_q[9] + truecum_q[10]
		  truesumrate_q = truesum_q/sum(truesum_q)
		  perc_cum = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
		  #perc_cum = c(0.2,0.4,0.6,0.8,1)
		  lift = truecum_q/perc_cum/sum(truesum_q)
		  
		  return(list(auc=auc,gini=gini,lift=lift,truerate_q=truerate_q,truesumrate_q=truesumrate_q,truesum_q=truesum_q,truecount_q=truecount_q,acc=acc,rec=rec,prec=prec,f=f))
		}

	##### Scenarios Models
		
		result = matrix(rep(NA,18*60),ncol=60)
		colnames(result) = c("Iteration","NVar","Sample","Scenario","auc","gini",
		                     "lift1","lift2","lift3","lift4","lift5","lift6","lift7","lift8","lift9","lift10",
		                     "truerate_q1","truerate_q2","truerate_q3","truerate_q4","truerate_q5","truerate_q6","truerate_q7","truerate_q8","truerate_q9","truerate_q10",
		                     "truesumrate_q1","truesumrate_q2","truesumrate_q3","truesumrate_q4","truesumrate_q5","truesumrate_q6","truesumrate_q7","truesumrate_q8","truesumrate_q9","truesumrate_q10",
		                     "truesum_q1","truesum_q2","truesum_q3","truesum_q4","truesum_q5","truesum_q6","truesum_q7","truesum_q8","truesum_q9","truesum_q10",
		                     "truecount_q1","truecount_q2","truecount_q3","truecount_q4","truecount_q5","truecount_q6","truecount_q7","truecount_q8","truecount_q9","truecount_q10",
		                     "acc","rec","prec","f")

	### Logit: 1

		scenario = "Logit"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		logit = glm(form,family=binomial(link="logit"),data=train)
		pred = predict.glm(logit,test,type="response")
		result[1,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))

	### Naive Bayes: 1

		scenario = "Naive Bayes"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		nbayes = naiveBayes(form,data = train)
		pred = predict(nbayes,test, type = "raw")[,2]
		result[2,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
	### Decision Tree (cp=c(0.001,0.01),minsplit=c(6,10)): 4
		
		# Tree cp 0 minsplit 10
		scenario = "Tree cp 0 minsplit 10"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		tree1 = rpart(form,data=train,cp=0, minsplit=10)
		pred = predict(tree1,test)[,2]
		result[3,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
		# Tree cp 0 minsplit 20
		scenario = "Tree cp 0 minsplit 20"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		tree2 = rpart(form,data=train,cp=0, minsplit=20)
		pred = predict(tree2,test)[,2]
		result[4,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
		# Tree cp 0.001 minsplit 10
		scenario = "Tree cp 0.001 minsplit 10"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		tree3 = rpart(form,data=train,cp=0.001, minsplit=10)
		pred = predict(tree3,test)[,2]
		result[5,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
		# Tree cp 0.01 minsplit 10
		scenario = "Tree cp 0.01 minsplit 10"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		tree4 = rpart(form,data=train,cp=0.01, minsplit=10)
		pred = predict(tree4,test)[,2]
		result[6,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		  
	### Random Forest (ntree=c(100,200,300,400)): 4

		# RF ntree 100
		scenario = "RF ntree 100"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		rf1 = randomForest(form,data=train,ntree=100)
		pred = predict(rf1,test,type="prob")[,2]
		result[7,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
		# RF ntree 200
		scenario = "RF ntree 200"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		rf2 = randomForest(form,data=train,ntree=200)
		pred = predict(rf2,test,type="prob")[,2]
		result[8,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
		# RF ntree 300
		scenario = "RF ntree 300"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		rf3 = randomForest(form,data=train,ntree=300)
		pred = predict(rf3,test,type="prob")[,2]
		result[9,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
		# RF ntree 400
		scenario = "RF ntree 400"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		rf4 = randomForest(form,data=train,ntree=400)
		pred = predict(rf4,test,type="prob")[,2]
		result[10,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
	  
	 ### AdaBoostM1 (ntree=c(100,200,300,400)): 4

		# AdaBoost ntree 100
		scenario = "AdaBoost ntree 100"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		ada1 = AdaBoostM1(form,train,control=Weka_control(I=100))
		pred = predict(ada1,test,type='probability')[,2]
		result[11,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))

		# AdaBoost ntree 200
		scenario = "AdaBoost ntree 200"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		ada2 = AdaBoostM1(form,train,control=Weka_control(I=200))
		pred = predict(ada2,test,type='probability')[,2]
		result[12,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))

		# AdaBoost ntree 300
		scenario = "AdaBoost ntree 300"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		ada3 = AdaBoostM1(form,train,control=Weka_control(I=300))
		pred = predict(ada3,test,type='probability')[,2]
		result[13,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
		# AdaBoost ntree 400
		scenario = "AdaBoost ntree 400"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		ada4 = AdaBoostM1(form,train,control=Weka_control(I=400))
		pred = predict(ada4,test,type='probability')[,2]
		result[14,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))

	 ### SVM (cost=c(1,5), gamma=c(0.01,0.001)): 4

		# SVM cost 1
		scenario = "SVM cost 1"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		svm1 = svm(form,data=train,probability =TRUE,cost=1)
		pred = predict(svm1,test,probability =TRUE)
		pred = attr(pred, "probabilities")[,2]
		result[15,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
		# SVM cost 5
		scenario = "SVM cost 5"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		svm2 = svm(form,data=train,probability =TRUE,cost=5)
		pred = predict(svm2,test,probability =TRUE)
		pred = attr(pred, "probabilities")[,2]
		result[16,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
		# SVM cost 1 gamma 0.001
		scenario = "SVM cost 1 gamma 0.001"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		svm3 = svm(form,data=train,probability =TRUE, cost=1, gamma=0.001)
		pred = predict(svm3,test,probability =TRUE)
		pred = attr(pred, "probabilities")[,2]
		result[17,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))
		
		# SVM cost 5 gamma 0.001
		scenario = "SVM cost 5 gamma 0.001"
		#print(paste("Nvar",v,"- Iteration",i,"-",s,"-",scenario))
		svm4 = svm(form,data=train,probability =TRUE,cost=5, gamma=0.001)
		pred = predict(svm4,test,probability =TRUE)
		pred = attr(pred, "probabilities")[,2]
		result[18,] = c(i,v,s,scenario,unlist(perf_metrics(true,pred)))

	return(result)	
	
}




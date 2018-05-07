##################################################################################
## Developed by Leda Basombrio April 2017
## Pre processing variables function (only numerical without missing values)

preprocess_numvars = function(data, # data frame with target and features
                              target_name, # target variable name
                              fields_include, # list of numerical features that will be cleaned
                              perzero=0.9, # max % of 0's allowed in variables
                              outratio=2.5, # for cleaning outliers, ratio between max value and p99, if ratio is over that number, cap at p99
                              corrcut = 0.8, # highest correlation admited
							  tag="") {  # tag to add to files that will be written in working directory
  
  #require(FSelector,quietly = T)
  #require(Information,quietly = T)
  options(stringsAsFactors=F)
  
  ###############################################################
  ### Univariate Analysis
  
  # Computing descriptive statistics for numerical variables
  
  print("Computing descriptive statistics")
  univ_f = function(x0) {
    obs = length(x0)
    obsna = length(x0[is.na(x0)])
    x = x0[!is.na(x0)]
    prom = mean(x)
    desv = sd(x)
    perc = quantile(x, c(0,0.01,0.025,0.05,0.1,0.25,0.33,0.5,0.67,0.75,0.9,0.95,0.975,0.99,1)) 
    names(perc)[1] = "Min"
    names(perc)[2:14] = paste0("P",names(perc)[2:14])
    names(perc)[15] = "Max"
    range = perc[15]-perc[1]
    maxp99 = perc[15]/perc[14]
    percna = obsna / obs
    perc0 = length(x[x==0])/obs
    return(c(Obs=obs,ObsNa=obsna,Mean=prom,Stdev=desv,perc,Range=range,Max_P99 = maxp99,PercNa = percna,Perc0 = perc0))
  }
  
  
  descriptive_stats = function(data,x_list) {
    
    n = length(x_list)
    matrix = matrix(rep(NA,n*24),ncol=24)
    colnames(matrix) = c("Variable","Obs","ObsNa","Mean","Std.Dev","Min","P01","P025","P05","P10","P25","P33","P50","P67","P75","P90","P95","P975","P99","Max","Range","Max_P99","PercNa","Perc0")
    
    for(i in 1:n) {
      x = as.numeric(data[,x_list[i]])
      matrix[i,1] = x_list[i]
      matrix[i,2:24] = univ_f(x)
    }
    
    return(data.frame(matrix))
  }
  
  desc_table = descriptive_stats(data,fields_include)
  write.csv(desc_table,paste0("desc_table",tag,".csv"))
  
  ### Selection of variables with range >0 and % of 0´s < 0.9 (perzero)
  print("Cleaning variables")
  list_univ = subset(desc_table, (Range>0) & (Perc0<perzero), select = Variable)[[1]]
  tabla_univ = subset(data,select = c(target_name,list_univ))
  
  ### Correction of outliers
  
  if( is.na(outratio) )  {
    
    tabla_out  = tabla_univ
    desc_table_out = NA
    
  } else {
    
    ### Variables that need to have outliers corrected
    list_out = subset(desc_table, (Range>0) & (Perc0<perzero) & (Max_P99>outratio), select = Variable)[[1]]
    
    ### Cleaning outliers (using a cap to do not lose records)
    cap_outlier = function(data,x_list,percentile) {
      n = length(x_list)
      for(i in 1:n) {
        xname = x_list[i]
        x = as.numeric(data[,xname])
        perc = quantile(x,percentile)
        x[x>perc] = perc
        data[,xname] = x
      }
      return(data)
    }
    
    tabla_out = cap_outlier(tabla_univ,list_out,0.99)
    
    desc_table_out = descriptive_stats(tabla_out,names(tabla_out))
    write.csv(desc_table_out,paste0("desc_table_out",tag,".csv"))
    
  }
  
  ###############################################################
  ### Computing Univariate Relationship with target
  
  print("Computing Information Value")
  # Information Value 
  iv = create_infotables(data=tabla_out, y=target_name)
  var_importance = data.frame(iv$Summary)
  var_importance = var_importance[with(var_importance, order(-IV)), ]
  write.csv(var_importance,paste0("var_importance",tag,".csv"))
 
  ### Correlation Matrix
  print("Computing Correlation")
  cor_matrix = cor(tabla_out[,!(names(tabla_out) %in% target_name)])
  write.csv(cor_matrix,paste0("cor_total",tag,".csv"))
  list_var_num2 = names(tabla_out[,!(names(tabla_out) %in% target_name)])
  
  ### Cleaning correlation
  if( is.na(corrcut) ) {
    
    corr_vars = NA
    cor_clean = NA
    var_importance_vf = NA
    list_clean = var_importance$Variable
   
    
  } else {
    
    print("Cleaning Correlation")
    ### Identifying  variables with high correlation
    n_corr = nrow(cor_matrix)
    k = 0
    for(row in 1:n_corr) {
      for(column in 1:n_corr) {
        x = cor_matrix[row,column]
        if(abs(cor_matrix[row,column])>corrcut & row<column ){
          k = k + 1
          if(k==1) {
            rowvar = list_var_num2[row]
            colvar = list_var_num2[column]
            correl = x
            rowiv = var_importance[var_importance$Variable == list_var_num2[row],"IV"]
            coliv = var_importance[var_importance$Variable == list_var_num2[column],"IV"]
            
          }
          if(k>1) {
            rowvar = c(rowvar,list_var_num2[row])
            colvar = c(colvar,list_var_num2[column])
            correl = c(correl,x)
            rowiv = c(rowiv,var_importance[var_importance$Variable == list_var_num2[row],"IV"])
            coliv = c(coliv,var_importance[var_importance$Variable == list_var_num2[column],"IV"])
            
          }     
        }
      }
    }
    
    corr_vars = data.frame(rowvar,colvar,correl,rowiv,coliv,stringsAsFactors=FALSE)
    write.csv(corr_vars,paste0("corr_vars",tag,".csv"))
    
    ### Cleaning correlated variables and keeping the one with most importance
    lista_nocorr = list()
    lista_total = unique(c(corr_vars$rowvar,corr_vars$colvar))
    datacor = corr_vars
    k = 0
    nfila = 10000
    while( nfila > 0) {
      k = k +1
      if(k==1) {
        datacor$best = datacor$rowvar
        datacor$best[datacor$rowiv<datacor$coliv] = datacor$colvar[datacor$rowiv<datacor$coliv]
        lista_best = unique(datacor$best)
        lista_nocorr = lista_total[lista_total %nin% lista_best]
        datacor = datacor[(datacor$rowvar %in% lista_best) & (datacor$colvar %in% lista_best),]
        datacor$best = datacor$rowvar
        datacor$best[datacor$rowiv<datacor$coliv] = datacor$colvar[datacor$rowiv<datacor$coliv]
        lista_total_k = unique(c(datacor$rowvar,datacor$colvar))
        nfila = nrow(datacor)
      } else {
        lista_best = unique(datacor$best)
        lista_nocorr = c(lista_nocorr,lista_total_k[lista_total_k %nin% lista_best])
        datacor = datacor[(datacor$rowvar %in% lista_best) & (datacor$colvar %in% lista_best),]
        datacor$best = datacor$rowvar
        datacor$best[datacor$rowiv<datacor$coliv] = datacor$colvar[datacor$rowiv<datacor$coliv]
        lista_total_k = unique(c(datacor$rowvar,datacor$colvar))
        nfila = nrow(datacor)    
        
      }
    }
    
    list_cleancorr =list_var_num2[list_var_num2 %nin% lista_nocorr]
    cor_clean = cor(tabla_out[,names(tabla_out) %in% c(list_cleancorr)])
    write.csv(cor_clean,paste0("corr_clean",tag,".csv"))
    
    ### Importance of cleaned variables
    var_importance_vf = var_importance[var_importance$Variable %in% list_cleancorr,]
    write.csv(var_importance_vf,paste0("var_importance_vf",tag,".csv"))
    
    list_clean = var_importance_vf$Variable
    
  }
  
  write.csv(tabla_out,paste0("tabla_out",tag,".csv"))
  
  return(list(list(desc_table=desc_table), # 1
              list(desc_table_out=desc_table_out), # 2
              list(var_importance = var_importance), # 3
              list(cor_matrix=cor_matrix), # 4
              list(corr_vars = corr_vars), # 5
              list(cor_clean = cor_clean), # 6
              list(var_importance_vf = var_importance_vf), # 7
              list(list_clean = list_clean), # 8
              list(tabla_out = tabla_out)) # 9
  )
  
}


#setting
iteration<-10
dupe<-10
target_variable<-"target"
modeling_formula<-as.formula(paste(target_variable,"~.",sep=""))

log_transform<-"y"#y->log, n->not log, yn->log-impute, not log predict

#read data
original_missing.df<-read.csv("missing.csv")

# log transform or not ####
if(log_transform=="n"){
  trans<-"raw"
}else{
  #log transform
  data_fit.df<-log10(original_missing.df+0.5)
  if(log_transform=="y"){
    trans<-"log"
  }else if(log_transform=="yn"){
    trans<-"log-raw"
  }
}

#### Miss Forest ####
library(missForest)
fit_imp.ls<-list()
set.seed(1)
for(j in 1:iteration){
  for(i in 1:dupe){
    data_fit_imp.df<-missForest(data_fit.df)$ximp  
    fit_imp.ls[[i]]<-data.frame(data_fit_imp.df)
    if(log_transform=="yn"){
      out_fit_imp.df<-10^fit_imp.ls[[i]]-0.5
    }else{
      out_fit_imp.df<-fit_imp.ls[[i]]
    }
    write.csv(out_fit_imp.df,paste("mF/fit_",trans,"_mF_imp_0",i-1,"_0",j-1,".csv",sep=""),row.names=F)
  }
}

#### DA ####
library(norm2)
set.seed(1)
emResult<-emNorm(data_fit.df,iter.max = 10000)
max1<-emResult$iter*2
fit_imp.ls<-as.list(NULL)
for(j in 1:iteration){
  for(i in 1:dupe){
    mcmcResult<-mcmcNorm(emResult,iter = max1)
    fit_imp.ls[[i]]<-data.frame(impNorm(mcmcResult))
    if(log_transform=="yn"){
      out_fit_imp.df<-10^fit_imp.ls[[i]]-0.5
    }else{
      out_fit_imp.df<-fit_imp.ls[[i]]
    }
    write.csv(out_fit_imp.df,paste("DA/fit_",trans,"_DA_imp_0",i-1,"_0",j-1,".csv",sep=""),row.names=F)
  }
}
##0i_0jの各jについて統合（0<=i<10を統合）

#### MICE ####
library(mice)
for(j in 1:iteration){
  result_mice.ls<-list()
  data_mice.mice<-mice(data_fit.df,seed=j,m=dupe)
  for(i in 1:dupe){
    fit_imp.df<-complete(data_mice.mice,i)
    if(log_transform=="yn"){
      out_fit_imp.df<-10^fit_imp.df-0.5
    }else{
      out_fit_imp.df<-fit_imp.df
    }
    write.csv(out_fit_imp.df,paste("MICE/fit_",trans,"_MICE_imp_0",i-1,"_0",j-1,".csv",sep=""),row.names=F)
  }
}

#### EM ####
library(Amelia)
set.seed(1)
for(j in 1:iteration){
  fit_em.ls<-amelia(data_fit.df,m=dupe)
    for(i in 1:dupe){
    fit_imp.df<-fit_em.ls[[1]][i][[1]]
    if(log_transform=="yn"){
      out_fit_imp.df<-10^fit_imp.df-0.5
    }else{
      out_fit_imp.df<-fit_imp.df
    }
    write.csv(out_fit_imp.df,paste("EM/fit_",trans,"_EM_imp_0",i-1,"_0",j-1,".csv",sep=""),row.names=F)
  }
}
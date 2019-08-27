# setting
library(randomForest)
library(kernlab)
target_variable<-"target"
modeling_formula<-as.formula(paste(target_variable,"~.",sep=""))
iteration<-10
dupe<-10
k_max<-10

#read test(complete) data
test.df<-read.csv("complete.csv")
origin_test.df<-test.df

### make result dataframe. 1st column is actual data
out.df<-matrix(NA,nrow(test.df),(iteration*dupe+1))
out.df[,1]<-test.df[,target_variable]
out_lm.df<-out.df
out_rf.df<-out.df
out_svm_gauss.df<-out.df
out_svm_poly.df<-out.df

#### predict: lm rf svm_gauss svm_poly, impute: MICE, DA, EM, mF ####
for(trans in c("raw","log","log-raw")){
  if(trans=="log"){
    test.df<-log10(test.df+0.5)
  }
  for(imputation in c("MICE","DA","EM","mF")){
    for(i in 0:(iteration-1)){
      for(j in 0:(dupe-1)){
        fit.df<-read.csv(paste(imputation,"/fit_",trans,"_",imputation,"_imp_0",i,"_0",j,".csv",sep=""))
        ## lm
        fit.lm<-lm(modeling_formula,fit.df)
        fit.lm.step<-step(fit.lm)
        fit.lm.step.predict<-predict(fit.lm.step,test.df)
        if(trans=="log"){
          out_lm.df[,(i*10+(j+1)+1)]<-10^fit.lm.step.predict-0.5
        }else{
          out_lm.df[,(i*10+(j+1)+1)]<-fit.lm.step.predict
        }

        ## ramdom forest
        fit.rf<-randomForest(modeling_formula,fit.df)
        fit.rf.predict<-predict(fit.rf,test.df)
        if(trans=="log"){
          out_rf.df[,(i*10+(j+1)+1)]<-10^fit.rf.predict-0.5
        }else{
          out_rf.df[,(i*10+(j+1)+1)]<-fit.rf.predict
        }

        ## SVM:gaussian
        fit.svm.gauss<-ksvm(modeling_formula,fit.df,kernel="rbfdot")
        fit.svm.gauss.predict<-predict(fit.svm.gauss,test.df)
        out_svm_gauss.df[,(i*10+(j+1)+1)]<-fit.svm.gauss.predict
        if(trans=="log"){
          out_svm_gauss.df[,(i*10+(j+1)+1)]<-10^fit.svm.gauss.predict-0.5
        }else{
          out_svm_gauss.df[,(i*10+(j+1)+1)]<-fit.svm.gauss.predict
        }

        ## SVM: polydot
        fit.svm.poly<-ksvm(modeling_formula,fit.df,kernel="polydot")
        fit.svm.poly.predict<-predict(fit.svm.poly,test.df)
        if(trans=="log"){
          out_svm_poly.df[,(i*10+(j+1)+1)]<-10^fit.svm.poly.predict-0.5
        }else{
          out_svm_poly.df[,(i*10+(j+1)+1)]<-fit.svm.poly.predict
        }
      }
    }
    write.csv(out_lm.df,paste("lm_",trans,"_",imputation,"_result.csv",sep=""))
    write.csv(out_rf.df,paste("rf_",trans,"_",imputation,"_result.csv",sep=""))
    write.csv(out_svm_gauss.df,paste("svm_gauss_",trans,"_",imputation,"_result.csv",sep=""))
    write.csv(out_svm_poly.df,paste("svm_poly_",trans,"_",imputation,"_result.csv",sep=""))
  }
}

####
#### predict: lm rf svm_gauss svm_poly impute: CF, k-nn ####
### make result dataframe. 1st column is actual data
out.df<-matrix(NA,nrow(test.df),(k_max+1))
out.df[,1]<-test.df[,target_variable]
out_lm.df<-out.df
out_rf.df<-out.df
out_svm_gauss.df<-out.df
out_svm_poly.df<-out.df
####
for(trans in c("raw","log","log-raw")){
  if(trans=="log"){
    test.df<-log10(test.df+0.5)
  }
  for(imputation in c("CF","knn")){
    for(i in 1:k_max){
        fit.df<-read.csv(paste(imputation,"/fit_",trans,"_",imputation,"_imp_k",i,".csv",sep=""))
        ## lm
        fit.lm<-lm(modeling_formula,fit.df)
        fit.lm.step<-step(fit.lm)
        fit.lm.step.predict<-predict(fit.lm.step,test.df)
        if(trans=="log"){
          out_lm.df[,(i+1)]<-10^fit.lm.step.predict-0.5
        }else{
          out_lm.df[,(i+1)]<-fit.lm.step.predict
        }

        ## random forest
        fit.rf<-randomForest(modeling_formula,fit.df)
        fit.rf.predict<-predict(fit.rf,test.df)
        if(trans=="log"){
          out_rf.df[,(i+1)]<-10^fit.rf.predict-0.5
        }else{
          out_rf.df[,(i+1)]<-fit.rf.predict
        }

        ## SCM: gussian
        fit.svm.gauss<-ksvm(modeling_formula,fit.df,kernel="rbfdot")
        fit.svm.gauss.predict<-predict(fit.svm.gauss,test.df)
        if(trans=="log"){
          out_svm_gauss.df[,(i+1)]<-10^fit.svm.gauss.predict-0.5
        }else{
          out_svm_gauss.df[,(i+1)]<-fit.svm.gauss.predict
        }

        ## SVM: polydot
        fit.svm.poly<-ksvm(modeling_formula,fit.df,kernel="polydot")
        fit.svm.poly.predict<-predict(fit.svm.poly,test.df)
        if(trans=="log"){
          out_svm_poly.df[,(i+1)]<-10^fit.svm.poly.predict-0.5
        }else{
          out_svm_poly.df[,(i+1)]<-fit.svm.poly.predict
        }
      }
    write.csv(out_lm.df,paste("lm_",trans,"_",imputation,"_result.csv",sep=""))
    write.csv(out_rf.df,paste("rf_",trans,"_",imputation,"_result.csv",sep=""))
    write.csv(out_svm_gauss.df,paste("svm_gauss_",trans,"_",imputation,"_result.csv",sep=""))
    write.csv(out_svm_poly.df,paste("svm_poly_",trans,"_",imputation,"_result.csv",sep=""))
  }
}


#### predict: CF k-nn, inpute: MICE, DA, EM, mF ####
library(FNN)
test.df<-origin_test.df[,colnames(origin_test.df)!=target_variable]
out.df<-matrix(NA,nrow(test.df),(iteration*dupe*k_max+1))
out.df[,1]<-origin_test.df[,target_variable]
out_knn.df<-out.df
out_cf.df<-out.df

for(trans in c("raw","log","log-raw")){
  if(trans=="log"){
    test.df<-log10(test.df+0.5)
  }
  for(imputation in c("MICE","DA","EM","mF")){
    for(k in 1:k_max){
      for(i in 0:(iteration-1)){
        for(j in 0:(dupe-1)){
          origin_fit.df<-read.csv(paste(imputation,"/fit_",trans,"_",imputation,"_imp_0",i,"_0",j,".csv",sep=""))
          fit.df<-origin_fit.df[,colnames(origin_fit.df)!=target_variable]
          result.df<-matrix()
          
          #knn: z-score normalization
          scale_fit.df<-scale(fit.df)
          scale_test.df<-scale(test.df,apply(fit.df,2,mean),apply(fit.df,2,sd))
          #calculate similarity
          out.knn<-get.knnx(data=scale_fit.df,query=scale_test.df,k=k,algorithm="kd_tree")
          out.knn.predict<-apply(matrix(origin_fit.df[,target_variable][out.knn$nn.index],nrow(test.df),k),1,mean)
          if(trans=="log"){
            out_knn.df[,(k-1)*100+j+i*10+2]<-10^out.knn.predict-0.5
          }else{
            out_knn.df[,(k-1)*100+j+i*10+2]<-out.knn.predict
          }
          
          #CF: min-max normalization
          normarization<-function(x){return ((x-apply(fit.df,2,min))/(apply(fit.df,2,max)-apply(fit.df,2,min)))}
          norm_fit.df<-t(apply(fit.df,1,normarization))
          norm_test.df<-t(apply(test.df,1,normarization))
          
          #subtract median
          submed<-function(x){return (x-apply(norm_fit.df,2,median))}
          submed_norm_fit.df<-t(apply(norm_fit.df,1,submed))
          submed_norm_test.df<-t(apply(norm_test.df,1,submed))
          ## fewer lines for execution but not to easy to understand
          #sweep_fit.df<-sweep(norm_fit.df,2,apply(norm_fit.df,2,median),FUN="-")

          #Main: Cosine similarity(AdjustedCosineSimilarityWithMedian)
          # sim.mtx: similarity matrix
          sim.mtx<-matrix(0,nrow(norm_test.df),nrow(submed_norm_fit.df))
          for(ti in (1:nrow(norm_test.df))){
            for(fj in (1:nrow(submed_norm_fit.df))){
              sim.mtx[ti,fj]<-sum(submed_norm_test.df[ti,]*submed_norm_fit.df[fj,])/(sqrt(sum(submed_norm_test.df[ti,]^2))*sqrt(sum(submed_norm_fit.df[fj,]^2)))
            }
          }
          for(test_id in 1:nrow(test.df)){
            sortlist<-order(sim.mtx[test_id,],decreasing=T)
            amp<-function(x){return (test.df[test_id,]/x)}
            out.cf.pred<-0
            amp.ls<-""
            if(k==1){
              amp.ls<-test.df[test_id,]/fit.df[sortlist[k],]
              amp.ls<-ifelse(is.na(amp.ls),0,amp.ls)
              out.cf.pred<-sum(origin_fit.df[,target_variable][sortlist[1:k]]*apply(data.frame(amp.ls),1,median)*sim.mtx[test_id,sortlist[1:k]])/sum(sim.mtx[test_id,sortlist[1:k]])
            }else{
              amp.ls<-apply(fit.df[sortlist[1:k],],1,amp)
              amp.ls<-ifelse(is.na(amp.ls),0,amp.ls)
              out.cf.pred<-sum(origin_fit.df[,target_variable][sortlist[1:k]]*apply(data.frame(do.call(rbind,apply(fit.df[sortlist[1:k],],1,amp))),1,median)*sim.mtx[test_id,sortlist[1:k]])/sum(sim.mtx[test_id,sortlist[1:k]])
            }
            if(trans=="log"){
              out_cf.df[test_id,(k-1)*100+j+i*10+2]<-10^out.cf.pred-0.5
            }else{
              out_cf.df[test_id,(k-1)*100+j+i*10+2]<-out.cf.pred
            }
          }
        }
      }
    }
    write.csv(out_knn.df,paste("knn_",trans,"_",imputation,"_result.csv",sep=""))
    write.csv(out_cf.df,paste("cf_",trans,"_",imputation,"_result.csv",sep=""))
  }
}
test.df<-origin_test.df

#### predict: CF k-nn, impute: CF k-nn ####
library(FNN)
test.df<-origin_test.df[,colnames(origin_test.df)!=target_variable]
out.df<-matrix(NA,nrow(test.df),(k_max*k_max+1))
out.df[,1]<-origin_test.df[,target_variable]
out_knn.df<-out.df
out_cf.df<-out.df

for(trans in c("raw","log","log-raw")){
  if(trans=="log"){
    test.df<-log10(test.df+0.5)
  }
  for(imputation in c("CF","knn")){
    for(i in 1:k_max){
      for(k in 1:k_max){
        origin_fit.df<-read.csv(paste(imputation,"/fit_",trans,"_",imputation,"_imp_k",i,".csv",sep=""))
        fit.df<-origin_fit.df[,colnames(origin_fit.df)!=target_variable]
        result.df<-matrix()

        #knn: z-score normalization
        scale_fit.df<-scale(fit.df)
        scale_test.df<-scale(test.df,apply(fit.df,2,mean),apply(fit.df,2,sd))
        
        #knn: calculate similarity
        out.knn<-get.knnx(data=scale_fit.df,query=scale_test.df,k=k,algorithm="kd_tree")
        out.knn.predict<-apply(matrix(origin_fit.df[,target_variable][out.knn$nn.index],160,k),1,mean)
        if(trans=="log"){
          out_knn.df[,(i-1)*10+(k-1)+2]<-10^out.knn.predict-0.5
        }else{
          out_knn.df[,(i-1)*10+(k-1)+2]<-out.knn.predict
        }

        #CF: min-max normalization
        normarization<-function(x){return ((x-apply(fit.df,2,min))/(apply(fit.df,2,max)-apply(fit.df,2,min)))}
        norm_fit.df<-t(apply(fit.df,1,normarization))
        norm_test.df<-t(apply(test.df,1,normarization))
        #subtract median
        submed<-function(x){return (x-apply(norm_fit.df,2,median))}
        submed_norm_fit.df<-t(apply(norm_fit.df,1,submed))
        submed_norm_test.df<-t(apply(norm_test.df,1,submed))
        #sweep_fit.df<-sweep(norm_fit.df,2,apply(norm_fit.df,2,median),FUN="-")
        
        #Main: Cosine similarity(AdjustedCosineSimilarityWithMedian)
        sim.mtx<-matrix(0,nrow(norm_test.df),nrow(submed_norm_fit.df))
        for(ti in (1:nrow(norm_test.df))){
          for(fj in (1:nrow(submed_norm_fit.df))){
            sim.mtx[ti,fj]<-sum(submed_norm_test.df[ti,]*submed_norm_fit.df[fj,])/(sqrt(sum(submed_norm_test.df[ti,]^2))*sqrt(sum(submed_norm_fit.df[fj,]^2)))
          }
        }
        for(test_id in 1:nrow(test.df)){
          sortlist<-order(sim.mtx[test_id,],decreasing=T)
          amp<-function(x){return (test.df[test_id,]/x)}
          out.cf.pred<-0
          amp.ls<-""
          if(k==1){
            amp.ls<-test.df[test_id,]/fit.df[sortlist[k],]
            out.cf.pred<-sum(origin_fit.df[,target_variable][sortlist[1:k]]*apply(data.frame(amp.ls),1,median)*sim.mtx[test_id,sortlist[1:k]])/sum(sim.mtx[test_id,sortlist[1:k]])
          }else{
            amp.ls<-apply(fit.df[sortlist[1:k],],1,amp)
            out.cf.pred<-sum(origin_fit.df[,target_variable][sortlist[1:k]]*apply(data.frame(do.call(rbind,apply(fit.df[sortlist[1:k],],1,amp))),1,median)*sim.mtx[test_id,sortlist[1:k]])/sum(sim.mtx[test_id,sortlist[1:k]])
          }
          if(trans=="log"){
            out_cf.df[test_id,(i-1)*10+(k-1)+2]<-10^out.cf.pred-0.5
          }else{
            out_cf.df[test_id,(i-1)*10+(k-1)+2]<-out.cf.pred
          }
        }
      }
    }
    write.csv(out_knn.df,paste("knn_",trans,"_",imputation,"_result.csv",sep=""))
    write.csv(out_cf.df,paste("cf_",trans,"_",imputation,"_result.csv",sep=""))
  }
}

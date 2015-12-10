
#script1.2
#Data sample
#Grid search in parallel

#Create a data sample
sampling<-function(data,size=1000,seed=1306){
        set.seed(seed)
        smpl<-sample.int(n=nrow(data),size=size,replace=F)
        data_smpl<-data[smpl,]
        return(data_smpl)
}

data_smpl<-sampling(data)

search<-function(data_smpl,k_folds=10,mtry1=floor(sqrt(ncol(data_smpl))),
                 mtry2=1.5*floor(sqrt(ncol(data_smpl))),ntree1=500,ntree2=1000){
        
        #Tuning grid
        tune_grid<-expand.grid(mtry=c(mtry1,mtry2),ntree=c(ntree1,ntree2))
        
        #Parallel backend
        n_cores<-detectCores()
        cl<-makeCluster(n_cores)
        registerDoParallel(cl)
        
        
        #Grid search in parallel
        results<-
                foreach(grid=c(1:nrow(tune_grid)),
                        .packages=c("caret","pROC","randomForest"),
                        .combine=rbind,
                        .multicombine=T)%dopar%{
                                
                                set.seed(1306)#Reproducibility
                                folds<-createFolds(y=data_smpl$response,k=k_folds)
                                ntree<-tune_grid[grid,"ntree"]
                                mtry<-tune_grid[grid,"mtry"]
                                
                                auc_results<-NULL
                                
                                search_results<-c()
                                
                                #k-fold cross-validation
                                for(i in 1:k_folds){
                                        
                                        train_cv<-data_smpl[-folds[[i]],]
                                        test_cv<-data_smpl[folds[[i]],]
                                        true_resp_cv<-test_cv$response
                                        
                                        #Random forest
                                        model_cv<-randomForest(data=train_cv,
                                                               response~.,
                                                               ntree=ntree,
                                                               mtry=mtry)
                                        
                                        #predictions
                                        pred_cv<-predict(model_cv,test_cv,type="prob")[,2]
                                        
                                        #Metrics, AUC
                                        roc_cv<-roc(true_resp_cv,pred_cv)
                                        auc_cv<-as.numeric(auc(roc_cv))
                                        
                                        auc_results<-c(auc_results,auc_cv)
                                        
                                        auc_results
                                        
                                }
                                return(c("auc"=sum(auc_results)/length(folds),
                                                  "mtry"=mtry,
                                                  "ntree"=ntree))
                                
                        }
        
        stopCluster(cl)
        return(results)

}


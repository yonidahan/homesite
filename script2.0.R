
#script2.0 : XGBoost


#Libraries
lapply(c("xgboost","dplyr","caret","ggplot2","doParallel"),require,character.only=T)

#Working Directory
setwd("C:/Users/Sarah/Desktop/Data Science/Projects/homesite")

#Read the data
train<-read.csv("train.csv")
test<-read.csv("test.csv")
ntrain<-nrow(train)


#Pre-processing
pre_process<-function(train,test,log=F,scale=T){
        print("Stack the data",quote=F)
        target_name<-setdiff(names(train),names(test))#Get the predicted variable name
        target<-c(train[,target_name],rep(0,nrow(test)))
        data<-cbind(rbind(train[,!(names(train)%in%target_name)],test),#Stack train and test data
                    target)
        print("Done!",quote=F)
        
        print("Create time variables",quote=F)
        data$Original_Quote_Date<-as.POSIXlt(as.character(data$Original_Quote_Date),
                                             format="%Y-%m-%d")
        data$year<-as.integer(format(data$Original_Quote_Date,"%y"))#Extract year
        data$month<-as.integer(format(data$Original_Quote_Date,"%m"))#Extract month
        data$day<-as.integer(format(data$Original_Quote_Date,"%d"))#Extract day
        data$julian<-as.integer(julian(data$Original_Quote_Date,#Number of days since first observation
                                       origin=min(data$Original_Quote_Date)))
        data<-select(data,-Original_Quote_Date)#Remove date column
        print("Done!",quote=F)
        
        print("Missing values",quote=F)
        sum_nas<-sapply(data[1:ntrain,],#% of missing values for each var
                        FUN=function(x)100*sum(is.na(x))/ntrain)
        sum_nas<-as.data.frame(sum_nas[sum_nas!=0])
        colnames(sum_nas)<-c("% missing")
        print(sum_nas,quote=F)
        print("I choose to remove them",quote=F)
        data<-data[,!(names(data)%in%rownames(sum_nas))]
        print("Done!",quote=F)
        
        print("Constant variables",quote=F)
        const_var<-sapply(data,FUN=function(x)length(unique(x)))#Number of unique values for each var
        const_var<-attr(const_var[const_var==1],"names")#Constant variable names
        print(const_var,quote=F)
        print("We can remove them",quote=F)
        data<-data[,!(names(data)%in%const_var)]
        print("Done!",quote=F)
        
        fact_var_names<-sapply(data,FUN=function(x)is.factor(x))
        fact_var_names<-attr(fact_var_names[fact_var_names==T],"names")
        print("Categorical variables",quote=F)
        print(fact_var_names,quote=F)
        print("Replace them with numeric values",quote=F)
        data[,fact_var_names]<-lapply(data[,fact_var_names],as.integer)
        print("Done!",quote=F)
        
        log_scale<-function(x){
                if(min(x)==0|min(x)<0){#To avoid undefined values
                        x<-log(x+1-min(x))
                        
                }else{
                        x<-log(x)
                }
                return(x)
        }
        
        if(log==T){
                print("Log scaling",quote=F)
                
                data[1:ntrain,!(names(data)%in%c("QuoteNumber","target"))]<-#On train data
                        lapply(data[1:ntrain,!(names(data)%in%c("QuoteNumber","target"))],log_scale)
                
                data[-c(1:ntrain),!(names(data)%in%c("QuoteNumber","target"))]<-#On test data
                        lapply(data[-c(1:ntrain),!(names(data)%in%c("QuoteNumber","target"))],log_scale)
                
                print("Done!",quote=F)
                
                return(data)
        }
        
        if(scale==T){
                print("Feature standardization",quote=F)
                
                data[1:ntrain,!(names(data)%in%c("QuoteNumber","target"))]<-#On train data
                        lapply(data[1:ntrain,!(names(data)%in%c("QuoteNumber","target"))],
                               FUN=function(x)scale(x,center=T,scale=T))
                
                data[-c(1:ntrain),!(names(data)%in%c("QuoteNumber","target"))]<-#On test data
                        lapply(data[-c(1:ntrain),!(names(data)%in%c("QuoteNumber","target"))],
                               FUN=function(x)scale(x,center=T,scale=T))
                
                print("Done!",quote=F)
                
                return(data)
        }
        
        
        data
        
}

data<-pre_process(train,test,scale=T,log=T)

target<-data$target
QuoteNumber<-data$QuoteNumber
data<-select(data,-QuoteNumber,-target)


#Create a data sample
sampling<-function(data,size=1000,seed=1306){
        set.seed(seed)
        smpl<-sample.int(n=ntrain,size=size,replace=F)
        data_smpl<-data[smpl,]
        return(data_smpl)
}

data_smpl<-sampling(data,size=1000,seed=1306)


#XGBoost in parallel

params<-list(etas=c(0.05,0.1,0.15,0.2),
             max_depth=c(4,6,8,10),
             subsample=c(0.5,0.7,0.85,0.95),
             colsample_bytree=c(0.6,0.7,0.9,1),
             nrounds=100
             )

#Chose: parameters to choose for grid search, otherwise defaults are those of XGBoost
#More information: https://github.com/dmlc/xgboost or 
#https://cran.r-project.org/web/packages/xgboost/xgboost.pdf
search<-function(data=data_smpl,k_folds=10,params=params){
        
        
        #Tuning grid
        if(chose)
}























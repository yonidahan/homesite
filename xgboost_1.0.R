
#XGBoost 

lapply(c("xgboost","dplyr","caret","data.table"),require,character.only=T)

setwd("C:/Users/Sarah/Desktop/Data Science/Projects/Homesite")


train<-read.csv("train.csv");test<-read.csv("test.csv")

n<-nrow(train)


#Slight feature engineering
feat_engin<-function(train,test,fact_thresh=20,allnumeric=T){
        
        print("Stack and prepare the data",quote=F)
        response_name<-setdiff(names(train),names(test))
        response<-c(train[,response_name],rep(0,nrow(test)))
        data<-rbind(train[,!c(names(train)%in%response_name)],test)
        
        data$Original_Quote_Date<-as.POSIXlt(as.character(data$Original_Quote_Date),
                                             format="%Y-%m-%d")
        print("Done!",quote=F)
        
        
        print("Remove variables with missing values",quote=F)
        sum_nas<-sapply(data[1:n,],FUN=function(x)sum(is.na(x)))
        print(sum_nas[sum_nas!=0]/n)
        na_var_names<-attr(sum_nas[sum_nas!=0],"names")
        data<-data[,!c(names(data)%in%na_var_names)]#Remove from train and test
        print("Done!",quote=F)
        
        
        print("Remove constant variables",quote=F)
        const_var_names<-sapply(data[1:n,],FUN=function(x)length(unique(x)))
        const_var_names<-attr(const_var_names[const_var_names==1],"names")
        data<-data[,!c(names(data)%in%const_var_names)]#Again, remove from train and test
        print("Done!",quote=F)
        
        
        print("Some factor levels are blanks or whitespaces",quote=F)
        print("We can change them to respectively Bl and Ws",quote=F)
        fact_var_names<-sapply(data,is.factor)
        fact_var_names<-attr(fact_var_names[fact_var_names==T],"names")
        replace_lev<-function(x){
                levels(x)[levels(x)==""]<-"Bl"
                levels(x)[levels(x)==" "]<-"Ws"
                return(x)
        }
        data[,fact_var_names]<-as.data.frame(sapply(data[,fact_var_names],
                                                    FUN=function(x)replace_lev(x)))
        print("Done!",quote=F)
        
        
        print("We can consider some numeric variables as factor ones",quote=F)
        var_to_conv<-sapply(data[,!c(names(data)%in%fact_var_names)],
                            FUN=function(x)length(unique(x)))
        var_to_conv<-attr(var_to_conv[var_to_conv<fact_thresh],"names")
        
        #Firstly, let's isolate the variables to which we can apply this method,
        #in terms of equivalence of the levels in train and test
        uniq_in_train<-sapply(data[1:n,var_to_conv],FUN=function(x)unique(x))
        uniq_in_test<-sapply(data[-c(1:n),var_to_conv],FUN=function(x)unique(x))
        uniq_in_train<-sapply(uniq_in_train,sort)
        uniq_in_test<-sapply(uniq_in_test,sort)
        var_to_conv<-var_to_conv[match(uniq_in_train,uniq_in_test,nomatch=0)!=0]
        
        #We can now encode these variables as factors
        data[,var_to_conv]<-lapply(data[,var_to_conv],as.factor)
        print("Done!",quote=F)
        
        
        print("Some of the initial factor variables don't have same levels in train and test",
              quote=F)
        print("We have to remove them",quote=F)
        uniq_in_train<-sapply(data[1:n,fact_var_names],FUN=function(x)unique(x))
        uniq_in_test<-sapply(data[-c(1:n),fact_var_names],FUN=function(x)unique(x))
        uniq_in_train<-sapply(uniq_in_train,sort)
        uniq_in_test<-sapply(uniq_in_test,sort)
        fact_to_remove<-fact_var_names[match(uniq_in_train,uniq_in_test,nomatch=0)==0]
        data<-data[,!c(names(data)%in%fact_to_remove)]
        print("Done!",quote=F)
        
        
        print("Let's create variables which indicate month and day of week",
              quote=F)
        data$Month<-as.factor(months(data$Original_Quote_Date))
        data$Day<-as.factor(weekdays(data$Original_Quote_Date))
        data<-select(data,-Original_Quote_Date)
        print("Done!",quote=F)
        
        
        
        if(allnumeric==T){
                print("Transform to numeric",quote=F)
                fact_var_names<-sapply(data,is.factor)
                fact_var_names<-attr(fact_var_names[fact_var_names==T],"names")
                data[,fact_var_names]<-as.data.frame(sapply(data[,fact_var_names],
                                                            FUN=function(x)as.numeric(x)))
                #Relevel: from 0
                data[,fact_var_names]<-as.data.frame(sapply(data[,fact_var_names],
                                                            FUN=function(x)x-1))
                data
                        
        }
        
        data$response<-response
        
        data
}


data<-feat_engin(train,test,fact_thresh=20,allnumeric=T)
rm(train,test)
QuoteNumber<-data$QuoteNumber
data<-select(data,-QuoteNumber)



response<-data$response
data<-select(data,-response)

#Train matrix
d_train<-xgb.DMatrix(data=data.matrix(data[1:n,]),label=response[1:n])


#Validation matrix
set.seed(1306)
d_valid<-xgb.DMatrix(data=data.matrix(data[sample(n,10000),]),
                     label=response[sample(n,10000)])


#Cross-validation
params<-list(
        objective="binary:logistic",
        booster="gbtree",
        eval_metric="auc",
        eta=0.01,
        max_depth=10,
        subsample=0.6,
        colsample_bytree=0.7
        )

cross_valid<-xgb.cv(
        params=params,
        data=d_train,
        nrounds=10,
        nfold=4,
        metrics=list("auc"),
        verbose=1,
        early.stop.round=10,
        maximize=T
        )
















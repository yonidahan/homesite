
#script0.2




#Reading
lapply(c("ggplot2","dplyr","caret","doParallel","foreach",
         "pROC","randomForest","ROCR"),require,character.only=T)
setwd("C:/Users/Sarah/Desktop/Data Science/Projects/homesite")

train<-read.csv("train.csv")
test<-read.csv("test.csv")

n<-nrow(train)

#Feature Engineering
feat_engin<-function(data1,data2){
        
        print("Stack and prepare the data",quote=F)
        response_name<-setdiff(names(data1),names(data2))
        response<-c(data1[,response_name],rep(0,dim(data2)[1]))
        data<-rbind(data1[,!(names(data1)%in%response_name)],data2)
        data$Original_Quote_Date<-as.POSIXlt(as.character(data$Original_Quote_Date),
                                             format="%Y-%m-%d")
        print("Done!",quote=F)
        
        
        print("Remove variables with missing values",quote=F)
        sum_nas<-sapply(data[1:n,],FUN=function(x)sum(is.na(x)))
        print(sum_nas[sum_nas!=0]/dim(data[1:n,])[1])
        na_var_names<-attr(sum_nas[sum_nas!=0],"names")
        data<-data[,!(names(data)%in%na_var_names)]
        print("Done!",quote=F)
        
        
        print("Remove constant variables",quote=F)
        const_var_names<-sapply(data,FUN=function(x)length(unique(x)))
        const_var_names<-attr(const_var_names[const_var_names==1],"names")
        data<-data[,!(names(data)%in%const_var_names)]
        print("Done!",quote=F)
        
        
        print("Some factor levels are blanks or whitespaces",quote=F)
        print("We can change them to respectively Bl and Ew",quote=F)
        fact_var_names<-sapply(data,is.factor)
        fact_var_names<-attr(fact_var_names[fact_var_names==T],"names")
        replace_lev<-function(x){
                levels(x)[levels(x)==""]<-"Bl"
                levels(x)[levels(x)==" "]<-"Ws"
                return(x)       
        }
        data[,fact_var_names]<-as.data.frame(sapply(data[,fact_var_names],replace_lev))
        print("Done!",quote=F)
        
        
        print("Convert some integer and numeric variables to factor ones",quote=F)
        var_to_conv<-sapply(data[,!(names(data)%in%c(fact_var_names))]
                            ,FUN=function(x)length(unique(x)))
        var_to_conv<-attr(var_to_conv[var_to_conv<100],"names")
        
        #Firstly, isolate the variables to which we can apply this method,
        #in terms of equivalence of the levels in train and test 
        uniq_in_train<-sapply(data[1:n,var_to_conv],unique)        
        uniq_in_test<-sapply(data[-c(1:n),var_to_conv],unique)
        uniq_in_train<-sapply(uniq_in_train,sort)
        uniq_in_test<-sapply(uniq_in_test,sort)
        var_to_conv<-var_to_conv[match(uniq_in_train,uniq_in_test,nomatch=0)!=0]
        
        #Now, we can encode these variables as factors
        data[,var_to_conv]<-lapply(data[,var_to_conv],as.factor)
        print("Done!",quote=F)
        
        
        print("Some of the initial factor variables don't have same levels in train and test",quote=F)
        print("We have to remove them",quote=F)
        uniq_in_train<-sapply(data[1:n,fact_var_names],unique)
        uniq_in_test<-sapply(data[-c(1:n),fact_var_names],unique)
        uniq_in_train<-sapply(uniq_in_train,sort)
        uniq_in_test<-sapply(uniq_in_test,sort)
        fact_to_remove<-fact_var_names[match(uniq_in_train,uniq_in_test,nomatch=0)==0]
        data<-data[,!(names(data)%in%fact_to_remove)]
        print("Done!",quote=F)
        
        
        print("Let's create variables which indicate the month and day of week",quote=F)
        data$month<-as.factor(months(data$Original_Quote_Date))
        data$day<-as.factor(weekdays(data$Original_Quote_Date))
        data<-select(data,-Original_Quote_Date)
        print("Done!",quote=F)
        
        print(
                data.frame(
                           factor=sum(sapply(data,is.factor)),
                           character=sum(sapply(data,is.character)),
                           time=1,
                           integer=sum(sapply(data,is.integer)))
                )
        
        data$response<-as.factor(response)
        return(data)
}


data<-feat_engin(train,test)
rm(train,test)
QuoteNumber<-data$QuoteNumber
data<-select(data,-QuoteNumber)



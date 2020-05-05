rm(list=ls())
library(e1071)

attrition <- read.csv("C:/Users/agome/Documents/attrition_data.csv", header =TRUE)

attrition$TERMINATION_YEAR[is.na(attrition$TERMINATION_YEAR)]<-0000
#converting all columns to factor type

attrition[] <- lapply(attrition, function(x) as.factor(x))
df1<-attrition[-c(1,6,13,15,16,18,19)]

#splitting data into test and training
sample_data<-sample(nrow(df1),0.70*nrow(df1))
training<-df1[sample_data,]
test<-df1[-sample_data,]

#calculating naieve Bayes
nBayes <- naiveBayes(STATUS~., data =training)
category<-predict(nBayes,test[,])
table(NBayes=category,STATUS=test$STATUS)

# calculating error rate
NB_wrong<-sum(category!=test$STATUS)

NB_error_rate<-NB_wrong/length(category)
NB_error_rate

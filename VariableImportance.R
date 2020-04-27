#cleaning environment
rm(list = ls())

#load csv
data_var<-read.csv('PATH_TO_CSV')

#Eliminate EmployeeID from prediction
df1<-data_var[-1]

#Convert JOB_GROUP to numeric as randomForest 'Can not handle categorical predictors with more than 53 categories.'
df1$JOB_GROUP<-as.numeric(df1$JOB_GROUP)

#Replace NA values with 0000
df1$TERMINATION_YEAR[is.na(df1$TERMINATION_YEAR)]<-0000

#Create fit model with target variable against the rest
library(randomForest)
rffit <- randomForest(df1$STATUS ~., data=df1, ntree=2000, keep.forest=FALSE, importance=TRUE)

#View importance of variables
importance(rffit) # relative importance of predictors (highest <-> most important)

#Plot importance(Plot is represented in descending order of importance)
varImpPlot(rffit) # plot results

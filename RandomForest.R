#cleaning environment
rm(list = ls())

#load csv
data_var<-read.csv('PATH_TO_CSV')

#Replace NA values with 0000
data_var$TERMINATION_YEAR[is.na(data_var$TERMINATION_YEAR)]<-0000

#Omiting Employee ID and columns with MeanGini less than 5.0
df1<-data_var[-c(1,6,13,15,16,18,19)]

#Convert JOB_GROUP to numeric as randomForest cannot handle categorical predictors with more than 53 categories.
df1$JOB_GROUP<-as.numeric(df1$JOB_GROUP)

#splitting data into test and training
sample_data<-sample(nrow(df1),0.70*nrow(df1))
training_data<-df1[sample_data,]
test_data<-df1[-sample_data,]

#Creating fit model with target variable against the rest
library(randomForest)
rffit <- randomForest(STATUS ~., data=training_data, ntree=30, keep.forest=TRUE, importance=TRUE)

#plotting fit to check optimal number of trees for best accuracy
plot(rffit, main = "Fit Plot")

#Predict test data results
pred<-predict(rffit,test_data)

# Making the Confusion Matrix 
table(test_data$STATUS, pred)

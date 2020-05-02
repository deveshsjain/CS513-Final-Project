#cleaning environment
rm(list = ls())

#load csv
data_var<-read.csv('C:/Users/deves/Desktop/Class/CS 513 KDD/Final Project/attrition_data.csv')

#Replace NA values with 0000
data_var$TERMINATION_YEAR[is.na(data_var$TERMINATION_YEAR)]<-0000

#converting all columns to integer type
data_var[] <- lapply(data_var, function(x) as.numeric(x))

#normalizing function
nor <-function(x) {return (x -min(x))/(max(x)-min(x))}

#Run nomalization on all columns
data_norm <- as.data.frame(lapply(data_var, nor))

#Omiting Employee ID and columns with MeanGini less than 5.0
df1<-data_norm[-c(1,6,13,15,16,18,19)]

#splitting data into test and training
sample_data<-sample(nrow(df1),0.70*nrow(df1))
training_data<-df1[sample_data,]
test_data<-df1[-sample_data,]

#SVM
library(e1071) 

#Linear classification, gamma = 1 (default for linear)
classifier = svm(formula = STATUS ~ ., 
                 data = training_data, 
                 type = 'C-classification', 
                 kernel = 'linear'
                 ) 

# Predicting the Test set results 
predicted_res<- predict(classifier, newdata = test_data) 

# Making the Confusion Matrix 
table(test_data$STATUS, predicted_res)


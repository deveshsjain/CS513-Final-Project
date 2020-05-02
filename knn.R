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

#Function to check accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

library(class)

#k=3
knn3<- knn(training_data, test_data,training_data$STATUS,k=3)

tabk3 <- table(knn3, test_data$STATUS)
tabk3

accuracy(tabk3)

#k=5
knn5<- knn(training_data, test_data,training_data$STATUS,k=5)

tab5 <- table(knn5, test_data$STATUS)
tab5

accuracy(tab5)

#k=10
knn10<- knn(training_data, test_data,training_data$STATUS,k=10)

tab10 <- table(knn10, test_data$STATUS)
tab10

accuracy(tab10)


#k=3 gives maximum accuracy
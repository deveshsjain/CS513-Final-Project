
rm(list=ls())
# load in data
attrition <- read.csv('PATH_TO_CSV', header = TRUE)
# set NA values in termination year to 0
attrition$TERMINATION_YEAR[is.na(attrition$TERMINATION_YEAR)]<-0000
#converting all columns to factor type
attrition[] <- lapply(attrition, function(x) as.numeric(x))
# remove unncessary columns from dataframe
df1<-attrition[-c(1,6,13,15,16,18,19)]
# sample_data<-sample(nrow(df1),0.70*nrow(df1))
# training<-df1[sample_data,]
# test<-df1[-sample_data,]
# calculating kmeans
kmeans_3<- kmeans(df1[,-14],2,nstart = 10)
#kmeans_3$cluster
# table(kmeans_3$cluster,training[,14])
# kpredict <- cl_pre#predict(kmeans_3, test, type = "Class") 
table(df1[,14], kmeans_3$cluster)
error <- (df1[,14]!=kmeans_3$cluster)
rate <- sum(error)/length(df1[,14])
rate


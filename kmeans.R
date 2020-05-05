
rm(list=ls())
# load in data
attrition <- read.csv("C:/Users/agome/Documents/attrition_data.csv",na.strings = " ")
# set NA values in termination year to 0
attrition$TERMINATION_YEAR[is.na(attrition$TERMINATION_YEAR)]<-0000
#converting all columns to factor type
attrition[] <- lapply(attrition, function(x) as.factor(x))

# remove unncessary columns from dataframe
df1<-attrition[-c(1,6,13,15,16,18,19)]

# calculating kmeans
kmeans_3<- kmeans(df1[,20],3,nstart = 10)
kmeans_3$cluster
table(kmeans_3$cluster,df1[,20])

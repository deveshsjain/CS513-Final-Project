
rm(list=ls())

# load in data
attrition <- read.csv("C:/Users/agome/Documents/attrition_data.csv",na.strings = " ")
# set NA values in Termination Year to 0
attrition$TERMINATION_YEAR[is.na(attrition$TERMINATION_YEAR)]<-0000
#converting all columns to factor type
attrition[] <- lapply(attrition, function(x) as.factor(x))

# remove unnecessary columns from data frame
df1<-attrition[-c(1,6,13,15,16,18,19)]

# calculating distribution
attrDist<-dist(df1[,20])

#calculating and plotting hcluster
results<-hclust(attrDist)
plot(results)

hclust_3<-cutree(results,3)
table(hclust_3, df1[,20])


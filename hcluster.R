rm(list=ls())
# load in data
attrition <- read.csv('PATH_TO_CSV', header = TRUE)
# set NA values in Termination Year to 0
attrition$TERMINATION_YEAR[is.na(attrition$TERMINATION_YEAR)]<-0000
#converting all columns to factor type
attrition[] <- lapply(attrition, function(x) as.numeric(x))
# remove unnecessary columns from data frame
df1<-attrition[-c(1,6,13,15,16,18,19)]
# sample_data<-sample(nrow(df1),0.70*nrow(df1))
# training<-df1[sample_data,]
# test<-df1[-sample_data,]
# calculating distribution
attrDist<-dist(df1[,-14])
#calculating and plotting hcluster
results<-hclust(attrDist)
plot(results)
hclust_2<-cutree(results,2)
#is.vector(hclust_2)
table(hclust_2, df1[,14])
#prediction <- predict(hclust_2, test[,20])

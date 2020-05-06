#C4.5 Analysis

rm(list=ls())
# 
setwd('~/Documents/COLLEGE/cs513/HW2')
attrition <- read.csv("attrition_data.csv", header = TRUE)
attrition[is.na(attrition)] <- 0000

attrition$TERMINATION_YEAR[is.na(attrition$TERMINATION_YEAR)]<-0000
attrition[] <- lapply(attrition, function(x) as.numeric(x))

# Extract rows of relevant data
df1<-attrition[-c(1,6,13,15,16,18,19)]

split_data <- sample(nrow(df1),0.70*nrow(df1))
training<-df1[split_data,]
test<-df1[-split_data,]


library('C50')
# Do C4.5 analysis
C50_class <- C5.0(as.factor(STATUS)~.,data=training )

summary(C50_class )
plot(C50_class)

# Predict using test data
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,14],C50=C50_predict)

# Error Rate
wrong<- (test[,14]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,14])
c50_rate
rm(list = ls())
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer) 
# load in data
attrition <- read.csv('PATH_TO_CSV', header = TRUE)

# set NA values in termination year to 0
attrition$TERMINATION_YEAR[is.na(attrition$TERMINATION_YEAR)]<-0000

#converting all columns to factor type
attrition[] <- lapply(attrition, function(x) as.factor(x))

# remove unnecessary columns from data frame
df1<-attrition[-c(1,6,13,15,16,18,19)]

#splitting data into test and training
sample_data<-sample(nrow(df1),0.70*nrow(df1))
training<-df1[sample_data,]
test<-df1[-sample_data,]

#calculate CART
CART_class<-rpart(STATUS~.,data = training)

# plot CART
rpart.plot(CART_class)

# calculate preidctions
CART_predict2<-predict(CART_class, test, type="class") 
table(Actual=test[,14],CART=CART_predict2)
CART_predict<-predict(CART_class,test) 

# calculating error rate
table(Actual=test[,14],CART=CART_predict2)
CART_wrong<-sum(test[,14]!=CART_predict2)
error_rate<-CART_wrong/length(test$STATUS)
error_rate

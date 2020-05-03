#Using package ANN2
#cleaning environment
rm(list = ls())

#load csv
data_var<-read.csv('PATH_TO_CSV')

#Replace NA values with 0000
data_var$TERMINATION_YEAR[is.na(data_var$TERMINATION_YEAR)]<-0000

#converting all columns to integer type
data_var[] <- lapply(data_var, function(x) as.numeric(x))

#Dropping column id and mean gini less than 5.0
#data_var1<-data_var[-1]
data_var1<-data_var[-c(1,6,13,15,16,18,19)]

#splitting data into test and training
random_draw <- sample(1:nrow(data_var1), 0.70*nrow(data_var1))
X_train <- data_var1[random_draw, -14]
y_train <- data_var1[random_draw, 14]
X_test <- data_var1[setdiff(1:nrow(data_var1), random_draw), -14]
y_test<- data_var1[setdiff(1:nrow(data_var1), random_draw), 14]

#install.packages('ANN2')
library(ANN2)

# Train neural network on classification task
NN <- neuralnetwork(X = X_train, y=y_train ,hidden.layers = 5)#,optim.type ='adam', learn.rates = 0.01, val.prop = 0)

# Plot the loss during training
plot(NN)

# Make predictions
y_pred <- predict(NN, newdata = X_test)

# Plot predictions
correct <- (y_test == y_pred$predictions)
#correct
#plot(X_test, pch = as.numeric(y_test), col = correct + 2)

#ANN

#cleaning environment
rm(list = ls())

#load csv
data_var<-read.csv('PATH_TO_CSV')

#Replace NA values with 0000
data_var$TERMINATION_YEAR[is.na(data_var$TERMINATION_YEAR)]<-0000

#converting all columns to integer type
data_var[] <- lapply(data_var, function(x) as.numeric(x))

#Dropping column id
data_var1<-data_var[-c(1,6,13,15,16,18,19)]

#splitting data into test and training
sample_data<-sample(nrow(data_var1),0.70*nrow(data_var1))
training_data<-data_var1[sample_data,]
test_data<-data_var1[-sample_data,]

#install.packages('neuralnet')
library(neuralnet)

nn<-neuralnet(STATUS~., training_data, hidden = 5, threshold = 0.01 )
#plot(nn)

pred <- predict(nn, test_data)
#print(nn)

#Confusion matrix
nn.results <- compute(nn, test_data)
results <- data.frame(actual = test_data$STATUS, prediction = nn.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

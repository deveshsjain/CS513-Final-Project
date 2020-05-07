# PCA Analysis
library(factoextra)

# Read files, set NA rows equal to 0000
attrition <- read.csv('PATH_TO_CSV', header = TRUE)
attrition[is.na(attrition)] <- 0000

attrition$TERMINATION_YEAR[is.na(attrition$TERMINATION_YEAR)]<-0000
attrition[] <- lapply(attrition, function(x) as.numeric(x))

# Extract rows of relevant data
df1<-attrition[-c(1,6,13,15,16,18,19)]

split_data <- sample(nrow(df1),0.70*nrow(df1))
training<-df1[split_data,]
test<-df1[-split_data,]

# doing initial PCA analysis
res.pca <- prcomp(training, scale. = TRUE)

#plotting eigenvalues, showing the percentages of variances explained by each principal component
fviz_eig(res.pca)

prediction <- predict(res.pca, newdata = test)





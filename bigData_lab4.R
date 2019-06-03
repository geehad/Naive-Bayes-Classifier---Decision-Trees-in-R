rm(list = ls())

######################################### 1
dir.create("bigData_lab5")
setwd("/home/gehad/bigData_lab5")
getwd()
######################################## 2

df_naive <- read.csv("nbtrain.csv")
View(df_naive)
# The variables are : age , gender , educ , income

######################################## 3
df_naive_dims <- dim(df_naive)
no_rows <- df_naive_dims[1]

df_train <- df_naive[1:9000,]
View(df_train)

df_test <- df_naive[9001:no_rows,]
View(df_test)

# split dataset into train to train the naive bayes model and the test part to test it

####################################### 4
#install.packages("e1071")
library(e1071)
str(df_naive)

naive_model <- naiveBayes(x=df_train[,1:3] , y=df_train[,4],laplace=0.01)
# Laplace smoothing coefficient = 0.01 means that  smoothing parameter alpha = 0.01
# and this mean that an amount(0.01) added to the number of observed cases in order to change the expected probability in a model of those data when not known to be zero
# this help to avoid overfitting 

####################################### 5  
naive_model
####################################### 6
NB_Predictions=predict(naive_model,df_test[,1:3])
View(NB_Predictions)
###################################### 7
confusionMatrix <- table(NB_Predictions,df_test$income)
confusionMatrix
# the model classifies correctly 797 data as first class in income out of 803 total right data belonging to class 1 of income
# and doen't classify any point belonging to class 2 out of 132 total right point belonging to class 2
# and classifies only 8 points belonging to class 3 out of 75 total right points belonging to class 3
# so the best performance of the model is for class 1 of income (10-50K)

##################################### 8
sum_cols <- colSums(confusionMatrix)
total_sum <- sum(sum_cols)
total_sum
model_accuracy <- ((confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3])/(total_sum)) * 100
model_accuracy

#################################### 9
class1_misclassificationRate <- (confusionMatrix[2,1]+confusionMatrix[3,1])/sum(confusionMatrix[,1])
class1_misclassificationRate

class2_misclassificationRate <- (confusionMatrix[1,2]+confusionMatrix[3,2])/sum(confusionMatrix[,2])
class2_misclassificationRate

class3_misclassificationRate <- (confusionMatrix[1,3]+confusionMatrix[2,3])/sum(confusionMatrix[,3])
class3_misclassificationRate
###################################


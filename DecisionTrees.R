setwd("/home/gehad/bigData_lab5")
getwd()

#install.packages("rpart.plot")
#install.packages("ROCR")
library("rpart")
library("rpart.plot")
library("ROCR")

#Read the data
play_decision <- read.table("DTdata.csv",header=TRUE,sep=",")
play_decision
summary(play_decision)

#Build the tree to "fit" the model
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 3),
             parms=list(split='information'))
# split='information' : means split on "information gain" 
#plot the tree
rpart.plot(fit, type = 4, extra = 1)

summary(fit)
#######################################################################################
# Q1: what is the defult value for split?                                      
# split defaults to gini

# Q2: what are the meanings of these control parameters?  
#          1- "minsplit=2"
#The minimum number of observations that must exist in a node in order for a split to be attempted is 2
#          2- "maxdepth=3" 
#The maximum depth of any node of the final tree, with the root node counted as depth 0 is 3
#          3- "minbucket=4" 
#The minimum number of observations in any terminal node is 4
# Support your answers with graphs for different values of these parameters.  ---------> Graphs in the pdf named Graphs_lab5_req2.pdf

#Q3: What will happen if only one of either minsplit or minbucket is specified
#    and not the other?
#  The code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate

#Q4: What does 'type' and 'extra' parameters mean in the plot function?
# 'type' is Type of plot that takes possible values : 0 , 1 , 2 , 3 , 4 , 5 each type plot a type as specified in the help
# 'extra' Display extra information at the nodes and possible values are : "auto" , 1 , 2 , 3 , 4 , 5 , 6 , 7 ,8 , 9 ,10 ,11 , +100 where each value display specific information on the plot

#Q5: Plot the tree with propabilities instead of number of observations in each node.
######################################################################################
rpart.plot(fit, type = 4, extra = 4) 
#Predict if Play is possible for condition rainy, mild humidity, high temperature and no wind
newdata <- data.frame(Outlook="overcast",Temperature="mild",Humidity="high",Wind=FALSE)
newdata
predict(fit,newdata=newdata,type=c("class"))
# type can be class, prob or vector for classification trees.

######################################################################################
#Q6: What is the predicted class for this test case?
#the predicted class is yes
#Q7: State the sequence of tree node checks to reach this class (label).
# starts from the root then checks the temperature then go left as it is mild , then checks Outlook so go right as it is overcast and stop at this leaf node and predicted yes as the prop of yes is 100%

## ================================= END ===================================== ##
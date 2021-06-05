rm(list=ls())

setwd("F:\\CMP 2020\\Big Data\\Labs\\Lab 4 - Classification\\Lab 4 - Classification\\Lab\\Req2")

#install.packages("rpart.plot")
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
# Gini -> it's a measure to split the node other than entropy

# Q2: what are the meanings of these control parameters?  
#          1- "minsplit=2"
#
#          2- "maxdepth=3" 
#
#          3- "minbucket=4" 
#
# Support your answers with graphs for different values of these parameters.

### minsplit -> the min number of observations in a node so a split is attempted.
### Ex: minsplit = 7 --> there will be non-terminal nodes with less than 7 obsrvations.
fit1 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=7, maxdepth = 3, minbucket = 1),
             parms=list(split='information'))
rpart.plot(fit1, type = 4, extra = 1)
summary(fit1)

### maxdepth -> the max depth of the tree.
### Ex: maxdepth = 1 --> will only have root (depth 0) and terminal nodes (depth 1) only
fit2 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 1, minbucket = 1),
             parms=list(split='information'))
rpart.plot(fit2, type = 4, extra = 1)
summary(fit2)

### minbucket --> the min number of observations in a terminal node
### minbucket = 2, terminal nodes with observations less than 2 will be deleted.
fit3 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 3, minbucket = 2),
             parms=list(split='information'))
rpart.plot(fit3, type = 4, extra = 1)
summary(fit3)

#Q3: What will happen if only one of either minsplit or minbucket is specified
#    and not the other?

##A3: From help:
#If only one of minbucket or minsplit is specified, 
#the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate.

#Q4: What does 'type' and 'extra' parameters mean in the plot function?

###A4: From Help
## type --> specify the type of the plot, values: 0,1,2,3,4,5
## extra --> Display extra information at the nodes, values: 0->11

#Q5: Plot the tree with propabilities instead of number of observations in each node.
##A5: extra = 4
rpart.plot(fit, type = 4, extra = 4)

######################################################################################
 
#Predict if Play is possible for condition rainy, mild humidity, high temperature and no wind
newdata <- data.frame(Outlook="overcast",Temperature="mild",Humidity="high",Wind=FALSE)
newdata
predict(fit,newdata=newdata,type=c("class"))
# type can be class, prob or vector for classification trees.

######################################################################################
#Q6: What is the predicted class for this test case?
#A6: The predicted class is "yes"
#Q7: State the sequence of tree node checks to reach this class (label).
#A7: The sequence = node 1 ->(Temperature="mild)-> node 2 ->Outlook="overcast"-> node 5 (leaf)
## ================================= END ===================================== ##

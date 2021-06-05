rm(list=ls())

setwd("F:\\CMP 2020\\Big Data\\Labs\\Lab 4 - Classification\\Lab 4 - Classification\\Lab\\Req1")

library("e1071")

sample = read.table("nbtrain.csv", header=TRUE, sep=",")
## Comment
# The variables:
# age: 20-30, 31-45, GT 45
# gender: F, M
# educ: College, Others, Prof/Phd
# income: 10-50K, 50-80K, GT 80K

#(3) we will now define the data frames to use the NB classifier
traindata = as.data.frame(sample[1:9000,])
testdata = as.data.frame(sample[9001:10010,])
## Comment
# We split the data to train, test to evaluate the model on unseen examples (test set).
# Since the model is expected to perform well on seen data (train), we need to see how
# general the model is by testing it on unseen data (test).

#(4)
model = naiveBayes(income~., data=traindata, laplace=0.01)
## Comment
# Laplace smoothing is a way to solve the problem of "Zero Probability" by adding
# like fake samples, for example if X = (.., age=20-30,..)
# and P(age=20-30|y=10-50K) = 0 this will yield 
# Conditional prob. to be zero P(X|y=10-50K) = 0.
# To avoid that we use the following rule P(X=xi|Y=yi) = (x(i) + l)/(N + l*d)
# N -> number of samples of class yi.
# x(i) -> number of samples of feature x = i within the class yi.
# l -> laplace coeff.
# d -> number of categories x can take.
### EXAMPLE
ageCounts = table(traindata[,c("income", "age")])
ageCounts
ageCounts = ageCounts/rowSums(ageCounts)
ageCounts
## P(age=31-45|income=GT 80K) = (220+l)/(646+(l*3))

#(5)display model
model

#(6)
results = predict (model, testdata)
# display results
results

#(7) Confusion Matrix
confusion = table(results, testdata$income, dnn=list('Predict', 'Actual'))
confusion = cbind(confusion, total=rowSums(confusion))
confusion = rbind(confusion, total=colSums(confusion))

# cols -> Actual class (T)
# rows -> Predict class (P)
confusion
## Comment
# Most of the predictions are '10-50K' since most of the training data (78%) belongs to
# the class '10-50K' so the model is kinda biased to that class.
# The classes should be almost equally represented in the training data.

acc = sum(results == testdata$income) / 1010
acc
##(8)Accuracy is almost 80% which isn't bad BUT again a big portion of the test set
# belongs to the class '10-50K' whice the model is biased to. Thus
# if the test set had a bigger portion of the other classes the accuracy would be low.
# A model that predicts only class '10-50K' would achieve similar results.
# So overall the model isn't good even with good accuracy since data is biased.

#(9) Misclassification rate = 1 - TPR(class)
## for class '10-50K'
1 - confusion[1,1]/confusion[4,1]
## for class '50-80K'
1 - confusion[2,2]/confusion[4,2]
## for class 'GT 80K'
1 - confusion[3,3]/confusion[4,3]

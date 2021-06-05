
#clean environment
rm(list=ls())
#get working directory
getwd()
#set working directory
setwd("F:\\CMP 2020\\Big Data\\Lab 1 - Introduction to R\\Requirement")

df = read.csv("titanic.csv")

#3
#================================================
#Show the dimensions of the data frame
dim(df)

#Structure
str(df)

#first and the last ten rows in the dataset.
head(df)
tail(df)

#Summary
summary(df)

#4
#================================================
#summary, 1st and 3rd Quartile
summary(df$Age)

# NA
is.na(df$Age)

# factors
df$Embarked %in% c("C", "Q", "S")
levels(df$Embarked)

#5
#================================================

df = df[!is.na(df$Age) & df$Embarked %in% c("C", "Q", "S"),]

is.na(df$Age)

# factors
df$Embarked %in% c("C", "Q", "S")
# still same levels ??????
levels(df$Embarked)

df = df[, !(names(df) %in% c("Cabin", "Ticket"))]

dim(df)
str(df)

#6
#================================================

t1 = table(df$Gender)
t1

pie(t1, col=c("red", "blue"), main="Count of Gender")

t2 = table(df$Gender, df$Survived)
t2

#More females survived
pie(t2[,2], col=c("red", "blue"), main="Gender vs Survivors")

t3 = table(df$Survived, df$Pclass)
t3

#The lower the class, the more victims exist
barplot(t3, col=c("red", "blue"), main="Class vs Survivors", xlab="class", ylab="count")

#graph for min,max,median, 1st and 3rd quartile
boxplot(df$Age, col="yellow")

hist(df$Age, col="darkblue", main="Age Density Distribution", xlab="Age")

#7
#==============================================
df_processed = df[, names(df) %in% c("Name", "Survived")]
write.csv(df_processed, "titanic_preprocessed.csv")

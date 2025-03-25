#proj component two
library(ggplot2)
library(rpart)
library(rpart.plot)
#install.packages("naivebayes")
library(naivebayes)


setwd("/Users/krishapatel/Downloads/CS111/React/Data 101/Airline Satisfaction")
AirlineTrain <- read.csv("train.csv")
AirlineTest <- read.csv("test.csv")
str(AirlineTrain)
#all plots for project component 2
#chatGPT was used to efficiently create all plots and then we edited code
#for visual aesthetics and readability. We selected 6 plots that we
#thought best represented trends in data.

#load training data
training <- read.csv("test.csv", header=TRUE)
head(training)

#mosaic plot for Type of Travel
colors <- c('darkblue', 'lightblue')
mosaicplot(training$satisfaction~training$Type.of.Travel, xlab="Satisfaction",ylab="Type of Travel", main="Mosaic of Travel Type vs Satisfaction", col=colors, border="black")

#mosaic plot for Type of Class Flown
colors <- c('darkblue', 'cornflowerblue', 'skyblue')
mosaicplot(training$satisfaction~training$Class, xlab="Satisfaction",ylab="Class", main="Mosaic of Class Flown vs Satisfaction", col=colors, border="black")

#box plots for Inflight Service
library(ggplot2)
ggplot(training, aes(x = satisfaction, y = Inflight.service, fill = satisfaction)) +
  geom_boxplot() +
  labs(x = "Satisfaction", y = "Inflight Service") +
  ggtitle("Relationship between Satisfaction and Inflight Service")

#box plots for Cleanliness
ggplot(training, aes(x = satisfaction, y = Cleanliness, fill = satisfaction)) +
  geom_boxplot() +
  labs(x = "Satisfaction", y = "Cleanliness") +
  ggtitle("Relationship between Satisfaction and Cleanliness")

#box plots for Seat Comfort
ggplot(training, aes(x = satisfaction, y = Seat.comfort, fill = satisfaction)) +
  geom_boxplot() +
  labs(x = "Satisfaction", y = "Seat Comfort") +
  ggtitle("Relationship between Satisfaction and Seat Comfort")

#box plots for Inflight Wifi Service
ggplot(training, aes(x = satisfaction, y = Inflight.wifi.service, fill = satisfaction)) +
  geom_boxplot() +
  labs(x = "Satisfaction", y = "Wifi Service") +
  ggtitle("Relationship between Satisfaction and Wifi Service")

#box plots for Online Boarding
ggplot(training, aes(x = satisfaction, y = Online.boarding, fill = satisfaction)) +
  geom_boxplot() +
  labs(x = "Satisfaction", y = "Online Boarding") +
  ggtitle("Relationship between Satisfaction and Online Boarding")



#rpart prediction model
summary(AirlineTrain)
# Seat.comfort+ Leg.room.service+ Ease.of.Online.booking+Gate.location+Food.and.drink

tree <- rpart(satisfaction ~ ., data = AirlineTrain)
rpart.plot(tree, main = "decision tree for airline satisfaction")
satisfactionPrediction <- predict(tree, AirlineTrain, type = "vector")
summary(satisfactionPrediction)

#confusion matrix for training data
(matrix <- table(satisfactionPrediction, AirlineTrain$satisfaction))
1-sum(diag(matrix))/sum(matrix)
#missclassification is 11.54527%

#calculation for accuracy
correct_predictions <- sum(diag(matrix))
total_predictions <- sum(matrix)
accuracy <- correct_predictions / total_predictions
(accuracy) #is 88.45473%

#naive bayes classification
naive_tree <- naive_bayes(satisfaction~., data = AirlineTrain)
#satisfactionBayesPrediction <- predict(naive_tree, AirlineTrain, usekernel = T)
bayesMatrix <- table(satisfactionBayesPrediction, AirlineTrain$satisfaction)
(bayesMatrix)
1 - sum(diag(bayesMatrix)) / sum(bayesMatrix)
#missclassification is 17.7%


#  Read data in dataframe
data <- read.csv("data/train.csv")
# Consider a data structure...
#str(data)
# Replace lines without age by median
data$Age[is.na(data$Age)] <- median(data$Age, na.rm=TRUE)
#Create a model of dependence of survival of a passenger gender, a class which he travels and age.
model = glm(Survived ~ Sex + Pclass + Age, data=data, family = binomial) #binomial means that selected 'logistic regression'
#Consider our model...
#summary(model)

t <- 0.4 #the threshold level
# Read test data
# test <- read.csv("data/test.csv")
# predictResault <- predict(model, newdata = test, type="response")
# test$Survived[predictResault >= t] = 1
# test$Survived[predictResault < t] = 0

# Using the model to predict on test data
# do not specify the parameter newdata
predictTrain <- predict(model, type="response")
# s <- table(data$Survived,predictTrain > t)
# TP <- s[2,2]
# TN <- s[1,1]
# FP <- s[1,2]
# FN <- s[2,1]
# TPR <- TP / (TP + FN) #sensitivity (~recall)
# FPR <- FP / (FP + TN)
# TNR <- TN / (TN + FP) #specificity
# FNR <- FN / (FN + TP)

# install.packages("ROCR")
# install.packages("PRROC")
#require("ROCR")
require("PRROC")

fg <- predictTrain[ data$Survived == 1 ]
bg <- predictTrain[ data$Survived == 0 ]
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr, xaxs = "i", yaxs = "i")
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc, xaxs = "i", yaxs = "i")
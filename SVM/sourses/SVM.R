#install.packages("e1071")
require("e1071")
# install.packages("kernlab")
require("kernlab")
require("MASS")


data(iris)
attach(iris)

x <- subset(iris, select = -5) #-Species
y <- iris$Species
svm_model <- svm(Species ~ ., data = iris)
#summary(svm_model)
pred <- predict(svm_model,x)
#table(pred,y)

# svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
# #print(svm_tune)
# svm_model_after_tune <- svm(Species ~ ., data = iris, kernel="radial", cost=1, gamma=0.5)
# # summary(svm_model_after_tune)

svm.data <- iris[ Species!="virginica" ,]
svm.data$Species <- factor(svm.data$Species)
svm.fit <- svm(Species ~ Petal.Length + Petal.Width, data = svm.data, kernel="linea")
plot(svm.fit, svm.data, Petal.Width ~ Petal.Length)

svm.data <- iris[ Species!="setosa" ,]
svm.data$Species <- factor(svm.data$Species)
svm.fit <- svm(Species ~ Petal.Length + Petal.Width, data = svm.data, kernel="linea")
plot(svm.fit, svm.data, Petal.Width ~ Petal.Length)

data(cats)
svm.data <- data.frame(cats[, c(2,3)], response = as.factor(cats$Sex))
svm.fit <- svm(response ~., data = svm.data, kernel = "linear", cost = 10, scale = FALSE)
plot(svm.fit, svm.data)
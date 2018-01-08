#install.packages("e1071")
require("e1071")
# install.packages("kernlab")
require("kernlab")


#attach(iris)
x <- subset(iris, select = -Species)
y <- Species
svm_model <- svm(Species ~ ., data = iris)
#summary(svm_model)
pred <- predict(svm_model,x)
#table(pred,y)

# svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
# #print(svm_tune)
# svm_model_after_tune <- svm(Species ~ ., data = iris, kernel="radial", cost=1, gamma=0.5)
# # summary(svm_model_after_tune)


data(iris)
svm_data <- iris[ Species!="virginica" ,]
svm_model_LS <- svm(Species ~ Petal.Length + Petal.Width, data = svm_data, kernel="linea")
plot(svm_model_LS, svm_data, Petal.Width ~ Petal.Length)

data(iris)
svm_data <- iris[ Species!="setosa" ,]
svm_model_NLSS <- svm(Species ~ Petal.Length + Petal.Width, data = svm_data, kernel="linea")
plot(svm_model_NLSS, svm_data, Petal.Width ~ Petal.Length)

data(iris)
svm_data <- iris[ , ]
svm_model_NLSL <- svm(Species ~ Sepal.Length + Petal.Width, data = svm_data)
plot(svm_model_NLSL, svm_data, Petal.Width ~ Sepal.Length, slice = list(Petal.Width = 3, Sepal.Length = 4))
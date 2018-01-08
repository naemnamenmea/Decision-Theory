require(e1071)
data("iris")
model <- naiveBayes(Species ~ ., data = iris)
# class(model)
# summary(model)
# print(model)

predict(model, iris[,-5], type="raw")

preds <- predict(model, newdata = iris[,-5])
conf_matrix <- table(preds, iris[,5])



tbl_list <- sapply(iris[-5], table, iris[ , 5])
tbl_list <- lapply(tbl_list, t)

cond_probs <- sapply(tbl_list, function(x) { 
  apply(x, 1, function(x) { 
    x / sum(x) }) })

cond_probs <- lapply(cond_probs, t)

print(cond_probs)
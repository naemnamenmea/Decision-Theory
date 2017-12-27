colors <- c("setosa" ="red", "versicolor" ="green3", "virginica" ="blue")
fontcolors <- c("versicolor" ="green3", "virginica" ="blue", "setosa" ="red")

a <- 3
b <- 4
xl <- iris[, c(a,b,5)]
l <- dim(xl)[1]
n <- dim(xl)[2]-1
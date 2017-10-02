# source("C:\\Users\\Андрей\\Desktop\\СМПР\\test.r", echo = TRUE)
# source('C:/Users/Андрей/Desktop/СМПР/test.r', echo = TRUE)
colors <- c("setosa" ="red", "versicolor" ="green3", "virginica" ="blue")
fontcolors <- c("setosa" ="red", "versicolor" ="green3", "virginica" ="blue")
# Евклидово расстояние
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}
# Сортируем объекты согласно расстояния до объекта z
sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  # Создаём матрицу расстояний
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  # Сортируем
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl)
}
# Применяем метод kNN
kNN <- function(xl, z, k)
{
  # Сортируем выборку согласно классифицируемого объекта
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  # Получаем классы первых k соседей
  classes <- orderedXl[1:k, n + 1]
  # Составляем таблицу встречаемости каждого класса
  counts <- table(classes)
  # Находим класс, который доминирует среди первых k соседей
  class <- names(which.max(counts))
  return (class)
}
a <- 3
b <- 4
xl <- iris[, c(a,b,5)]
# Создаем пустой холст
plot(main="Classification of Irises", 1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2], xlim=c(0, 7.5), ylim=c(0, 4), xaxs="i", yaxs="i")
# Классификация поля объектов
for(i in seq(from=0, to=4, by=0.5)) {
  for(j in seq(from=0, to=7.5, by=0.5)) {
    class <- kNN(xl, c(j,i), k=1)
    points(j, i, pch =1, asp =1, cex =3, col =fontcolors[class], lwd =2)
  }
}
# Рисуем выборку
points(iris[, c(a,b)], pch =21, bg = colors[iris$Species], col ="black", asp=1)
legend("topleft", legend=c("setosa", "versicolor", "virginica"),
       cex=1.5, pch = c(21,21,21),
        text.font=6, pt.bg=colors, xpd=TRUE, horiz=FALSE, bty="y", bg="white")

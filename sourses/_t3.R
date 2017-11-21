euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}
#ќЅ–ј«≈÷
kNN <- function(xl, z, k, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl <- xl[order(distances[, 2]), ]
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}
#l и n мы вычисл€ем заранее
kNN <- function(xl, z, k, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)
  st <- 0
  for (i in 1:l)
  {
    st <- system.time(metricFunction(xl[i, 1:n], z))
    # distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl <- xl[order(distances[, 2]), ]
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (st)
}

  sum <- 0
  for (i in 1:100)
  {
    sum <- sum+ system.time(euclideanDistance(xl[i, 1:2], c(i,i)))[3]
    # sum <- sum+kNN(xl,c(i,i),6)[3]
  }
  sum

#n и l мы считаем по ходу дела
kNN <- function(xl, z, k, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl <- xl[order(distances[, 2]), ]
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}
euclideanDistance <- function(u, v) #ф-ция расстояния для 2х точек
{
  sqrt(sum((u - v)^2))
}

ker.type <- c("uniform",    "triangle",  "parabolic", "biquadratic",
              "tricaprate", "tricubic",  "gaussian",  "cosine",
              "logistics",  "sigmoidal", "silvermans")

kerne <- function(u, type)
{
  switch(type,
         uniform     = ifelse(abs(u)>1, 0, 1/2),
         triangle    = ifelse(abs(u)>1, 0, 1-abs(u)),
         parabolic   = ifelse(abs(u)>1, 0, 3/4*(1-u^2)),
         biquadratic = ifelse(abs(u)>1, 0, 15/16*(1-u^2)^2),
         tricaprate  = ifelse(abs(u)>1, 0, 35/32*(1-u^2)^3),
         tricubic    = ifelse(abs(u)>1, 0, 70/81*(1-abs(u)^3)^3),
         gaussian    = 1/sqrt(2*pi)*exp(-1/2*u^2),
         cosine      = ifelse(abs(u)>1, 0, pi/4*cos(pi/2*u)),
         logistics   = 1/(exp(u)+2+exp(-u)),
         sigmoidal   = 2/pi/(exp(u)+exp(-u)),
         silvermans  = exp(-abs(u)/sqrt(2))/2*sin(abs(u)/sqrt(2)+pi/4)
  )
}

loo <- function(k, xl, a=kNN)
{
  l <- nrow(xl)
  point <- xl[ , -ncol(xl) ]
  sum <- 0
  for (i in 1:l)
  {
    if(a(xl[-i, ], point[i, ], k) != xl$Species[i])
    {
      sum <- sum+1
    }
  }
  sum/l
}

loo_potential <- function(xl,  h, gamma){
  
  sum <- 0
  l <- nrow(xl)
  n <- ncol(xl)
  
  for(i in 1:l) {
    value <- potentialFunc(xl[-i, ],  xl[i, -n], h, gamma[-i])
    if(xl[i, n] != value) {
      sum <- sum + 1
    }
  }
  return(sum / l)  
}

w <- function(i,k=0,q=0) #весовая ф-ция
{
  if(k>0) {
    (k+1-i)/k
  }
  else if(q>0&q<1){
    q^i
  }
  else return(-1)
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1

  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl)
}

## Восстановление центра нормального распределения
estimateMu <- function(objects)
{
  ## mu = 1 / m * sum_{i=1}^m(objects_i)
  rows <- dim(objects)[1]
  cols <- dim(objects)[2]
  mu <- matrix(NA, 1, cols)
  for (col in 1:cols)
  {
    mu[1, col] = mean(objects[,col])
  }
  return(mu)
}

## Восстановление ковариационной матрицы нормального распределения
estimateCovarianceMatrix <- function(objects, mu)
{
  rows <- dim(objects)[1]
  cols <- dim(objects)[2]
  sigma <- matrix(0, cols, cols)
  for (i in 1:rows)
  {
    sigma <- sigma + (t(objects[i,] - mu) %*%
                        (objects[i,] - mu)) / (rows - 1)
  }
  return (sigma)
}

## Нормализация обучающей выборки
trainingSampleNormalization <- function(xl)
{
  n <- dim(xl)[2] - 1
  for(i in 1:n)
  {
    xl[, i] <- (xl[, i] - mean(xl[, i])) / sd(xl[, i])
  }
  return (xl)
}

## Добавление колонки для из -1 для w0
trainingSamplePrepare <- function(xl)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  xl <- cbind(xl[, 1:n], seq(from = -1, to = -1,
                             length.out = l), xl[, n + 1])
}
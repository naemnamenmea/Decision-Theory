euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
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
kNN <- function(xl, z, k)
{
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}
w <- function(i,k=0,q=0)
{
  if(k>0) {
    (k+1-i)/k
  }
  else if(q>0&q<1){
    q^i
  }
  else return(-1)
}
kwNN <- function(xl, z, k)
{
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k, n + 1]
  classesList <- unique(xl[,n+1])
  counts <- rep(0,length(classesList))
  for (i in 1:k)
  {
    counts[which(classes[i]==classesList)] =
      counts[which(classes[i]==classesList)] + w(i,k)
  }
  as.character(classesList[which.max(counts)])
}
loo <- function(k, xl, a=kNN)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  sum <- 0
  for (i in 1:l)
  {
    if(a(xl[-i, ], xl[i,-(n+1)], k) != xl$Species[i])
    {
      sum <- sum+1
    }
  }
  sum/l
}
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
parsenWindowFix <- function(xl, u, h, kerType=ker.type[3])
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  classesList <- unique(xl[ , n+1])
  counts <- rep(0,length(classesList))
  for (i in 1:l)
  {
    counts[which(xl[i,n+1]==classesList)] <-
      counts[which(xl[i,n+1]==classesList)] +
      kerne(euclideanDistance(u,xl[i, 1:n])/h,kerType)
  }
  if(max(counts)==0) return("")
  as.character(classesList[which.max(counts)])
}
parsenWindowFloat <- function(xl, u, k, kerType=ker.type[3])
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, euclideanDistance(xl[i, 1:n], u))
  }
  pos <- order(distances[, 2])
  orderedXl <- xl[pos, ]
  h <- 1
  for (i in (k+1):l)
  {
    if(distances[pos[i], 2]!=0)
    {
      h <- distances[pos[i], 2]
      break
    }
  }
  classesList <- unique(orderedXl[ , n+1])
  counts <- rep(0,length(classesList))
  for (i in 1:l)
  {
    counts[which(orderedXl[i,n+1]==classesList)] <-
      counts[which(orderedXl[i,n+1]==classesList)] +
      kerne(distances[pos[i], 2]/h,kerType)
  }
  as.character(classesList[which.max(counts)])
}
Margin <- function(xl,i)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  classesList <- unique(xl[,n+1])
  counts <- rep(0,length(classesList))
  distances <- matrix(NA, l, 2)
  for (j in 1:l)
  {
    distances[j, ] <- c(j, euclideanDistance(xl[i, 1:n], xl[j,1:n]))
  }
  orderedXl <- xl[order(distances[-i, 2]), ]
  for(j in  1:(l-1))
  {
    ind <- which(orderedXl[j,n+1]==classesList)
    counts[ind] <- counts[ind]+w(j,k=l)
  }
  return(counts[which(xl[i,n+1]==classesList)] -
           max(counts[-which(xl[i,n+1]==classesList)]))
}

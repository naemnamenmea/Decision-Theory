potentialFunc <- function(xl,u,h,charge,metricFunction = euclideanDistance)
{#не работает почему-то ((
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], u))
  }
  orderedXl <- xl[order(distances[, 2]), ]
  classesList <- unique(xl[ ,n+1])
  clLength <- length(classesList)
  counts <- rep(0,clLength)
  for (i in 1:l)
  {
    counts[which(orderedXl[i,n+1]==classesList)] <- 
      counts[which(orderedXl[i,n+1]==classesList)] +
      charge[i]*kerne(distances[i, 2]/h[i],ker.type[3])
  }
  as.character(classesList[which.max(counts)])
}
optimizedCharge <- function(xl,eps,h)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  charge <- rep(0,l)
  while(TRUE)
  {
    mistakes <- 0
    for(i in 1:l)
    {
      if(potentialFunc(xl,xl[i, 1:n],h,charge[i])!=xl[i,n+1])
      {
        mistakes <- mistakes+1
        charge[i] <- charge[i]+1
      }
    }
    if(mistakes/l<eps) break
  }
  return(charge)
}
potentialFunc(xl,c(6.5,2),rep(1,150),rep(10,150))
vec <- optimizedCharge(xl,0.1,1)
vec
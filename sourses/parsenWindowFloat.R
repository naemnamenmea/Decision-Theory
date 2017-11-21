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
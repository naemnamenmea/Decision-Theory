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
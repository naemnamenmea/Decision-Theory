euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
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
Margin <- function(xl,i)
{
  # l <- dim(xl)[1]
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

a <- 3
b <- 4
xl <- iris[, c(a,b,5)]
l <- dim(xl)[1]
Marga <- NA
for(i in 1:l)
{
  Marga[i] <- Margin(xl,i)
}
Marga <- Marga[order(Marga)]
plot(x=1, type="n", xlab="x_i", ylab="Margin",
     xlim=c(0, l), ylim=c(-10, 30), xaxs="i", yaxs="i")
lines(1:l,Marga,lwd=2)
abline(h=0)
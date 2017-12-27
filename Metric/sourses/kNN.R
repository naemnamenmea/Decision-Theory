kNN <- function(xl, z, k, metricFunction = euclideanDistance)
{
  n <- dim(xl)[2] - 1
  distances <- apply(xl[ ,1:n], 1, metricFunction, z) 
  sortedDist <-  sort(distances, index.return=TRUE)
  classes <- xl[sortedDist$ix[1:k], n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}
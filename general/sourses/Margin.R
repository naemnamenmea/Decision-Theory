Margin <- function(xl,i) #отступ i-ого объекта выборки xl
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
  
#  return(counts[which(xl[i,n+1]==classesList)] -
#           max(counts[-which(xl[i,n+1]==classesList)]))  

  margY <- counts[which(xl[i,n+1]==classesList)]
  margNY <- max(counts[-which(xl[i,n+1]==classesList)]) 
  if(margY<margNY) return(margY-margNY)
  else return(margY/margNY)
}

# Margin <- function(xl,i)
# {
# l <- dim(xl)[1]
# n <- dim(xl)[2]-1
# distances <- matrix(NA, l, l)
# elIndex <- matrix(NA, l, l)
# for (j in 1:l)
# {
#   for(g in 1:l)
#   {
#     distances[j,g] <- metricFunction(xl[j, 1:n], xl[g, 1:n])
# 	elIndex[j,g] <- g
#   }
# }
#   classesList <- unique(xl[,n+1])
#   counts <- rep(0,length(classesList))
# for (j in 1:l)
# {
#   for(g in 1:l)
#   {
#     distances[j,g] <- metricFunction(xl[j, 1:n], xl[g, 1:n])
# 	elIndex[j,g] <- g
#   }
# }
#   orderedXl <- xl[order(distances[-i, 2]), ]
#   for(j in  1:(l-1))
#   {
#     classNumber <- vvz[orderedXl[j,n+1]]
#     counts[classNumber] <- counts[classNumber]+w(j,k=l)
#   }
#   return(counts[which(xl[i,n+1]==classesList)] - 
#     max(counts[-which(xl[i,n+1]==classesList)]))
# }
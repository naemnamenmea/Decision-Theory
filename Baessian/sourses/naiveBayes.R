naiveBayes <- function(xl, x, lambda, h)
{
  aprior_prob <- c()
  apost_prob <- c()
  prob <- c()
  classXl <- levels(xl$Species)     
  n <- dim(xl)[2]-1
  
  for(y in 1:length(classXl))
  {
    sub_xl <- xl[xl$Species == classXl[y], ]
    m <- nrow(sub_xl)
    aprior_prob[y] <- m / dim(xl)[1]
    
    sum <- 0
    for(j in 1:m)
    {
      apost_value <- 1
      for(i in 1:n)
      {
        kerneRes <- kerne((x[i] - sub_xl[j,i]) / h[i], ker.type[7]) / h[i]
        apost_value <- apost_value * kerneRes
      }
      sum <- sum + apost_value
    }
    apost_prob[y] <- sum / m
    
    prob[y] <- log(lambda[y]*aprior_prob[y]) + log(apost_prob[y]) 
  }   
  # if(max(prob)==0) return("") // check prob list for 0 val
  # for parabolic ker f.ex.
  return(levels(xl$Species)[match(max(prob), prob)])
}
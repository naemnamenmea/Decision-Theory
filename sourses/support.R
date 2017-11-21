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
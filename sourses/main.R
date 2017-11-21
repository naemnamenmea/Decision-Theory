ker.type <- c("uniform",    "triangle",  "parabolic", "biquadratic",
              "tricaprate", "tricubic",  "gaussian",  "cosine",
              "logistics",  "sigmoidal", "silvermans")

euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}
sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1] # ~ dim(xl[?,const]) = 150
  n <- dim(xl)[2] - 1 # ~ dim(xl[const,?]) - 1 = 2
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
# parsenWindowFloat(xl, c(1.5,0.2), 3) ????!?!?!?!?!?!?
a <- 3
b <- 4
xl <- iris[, c(a,b,5)]
#
# 
# res1 <- 1
# for(i in 1:40)
# {
#   res1[i] <- loo(i,xl,parsenWindowFloat)
# }
# res1
# 
# par(xpd=FALSE,oma=c(0,0,0,10)) #oma=c(0,0,0,10)
# plot(x=1, type="n", xlab="h", ylab="LOO",
#      xlim=c(0, 41), ylim=c(0, 2), xaxs="i", yaxs="i")
# points(1:40,res2[1:40])
# legend(x=41, y=2*2/3, legend=c("a: parsenWindowFix", "Ядро: Параболическое", "h_opt = 1", "LOO(1) = 0.04"),
#        cex=1.2, text.font=6, xpd=NA,bty="n") #cex=1.5

# res <- min(loo(1:40,xl))
# warnings()

colors <- c("setosa" ="red", "versicolor" ="green3", "virginica" ="blue")
fontcolors <- c("versicolor" ="green3", "virginica" ="blue", "setosa" ="red")
# Создаем пустой холст
margin <- 0.3
xright <- max(xl[, 1]) + margin
xleft <-max(min(xl[, 1]) - margin,0)
ytop <- max(xl[, 2]) + margin
ybot <- max(min(xl[, 2]) - margin,0)
par(xpd=FALSE,oma=c(0,0,0,10))
plot(main="parsenWindowFloat, Ker = Parabolic, для k=32", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
     xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")
# # Классификация поля объектов
# exTime <- system.time(
# for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
#   for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
#     class <- parsenWindowFloat(xl, c(j,i), 32)
#     if(class!="") points(j, i, pch =1, col =fontcolors[class]) # cex=6, lwd=4, asp=1
#   }
# })
# # Рисуем выборку
# points(iris[, c(a,b)], pch =21, bg = colors[iris$Species], col ="black")
# legend(x=xright, y=ytop*2/3, legend=names(colors), #"topleft", inset=c(1.1,0.2)
#        cex=1.5, pch = c(21,21,21),
#        text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]],colors[names(colors)[3]]),
#        xpd=NA,horiz=FALSE, bty="n", bg="white")

veC=c("grey","black","brown","red")
par(xpd=FALSE,oma=c(0,0,0,10))
plot(x=1, type="n", xlab="parametr", ylab="LOO",
     xlim=c(0, 41), ylim=c(0, 0.15), xaxs="i", yaxs="i")

lines(1:40,res4,col=veC[1],lwd=3)
lines(1:40,res3,col=veC[2],lwd=3)
lines(1:40,res1,col=veC[3],lwd=3)
lines(1:40,res2,col=veC[4],lwd=3)

legend(x=41, y=0.15*2/3, lty=1, lwd=3, col=veC, legend=c("kNN", "kwNN",
      "parsenWindowFix", "parsenWindowFloat"),
       cex=1.2, text.font=6, xpd=NA,bty="n",pch=20) #cex=1.5
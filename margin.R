euclideanDistance <- function(u, v) #ф-ция расстояния для 2х точек
{
  sqrt(sum((u - v)^2))
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
  margY <- counts[which(xl[i,n+1]==classesList)]
  margNY <- max(counts[-which(xl[i,n+1]==classesList)]) 
  if(margY<margNY) return(margY-margNY)
  else return(margY/margNY)
}
STOLP <- function(xl, delta, eps, metricFunction = euclideanDistance)
{
  n <- dim(xl)[2]-1 #кол-во признаков
  l <- dim(xl)[1] #кол-во объектов выборки
  classesList <- unique(xl[ ,n+1]) #список уникальных классов
  len <- length(classesList)
  vvz <- NULL #каждому классу ставится в соответствие номер
  for(i in 1:len) { vvz[classesList[i]] <- i }
  mar <- rep(NA,len) #список отступов для элементов выборки
  j <- 0
  for(i in 1:l) #отсеиваем выбросы
  {
    if(Margin(xl,i)>=delta)
    {
      j <- j+1
      xl[j, ] <- xl[i, ]
    }
  }
  l <- j
  for(i in 1:l)
  {
    tmp <- Margin(xl[1:l, ],i)
    tmp2 <- vvz[xl[i,n+1]] #номер класса i-го объекта выборки
    #перенумеровываем выборку так, что
    #первые len эл. выборки имеют макс. отступы для каждого класса
    if(is.na(mar[tmp2]) | mar[tmp2]<tmp) #если уже был рассмотрен хотя бы 1 объект
    {#класса с номером tmp2, то...
      #ищем объект с максимальным отступом для этого класса
        mar[tmp2] <- tmp #новый макс. отступ для tmp2 класса
        tmp <- xl[tmp2, ]
        xl[tmp2, ] <- xl[i, ]
        xl[i, ] <- tmp
    }
  }
  delta <- 0
  while(len!=l)
  {
    j <- 0
    mistakeObj <- NA
    for(i in (len+1):l)
    {
      if(kNN(xl[1:len, ],xl[i,1:n],6)!=xl[i,n+1])
      {
        j <- j+1
        mistakeObj[j] <- i
      }
    }
    if(j<eps) break
    mar <- rep(NA,length(classesList))
    index <- rep(NA,length(classesList))
    for(i in mistakeObj)
    {
      tmp3 <- xl[1:len, ]
      tmp3[len+1, ] <- xl[i, ]
      tmp <- Margin(tmp3[1:(len+1), ],len+1)
      tmp2 <- vvz[xl[i,n+1]]
      if(tmp<delta)
      {
        if(is.na(mar[tmp2]) | mar[tmp2]>tmp)
        {
          mar[tmp2] <- tmp
          index[tmp2] <- i
        }
      }
    }
    if(all(is.na(index))) { delta <- delta+1 }
    else { delta <- 0 }
    for(i in index)
    {
      if(!is.na(i))
      {
        len <- len+1
        tmp <- xl[len, ]
        xl[len, ] <- xl[i, ]
        xl[i, ] <- tmp
      }
    }
  }
  xl[1:len, ]
}

a <- 3
b <- 4
xl <- iris[, c(a,b,5)]
l <- dim(xl)[1]
# Marga <- NA
# for(i in 1:l)
# {
#   Marga[i] <- Margin(xl,i)
# }
# Marga <- Marga[order(Marga)]
# plot(x=1, type="n", xlab="x_i", ylab="Margin",
#      xlim=c(0, l), ylim=c(-10, 30), xaxs="i", yaxs="i")
# lines(1:l,Marga,lwd=2)
# abline(h=0)

time2 <- system.time( res2 <- STOLP(xl,0,4) )
time1; time2
res; res2

margin <- 0.3
xright <- max(xl[, 1]) + margin
xleft <-max(min(xl[, 1]) - margin,0)
ytop <- max(xl[, 2]) + margin
ybot <- max(min(xl[, 2]) - margin,0)
par(xpd=FALSE,oma=c(0,0,0,10))
plot(main="STOLP: delta=0, mistakeObj=4", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
     xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")
points(iris[, c(a,b)], pch =1, col = colors[iris$Species])
points(res2[, c(1,2)], pch =21, bg = colors[res2$Species], col ="black",cex=1.5)
legend(x=xright, y=ytop*3/4, legend=names(colors), #"topleft", inset=c(1.1,0.2)
       cex=1.5, pch = c(21,21,21),
       text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]],colors[names(colors)[3]]),
       xpd=NA,horiz=FALSE, bty="n", bg="white")
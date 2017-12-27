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
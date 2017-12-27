# Краткий обзор алгоритмов классификации, их оптимизации и анализа
___
Основная задача, которая будет здесь рассматриваться - это классифицировать объект (т.е. определить к какому классу он принадлежит) по имеющимся данным (выборке), и сделать это максимально верно.

Для начала рассмотрим алгоритмы классификации, их отличительные особенности, графические иллюстрации и то, что я допишу, надеюсь, ...
Все рассматриваемые алгоритмы метрические, т.е. опираются на некоторую функцию расстояния (в нашем случае Евклидово). Введем следующие обозначения:

 `xl` - выборка объектов 
 
 `u` - классифицируемый объект
 
 `p` - функция расстояния
 
 `Y` - класс
 
## kNN
___
Смысл алгоритма очень прост. Если в двух словах, то мы говорим, что объект `u` относится к классу `Yi`, если среди k ближайших объектов со схожими свойствами больше объектов, принадлежащих к классу `Yi`. Степень близости каждого из `k` соседей не учитывается, важен сам факт соседства. Визуально это отображено на картинке ниже:

![kNN](/images/kNN.png)

```R
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
```

[Оптимальный k для kNN](/images/kNN_kOpt.png).
Код лежит [тут](/sourses/kNN.R).
Подробнее про **_Метод ближайших соседей_** можно найти [здесь](www.machinelearning.ru/wiki/index.php?title=Метод_ближайшего_соседа).

## kwNN
___
Здесь уже вводится т.н. весовая фунция `w(i)`, которая в алгоритме kNN была просто константой множителем - 1. `w(i)` - убывающая функция. Тут мы опять нахожим `k` ближайших соседей, но теперь чем дальше сосед, тем меньший вклад он привносит в определении класса объекта `u`.

![kwNN](/images/kwNN.png)

```R
kwNN <- function(xl, z, k, metricFunction = euclideanDistance)
{
  n <- dim(xl)[2] - 1
  distances <- apply(xl[ ,1:n], 1, metricFunction, z) 
  sortedDist <-  sort(distances, index.return=TRUE)
  classes <- xl[sortedDist$ix[1:k], n + 1]
  classesList <- unique(classes)
  counts <- rep(0,length(classesList))
  for (i in 1:k)
  {
    counts[which(classes[i]==classesList)] =
      counts[which(classes[i]==classesList)] + w(i,k)
  }
  as.character(classesList[which.max(counts)])
}
```

[Оптимальный k для kwNN](/images/kwNN_kOpt.png).
Код лежит [тут](/sourses/kwNN.R).
Подробнее про **_Метод взвешенных ближайших соседей_** можно найти [здесь](http://www.machinelearning.ru/wiki/index.php?title=Метод_k_взвешенных_ближайших_соседей_(пример)).

![преимущество kNN над kwNN 1](/images/kNN_k11.png) ![преимущество kNN над kwNN 2](/images/kwNN_k11.png)

## Парзеновское окно
___
Представим что наш объект `u` является центром сферы, и мы расширили радиус сферы до значения `h` - ширины окна. Тогда чем больше объектов `Yi` класса окажется внутри этой сферы, тем вероятнее что объект `u` принадлежит классу `Yi`. По сути этот метод является двойственным к kNN (kwNN).

![Парзеновское окно](/images/parsenWindowFix.png)

```R
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
```

[Оптимальное h для Парзеновского окна](/images/parsenWindowFix_hOpt.png).
Код лежит [тут](/sourses/parsenWindowFix.R).
[Наглядная иллюстрация](/images/pasrenwindowexample.png).
Подробнее про **_метод Парзеновского окна_** можно найти [здесь](www.machinelearning.ru/wiki/index.php?title=Метод_парзеновского_окна).

## Парзеновское окно с переменной шириной
___
Здесь выбор параметра `h` зависит от `k`. Так что по большому счету это тот же kNN. Только тут вместо `w(i)`, используется `Ker(u)` - функция ядра, интеграл по которой = 1, и она также убывает на интервале `[0,oo]`.

![Парзеновское окно с переменной шириной](/images/parsenWindowFloat.png)

```R
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
```

[Оптимальное k для Парзеновского окна с переменной шириной](https://github.com/naemnamenmea/SMCS/blob/master/images/parsenWindowFloat_kOpt.png).
Код лежит [тут](/sourses/parsenWindowFloat.R).
Подробнее про **_метод Парзеновского окна с переменной шириной_** можно найти [здесь](machinelearning.ru/wiki/index.php?title=Метод_Парзеновского_окна_(пример)).

<img src="images/PWFix_gaussian.png" width=40%/> <img src="images/PWFix_triangle.png" width=52%/> 

## Метод потенциальных функций
___
Тут тоже все несложно. Мы проводим аналогию с физическими частицами (они имеют заряд и радиус действия этого заряда) и полагаем что каждый объект выборки `xl` имеет некий потенциал (заряд) и расстояние его действия. Каждый `i`-ый объект вносит в долю своего класса значение, которое равно `потенциал * ( степень действия/расстояние до объекта u )`.

![Метод ПФ](/images/potential.png)

```R
potentialFunc <- function(xl, x, h, gammaV){
  distances <- c()
  wght_to_class <- c()
  for(i in 1:nrow(xl)){
    distances[i] <- euclideanDistance(xl[i , 1:length(xl) - 1] , x)
    wght_to_class[i] <- kerne(distances[i] / h[i], ker.type[7]) * gammaV[i]  
  }
  
  potentional_wght <- data.frame(p_class <- xl$Species, wght_to_class)
  wght_max <- c( sum_setosa <- sum(potentional_wght[potentional_wght$p_class == "setosa" , 2]),
                 sum_versicolor <- sum(potentional_wght[potentional_wght$p_class == "versicolor" , 2]),
                 sum_virginica <- sum(potentional_wght[potentional_wght$p_class == "virginica" , 2]) )
  if(sum(wght_max) == 0){
    
    res <- ""
  }else{
    res <- levels(xl$Species)[match(max(wght_max), wght_max)]
  }
  return(res)
}

getBestGamma <- function(xl, h, gammaV, eps){
  i <- 1
  while(loo_potential(xl, h, gammaV) > eps){
    cur_point <- c(xl[i, 1], xl[i, 2])
    el_class <- xl[i , 3]
    el_check <- potentialFunc(xl, cur_point, h, gammaV)  
    if(el_class != el_check){
      gammaV[i] <-  gammaV[i] + 1
    }
    i <- i + 1
    
  }
  return(gammaV) 
}
```

Код лежит [тут](/sourses/potential.R).
Подробнее про **_метод потенциальных функций_** можно найти [здесь](www.machinelearning.ru/wiki/index.php?title=Метод_потенциальных_функций).

Полезно ознакомиться с таблицей ниже...

| Алгоритм                     | Оптимальный параметр |  LOO  |
| ---------------------------- |:--------------------:|:------:
| kNN                          | k = 6                |0.0(3) |
| kwNN                         | k = 4                |0.04   |
| Парзеновское окно (h=const)  | h = 1                |0.04   |
| Парзеновское окно h(k)       | h = 32               |0.0(3) |
| Потенциальные функции        | h = 1; g = 1         |0.05(3)|


### Сравнение алгоритмов:
![сравнение алгоритмов](/images/comparison.png)

## Margin (отступы)
___
Грубо говоря `Margin =` степень близости объекта `u` до своего класса `-` степень близости до ближайшего НЕ своего класса.
Посчитаем отступы всех элементов выборки `xl`, расположим объекты в порядке возрастания отступов и построим их график:

![margin](/images/margin.png)
Код лежит [тут](/sourses/Margin.R).
Подробнее про **_Отступы_** и их применение можно найти [здесь](ru.learnmachinelearning.wikia.com/wiki/Отступ_(для_классификатора)).

## STOLP
___
Этот метод (оптимизационный) позволяет сократить выборку `xl` выбросив из нее шумовые, неинформативные объекты, используя какой-то определенный алгоритм, например, `kNN` и функцию отступов `Margin`. Думаю не нужно объяснять почему он так хорош...

![STOLP](/images/STOLP.png)

```R
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
```

Код лежит [тут](/sourses/STOLP.R).
Подробнее про алгоритм **_STOLP_** можно найти [здесь](http://www.machinelearning.ru/wiki/index.php?title=Машинное_обучение_(курс_лекций%2C_К.В.Воронцов)).
[loo for kNN before STOLP](/images/loo_for_kNN_before_STOLP.png)
[loo for kNN after STOLP](/images/loo_for_kNN_after_STOLP.png)
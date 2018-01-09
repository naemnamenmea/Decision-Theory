drawMap <- function(algorithm, args, title) { #drawkNN
	margin <- 0.3
	xright <- max(xl[, 1]) + margin
	xleft <-max(min(xl[, 1]) - margin,0)
	ytop <- max(xl[, 2]) + margin
	ybot <- max(min(xl[, 2]) - margin,0)
	# par(xpd=FALSE,oma=c(0,0,0,10))
	# png(file = "image.jpg")
  plot(main=title, x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
       xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")

  for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
    for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
      class <- do.call(algorithm, c(list(c(j,i)), args))
      if(class!="") points(j, i, pch =1, col =fontcolors[class]) # cex=6, lwd=4, asp=1
    }
  }

  points(iris[, c(a,b)], pch =21, bg = colors[iris$Species], col ="black", asp=1)

  legend("bottomright", legend=names(colors), #x=xleft, y=ytop*2/3, inset=c(1.1,0.2)
         cex=1.2, pch = c(21,21,21),
         text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]],colors[names(colors)[3]]),
         xpd=NA,horiz=FALSE, bg="white", y.intersp=0.8) #xpd=TRUE, bty="y"
  # dev.off()
}

drawLOO <- function(algorithm, xl, paramRange, xlab, ylab) { #drawlookNN
  lp <- length(paramRange)
	kLOO <- matrix(NA, lp, 2)
	j <- 1
	for(i in paramRange)
	{
	  kLOO[j, ] <- c(i, loo(i,xl,algorithm))
	  j <- j + 1
	}
  optP <- kLOO[ which.min(kLOO[ ,2]),1 ]
  optLOO <- min(kLOO[ ,2])
	# par(xpd=FALSE,oma=c(0,0,0,0)) #oma=c(0,0,0,10)
	plot(main=paste0("p* = ",optP,"; LOO(kNN(p*)) = ", sprintf('%.4f', optLOO)), x=kLOO[ ,1], y=kLOO[ ,2], type="l", xlab=xlab, ylab=ylab, lwd=2,
		 xlim=c(0, max(kLOO[ ,1])+1), ylim=c(0, max(kLOO[ ,2])+0.1), xaxs="i", yaxs="i")
	points(optP, optLOO, cex=2, pch=19)
	return(kLOO)
}

drawmarga <- function() {
	Marga <- NA
	for(i in 1:l)
	{
	  Marga[i] <- Margin(xl,i)
	}
	Marga <- Marga[order(Marga)]
	plot(x=1, type="n", xlab="x_i", ylab="Margin",
		 xlim=c(0, l), ylim=c(-5, 5), xaxs="i", yaxs="i")
	lines(1:l,Marga,lwd=2)
	abline(h=0)
}

drawSTOLPkNN <- function() {
	res <- STOLP(xl,0,4)

	margin <- 0.3
	xright <- max(xl[, 1]) + margin
	xleft <-max(min(xl[, 1]) - margin,0)
	ytop <- max(xl[, 2]) + margin
	ybot <- max(min(xl[, 2]) - margin,0)
	par(xpd=FALSE,oma=c(0,0,0,10))
	plot(main="STOLP: delta=0, mistakeObj=4", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
		 xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")
	points(iris[, c(a,b)], pch =1, col = colors[iris$Species])
	points(res[, c(1,2)], pch =21, bg = colors[res$Species], col ="black",cex=1.5)
	legend(x=xright, y=ytop*3/4, legend=names(colors), #"topleft", inset=c(1.1,0.2)
		   cex=1.5, pch = c(21,21,21),
		   text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]],colors[names(colors)[3]]),
		   xpd=NA,horiz=FALSE, bty="n", bg="white")
}

drawLinear <- function(ObjectsCountOfEachClass=87)
{
  ## Моделируем обучающие данные
  library(MASS)
  Sigma1 <- matrix(c(2, 4, 1, 41), 2, 2)
  Sigma2 <- matrix(c(4, 1, 3, 41), 2, 2)
  xy1 <- mvrnorm(n=ObjectsCountOfEachClass, c(0, 0), Sigma1)
  xy2 <- mvrnorm(n=ObjectsCountOfEachClass, c(10, -10), Sigma2)
  xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))
  colors <- c("blue", "white", "yellow")
  ## Нормализация данных
  xlNorm <- trainingSampleNormalization(xl)
  xlNorm <- trainingSamplePrepare(xlNorm)
  ## Отображение данных
	plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1, xlab = "x1", ylab = "x2", main = "Linear classifierss")
	
	## ADALINE
	w <- sg.ADALINE(xlNorm)
	abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "brown")
  
	## Правило Хебба
	w <- sg.Hebb(xlNorm)
	abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "green3")
	
	## Логистическая регрессия
	w <- sg.LogRegression(xlNorm)
	abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "red")
		   
	legend("bottomright", c("ADALINE", "Hebb's rule",
	"Logistic regression"), pch = c(15,15,15), 
	col = c("brown", "green3", "red"))
}

drawPlugIn <- function()
{
	## Количество объектов в каждом классе
	ObjectsCountOfEachClass <- 11
	## Подключаем библиотеку MASS для генерации многомерного нормального распределения
	library(MASS)
	## Генерируем тестовые данные
	Sigma1 <- matrix(c(10, 0, 0, 1), 2, 2)
	Sigma2 <- matrix(c(1, 0, 0, 5), 2, 2)
	Mu1 <- c(6, 0)
	Mu2 <- c(-2, 0)
	xy1 <- mvrnorm(n=ObjectsCountOfEachClass, Mu1, Sigma1)
	xy2 <- mvrnorm(n=ObjectsCountOfEachClass, Mu2, Sigma2)
	## Собираем два класса в одну выборку
	xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))
	## Рисуем обучающую выборку
	colors <- c(rgb(0/255, 162/255, 232/255), rgb(0/255, 200/255, 0/255))
	plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1, xlab = "sigma_1", ylab = "sigma_2")
	## Оценивание
	objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]
	objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]
	mu1 <- estimateMu(objectsOfFirstClass)
	mu2 <- estimateMu(objectsOfSecondClass)
	sigma1 <- estimateCovarianceMatrix(objectsOfFirstClass, mu1)
	sigma2 <- estimateCovarianceMatrix(objectsOfSecondClass, mu2)
	coeffs <- getPlugInDiskriminantCoeffs(mu1, sigma1, mu2, sigma2)
	## Рисуем дискриминантую функцию – красная линия
	x <- y <- seq(-10, 20, len=100)
	z <- outer(x, y, function(x, y) coeffs["x^2"]*x^2 +
				 coeffs["xy"]*x*y
			   + coeffs["y^2"]*y^2 + coeffs["x"]*x
			   + coeffs["y"]*y + coeffs["1"])
	contour(x, y, z, levels=0, drawlabels=FALSE, lwd = 3, col = "red", add = TRUE)
	# par(mfrow=c(2,2))
}

drawLDF <- function ()
{
	## Количество объектов в каждом классе
	ObjectsCountOfEachClass <- 11
	## Подключаем библиотеку MASS для генерации многомерного нормального распределения
	library(MASS)
	## Генерируем тестовые данные
	Sigma1 <- matrix(c(2, 0, 0, 2), 2, 2)
	Sigma2 <- matrix(c(2, 0, 0, 2), 2, 2)
	Mu1 <- c(1, 0)
	Mu2 <- c(15, 0)
	xy1 <- mvrnorm(n=ObjectsCountOfEachClass, Mu1, Sigma1)
	xy2 <- mvrnorm(n=ObjectsCountOfEachClass, Mu2, Sigma2)

	## Собираем два класса в одну выборку
	xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))

	## Рисуем обучающую выборку
	colors <- c(rgb(0/255, 162/255, 232/255), rgb(0/255, 200/255, 0/255))
	plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1)

	## Оценивание
	objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]
	objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]
	mu1 <- estimateMu(objectsOfFirstClass)
	mu2 <- estimateMu(objectsOfSecondClass)
	Sigma <- estimateFisherCovarianceMatrix(objectsOfFirstClass,
	objectsOfSecondClass, mu1, mu2)

	## Получаем коэффициенты ЛДФ
	inverseSigma <- solve(Sigma)
	alpha <- inverseSigma %*% t(mu1 - mu2)
	mu_st <- (mu1 + mu2) / 2
	beta <- mu_st %*% alpha

	## Рисуем ЛДФ
	abline(beta / alpha[2,1], -alpha[1,1]/alpha[2,1], col = "red", lwd = 3)
}
## Логарифмическая функция потерь
lossLog <- function(x)
{
	return (log2(1 + exp(-x)))
}
## Сигмоидная функция
lossSigmoid <- function(z)
{
	return (1 / (1 + exp(-z)))
}
## Стохастический градиент для логистической регрессии
sg.LogRegression <- function(xl)
{
	l <- dim(xl)[1]
	n <- dim(xl)[2] - 1
	w <- c(1/2, 1/2, 1/2)
	iterCount <- 0
	lambda <- 1/l
	## initialize Q
	Q <- 0
	for (i in 1:l)
	{
		## calculate the scalar product <w,x>
		wx <- sum(w * xl[i, 1:n])
		## calculate a margin
		margin <- wx * xl[i, n + 1]
		Q <- Q + lossSigmoid(margin)
	}
	repeat
	{
		# select the random index from the error objects errorIndexes
		i <- sample(1:l, 1)
		iterCount <- iterCount + 1
		# i <- sample(1:l, 1)
		xi <- xl[i, 1:n]
		yi <- xl[i, n + 1]
		## calculate the scalar product <w,xi>
		wx <- sum(w * xi)
		## make a gradient step
		margin <- wx * yi
		ex <- lossSigmoid(margin)
		eta <- 0.3#1 / iterCount
		w <- w + eta * xi * yi * lossSigmoid(-wx * yi)
		## Calculate a new Q
		Qprev <- Q
		Q <- (1 - lambda) * Q + lambda * ex
		if (abs(Qprev - Q) / abs(max(Qprev, Q)) < 1e-5)
		break
	}
	return (w)
}
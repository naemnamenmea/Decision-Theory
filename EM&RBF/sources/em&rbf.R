#http://www.di.fc.ul.pt/~jpn/r/rbf/rbf.html
#https://www.r-bloggers.com/k-means-clustering-in-r/
#https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html#mclustda
#http://www.di.fc.ul.pt/~jpn/r/EM/EM.html#using-rs-mclust-for-classification
#https://cran.r-project.org/web/packages/sBIC/vignettes/GaussianMixtures.pdf

######################################################################

rbf.gauss <- function(gamma=1.0) {
  
  function(x) {
    exp(-gamma * norm(as.matrix(x),"F")^2 )
  }
}

rbf <- function(X, Y, K=10, gamma=1.0) {
  N     <- dim(X)[1] # number of observations
  ncols <- dim(X)[2] # number of variables
  
  repeat {
    km <- kmeans(X, K)  # let's cluster K centers out of the dataset
    if (min(km$size)>0) # only accept if there are no empty clusters
      break
  }
  
  mus <- km$centers # the clusters points
  
  Phi <- matrix(rep(NA,(K+1)*N), ncol=K+1)
  for (lin in 1:N) {
    Phi[lin,1] <- 1    # bias column
    for (col in 1:K) {
      Phi[lin,col+1] <- exp( -gamma * norm(as.matrix(X[lin,]-mus[col,]),"F")^2 )
    }
  }
  
  w <- pseudoinverse(t(Phi) %*% Phi) %*% t(Phi) %*% Y  # find RBF weights
  
  list(weights=w, centers=mus, gamma=gamma)  # return the rbf model
}

rbf.predict <- function(model, X, classification=FALSE) {
  
  gamma   <- model$gamma
  centers <- model$centers
  w       <- model$weights
  N       <- dim(X)[1]    # number of observations
  
  pred <- rep(w[1],N)  # we need to init to a value, so let's start with the bias
  
  for (j in 1:N) {  
    # find prediction for point xj
    for (k in 1:length(centers[,1])) {
      # the weight for center[k] is given by w[k+1] (because w[1] is the bias)
      pred[j] <- pred[j] + w[k+1] * exp( -gamma * norm(as.matrix(X[j,]-centers[k,]),"F")^2 )
    }
  }
  
  if (classification) {
    pred <- unlist(lapply(pred, sign))
  }
  pred
}

######################################################################

# install.packages("mclust")
# install.packages("corpcor")
# install.packages("RSNNS")
require(mclust) #Mclust
require(MASS)
require(corpcor) #pseudoinverse
require(RSNNS)

data(iris)

mc <- Mclust(iris[,1:4],G=3) # 3 clusters
plot(mc, what=c("classification"), dimens=c(1,3)) # using 1st and 3rd column of the iris dataset
mc$parameters$mean # centers of clusters #eq to t(irisCluster$centers)
mc$G # number of clusters

irisCluster <- kmeans(iris[,1:4],3)

######################################################################

target <- function(x1, x2) {
  2*(x2 - x1 + .25*sin(pi*x1) >= 0)-1
}

N <- 100
set.seed(1)
X <- data.frame(x1=runif(N, min=-1, max=1),
                x2=runif(N, min=-1, max=1))
Y <- target(X$x1, X$x2)
plot(X$x1, X$x2, col=Y+3)

rbf.model <- rbf(X, Y) # using default values for K and gamma

######################################################################

N.test <- 200
X.out <- data.frame(x1=runif(N.test, min=-1, max=1),
                    x2=runif(N.test, min=-1, max=1))
Y.out <- target(X.out$x1, X.out$x2)

rbf.pred <- rbf.predict(rbf.model, X.out, classification=TRUE)
binary.error <- sum(rbf.pred != Y.out)/N.test
binary.error

plot(X.out$x1, X.out$x2, col=Y.out+3, pch=0)
points(X.out$x1, X.out$x2, col=rbf.pred+3, pch=3)
points(rbf.model$centers, col="black", pch=19) # draw the model centers
legend("topleft",c("true value","predicted"),pch=c(0,3),bg="white")

######################################################################

rbfn.model <- RSNNS::rbf(as.matrix(X,ncol=2), 
                         as.matrix(Y), 
                         size=30,    # number of centers, ie, number of neurons in hidden layer
                         maxit=1000, # max number of iterations to learn
                         linOut=TRUE) # linear activation function (otherwise logistic)

rbf.network.pred <- sign( predict(rbfn.model, X.out) ) # apply sign, since this is a classification eg
binary.error <- sum(rbf.network.pred != Y.out)/N.test
binary.error     

plot(X.out$x1, X.out$x2, col=Y.out+3, pch=0)
points(X.out$x1, X.out$x2, col=rbf.network.pred+3, pch=3)
legend("topleft",c("true value","predicted"),pch=c(0,3),bg="white")
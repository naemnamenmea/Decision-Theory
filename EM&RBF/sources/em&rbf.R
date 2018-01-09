#http://www.di.fc.ul.pt/~jpn/r/rbf/rbf.html
#https://www.r-bloggers.com/k-means-clustering-in-r/
#https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html#mclustda
#http://www.di.fc.ul.pt/~jpn/r/EM/EM.html#using-rs-mclust-for-classification
#https://cran.r-project.org/web/packages/sBIC/vignettes/GaussianMixtures.pdf
#http://rstudio-pubs-static.s3.amazonaws.com/154174_78c021bc71ab42f8add0b2966938a3b8.html

#-------------------------------- Expectation  -----------------------------

expectation <- function(sample,p,a,b)
{
  p_expectation <- (p*dbinom(sample,1,a)) / ( p*dbinom(sample,1,a) + (1-p)*dbinom(sample,1,b) )
  return(p_expectation)
}


#-------------------------------- Maximization   --------------------------------

maximization <- function(sample,epart){
  
  # estimate p
  
  p_temp <- mean(epart)
  
  # estimate a and b
  
  a_temp <- sum(sample*epart) / sum(epart)
  b_temp <- sum(sample*(1-epart)) / sum(1-epart)
  
  list(p_temp,a_temp,b_temp)   
}



#---------------------------- Expectation Maximization Algorithm  -------------------------

EM <- function(sample,p_inits,a_inits,b_inits,maxit=1000,tol=1e-6)
{
  # Estimation of parameter(Initial)
  flag <- 0
  p_cur <- p_inits; a_cur <- a_inits; b_cur <- b_inits
  
  # Iterate between expectation and maximization parts
  
  for(i in 1:maxit){
    cur <- c(p_cur,a_cur,b_cur)
    new <- maximization(sample,expectation(sample, p_cur, a_cur, b_cur))
    p_new <- new[[1]]; a_new <- new[[2]]; b_new <- new[[3]]
    new_step <- c(p_new,a_new,b_new)
    
    # Stop iteration if the difference between the current and new estimates is less than a tolerance level
    if( all(abs(cur - new_step) < tol) ){ flag <- 1; break}
    
    
    # Otherwise continue iteration
    p_cur <- p_new; a_cur <- a_new; b_cur <- b_new
  }
  if(!flag) warning("Didn't converge\n")
  
  list(p_cur, a_cur, b_cur)
}



#------------------------------ Calculating Information matrix ----------------------------

Info.Mat.function <- function(sample, p.est, a.est, b.est){
  expectation.est <- expectation(sample,p.est, a.est, b.est)
  info.mat <- matrix(rep(0,9),3,3)
  info.mat[1,1] <- - sum(expectation.est)/(p.est^2)  - sum((1-expectation.est))/((1-p.est)^2) 
  info.mat[2,2] <- - sum(expectation.est*sample)/(a.est^2) - sum(expectation.est*(1-sample))/((1-a.est)^2)
  info.mat[3,3] <- - sum((1-expectation.est)*sample)/(b.est^2) - sum((1-expectation.est)*(1-sample))/((1-b.est)^2)
  return(-info.mat)
}


#------------------ Now Generate sample data --------------------------------

n <- 10000
p_true <- 0.85 # prob of using first coin
a_true <-  0.50 # the first coin has P(heads) = 0.50
b_true <-  0.70 # the second coin has P(heads) = 0.70
true <- c(p_true,a_true,b_true)
u <- ifelse(runif(n)<p_true, rbinom(n,1,a_true),rbinom(n,1,b_true))


# Set parameter estimates
p_init = 0.70; a_init = 0.70; b_init = 0.60


#--------------------Return EM Algorithm function and calculate Confidence Interval-----------------------------

output <- EM(u,p_init,a_init,b_init)


# Confidence Intervals
sd.out <- sqrt(diag(solve(Info.Mat.function(u,output[[1]],output[[2]],output[[3]]))))
data.frame("Truth" = true, "EM Estimate" = unlist(output), "Lower CI" = unlist(output) - qnorm(.975)*sd.out, "Upper CI" = unlist(output) + qnorm(.975)*sd.out)

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
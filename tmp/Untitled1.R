drawMap(kNN,list(xl=xl,k=6),"kNN")

eps <- 0.5
# gammaV <- getBestGamma(xl,h,eps)
h <- c(2,2,2)
gammaV <- c(1.2,1.5,1.7)

nxl <- xl[c(20,87,144), ]
drawMap(potentialFunc,list(xl=nxl,h=h,gammaV=gammaV),title = "Potential function, h=(2,2,2), p=(1.2,1.5,1.7)")
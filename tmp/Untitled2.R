apply_some_function <- function(data, function_name) { 
  FUN <- match.fun(function_name) 
  FUN(data) 
} 


f = function(x, f) {
  eval(as.call(list(as.name(f), x))) 
}
f(1:10, "mean") 
f(1:10, "max") 



par(mfcol = c(1,1))
drawNaiveBayes(c(1,1,1),c(1,1))
potentialFunc(xl,c(1,1),1,tmp)
tmp <- optimizedCharge(xl,4,1)
drawPotential()
drawparsenWindowFix()

res <- loo_potential(xl,rep(1,nrow(xl)),rep(1,nrow(xl)))
res

getBestGamma(xl,rep(1,nrow(xl)),rep(1,nrow(xl)),0.01)
potentialFunc(xl,c(3,4),rep(1,nrow(xl)),rep(1,nrow(xl)))

xl <- trainingSampleNormalization(xl)
xl <- trainingSamplePrepare(xl)
w <- sg.ADALINE(xl)
warnings()

a <- 3
b <- 4
xl <- iris[ ,c(a,b,5)]

library(shiny)
runApp("shinyApp")
shinyApp(ui = ui,server = server)

margin <- 0.3
xright <- max(xl[, 1]) + margin
xleft <-max(min(xl[, 1]) - margin,0)
ytop <- max(xl[, 2]) + margin
ybot <- max(min(xl[, 2]) - margin,0)
par(xpd=FALSE,oma=c(0,0,0,10))

plot(main="naiveBayes", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
     xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")
points(xl[, c(1,2)], pch =21, bg = colors[xl$Species], col ="black", asp=1)
margin <- 0.3
xright <- max(xl[, 1]) + margin
xleft <-max(min(xl[, 1]) - margin,0)
ytop <- max(xl[, 2]) + margin
ybot <- max(min(xl[, 2]) - margin,0)
par(xpd=FALSE,oma=c(0,0,0,10))

drawkNN <- function() {
  plot(main="title", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
       xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")

  for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
    for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
      class <- kNN(xl, c(j,i), 50)
      if(class!="") points(j, i, pch =1, col =fontcolors[class]) # cex=6, lwd=4, asp=1
    }
  }

  points(iris[, c(a,b)], pch =21, bg = colors[iris$Species], col ="black", asp=1)
  
  legend(x=xright, y=ytop*2/3, legend=names(colors), #"topleft", inset=c(1.1,0.2)
         cex=1.5, pch = c(21,21,21),
         text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]],colors[names(colors)[3]]),
         xpd=NA,horiz=FALSE, bty="n", bg="white") #xpd=TRUE, bty="y"
}

drawkNN()
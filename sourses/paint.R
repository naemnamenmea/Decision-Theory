drawa <- function() {
	drawkNN()
	#drawkwNN()
	#drawparsenWindowFix()
	#drawparsenWindowFloat()
}

drawloo <- function() {
	drawlookNN()
	#drawlookwNN()
	#drawlooparsenWindowFix()
	#drawlooparsenWindowFloat()
}

drawkNN <- function() {
	margin <- 0.3
	xright <- max(xl[, 1]) + margin
	xleft <-max(min(xl[, 1]) - margin,0)
	ytop <- max(xl[, 2]) + margin
	ybot <- max(min(xl[, 2]) - margin,0)
	par(xpd=FALSE,oma=c(0,0,0,10))

  plot(main="kNN, k=6", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
       xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")

  for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
    for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
      class <- kNN(xl, c(j,i), 6)
      if(class!="") points(j, i, pch =1, col =fontcolors[class]) # cex=6, lwd=4, asp=1
    }
  }

  points(iris[, c(a,b)], pch =21, bg = colors[iris$Species], col ="black", asp=1)
  
  legend(x=xright, y=ytop*2/3, legend=names(colors), #"topleft", inset=c(1.1,0.2)
         cex=1.5, pch = c(21,21,21),
         text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]],colors[names(colors)[3]]),
         xpd=NA,horiz=FALSE, bty="n", bg="white") #xpd=TRUE, bty="y"
}

drawkwNN <- function() {
	margin <- 0.3
	xright <- max(xl[, 1]) + margin
	xleft <-max(min(xl[, 1]) - margin,0)
	ytop <- max(xl[, 2]) + margin
	ybot <- max(min(xl[, 2]) - margin,0)
	par(xpd=FALSE,oma=c(0,0,0,10))

  plot(main="kwNN, k=4", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
       xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")

  for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
    for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
      class <- kwNN(xl, c(j,i), 4)
      if(class!="") points(j, i, pch =1, col =fontcolors[class]) # cex=6, lwd=4, asp=1
    }
  }

  points(iris[, c(a,b)], pch =21, bg = colors[iris$Species], col ="black", asp=1)
  
  legend(x=xright, y=ytop*2/3, legend=names(colors), #"topleft", inset=c(1.1,0.2)
         cex=1.5, pch = c(21,21,21),
         text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]],colors[names(colors)[3]]),
         xpd=NA,horiz=FALSE, bty="n", bg="white") #xpd=TRUE, bty="y"
}

drawparsenWindowFix <- function() {
	margin <- 0.3
	xright <- max(xl[, 1]) + margin
	xleft <-max(min(xl[, 1]) - margin,0)
	ytop <- max(xl[, 2]) + margin
	ybot <- max(min(xl[, 2]) - margin,0)
	par(xpd=FALSE,oma=c(0,0,0,10))

  plot(main="parsenWindowFix, h=1", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
       xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")

  for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
    for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
      class <- parsenWindowFix(xl, c(j,i), 1)
      if(class!="") points(j, i, pch =1, col =fontcolors[class]) # cex=6, lwd=4, asp=1
    }
  }

  points(iris[, c(a,b)], pch =21, bg = colors[iris$Species], col ="black", asp=1)
  
  legend(x=xright, y=ytop*2/3, legend=names(colors), #"topleft", inset=c(1.1,0.2)
         cex=1.5, pch = c(21,21,21),
         text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]],colors[names(colors)[3]]),
         xpd=NA,horiz=FALSE, bty="n", bg="white") #xpd=TRUE, bty="y"
}

drawparsenWindowFloat <- function() {
	margin <- 0.3
	xright <- max(xl[, 1]) + margin
	xleft <-max(min(xl[, 1]) - margin,0)
	ytop <- max(xl[, 2]) + margin
	ybot <- max(min(xl[, 2]) - margin,0)
	par(xpd=FALSE,oma=c(0,0,0,10))

  plot(main="parsenWindowFloat, h(k), где k=32", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
       xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")

  for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
    for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
      class <- parsenWindowFloat(xl, c(j,i), 32)
      if(class!="") points(j, i, pch =1, col =fontcolors[class]) # cex=6, lwd=4, asp=1
    }
  }

  points(iris[, c(a,b)], pch =21, bg = colors[iris$Species], col ="black", asp=1)
  
  legend(x=xright, y=ytop*2/3, legend=names(colors), #"topleft", inset=c(1.1,0.2)
         cex=1.5, pch = c(21,21,21),
         text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]],colors[names(colors)[3]]),
         xpd=NA,horiz=FALSE, bty="n", bg="white") #xpd=TRUE, bty="y"
}

drawlookNN <- function() {
	res1 <- 1
	for(i in 1:40)
	{
	  res1[i] <- loo(i,xl,kNN)
	}
	res1

	par(xpd=FALSE,oma=c(0,0,0,10)) #oma=c(0,0,0,10)
	plot(x=1, type="n", xlab="h", ylab="LOO for kNN",
		 xlim=c(0, 41), ylim=c(0, 0.1), xaxs="i", yaxs="i")
	points(1:40,res1[1:40])
}

drawlookwNN <- function() {
	res1 <- 1
	for(i in 1:40)
	{
	  res1[i] <- loo(i,xl,kwNN)
	}
	res1

	par(xpd=FALSE,oma=c(0,0,0,10)) #oma=c(0,0,0,10)
	plot(x=1, type="n", xlab="h", ylab="LOO for kwNN",
		 xlim=c(0, 41), ylim=c(0, 0.1), xaxs="i", yaxs="i")
	points(1:40,res1[1:40])
}

drawloodrawparsenWindowFix <- function() {
	res1 <- 1
	for(i in 1:40)
	{
	  res1[i] <- loo(i,xl,drawparsenWindowFix)
	}
	res1

	par(xpd=FALSE,oma=c(0,0,0,10)) #oma=c(0,0,0,10)
	plot(x=1, type="n", xlab="h", ylab="LOO for drawparsenWindowFix",
		 xlim=c(0, 41), ylim=c(0, 20), xaxs="i", yaxs="i")
	points(1:40,res1[1:40])
}

drawloodrawparsenWindowFloat <- function() {
	res1 <- 1
	for(i in 1:40)
	{
	  res1[i] <- loo(i,xl,drawparsenWindowFloat)
	}
	res1

	par(xpd=FALSE,oma=c(0,0,0,10)) #oma=c(0,0,0,10)
	plot(x=1, type="n", xlab="h", ylab="LOO for drawparsenWindowFloat",
		 xlim=c(0, 41), ylim=c(0, 0.1), xaxs="i", yaxs="i")
	points(1:40,res1[1:40])
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
source("main.R",echo=FALSE)
drawmarga()
drawlookNN()
drawkNN()
drawlookNN()
xl <- STOLP(xl,0,4)
drawSTOLPkNN()

xl[1,1:2] <- c(3.8,1.3)
xl[1,3] <- "versicolor"
xl[2,1:2] <- c(3.8,1.7)
xl[2,3] <- "versicolor"
xl[3,1:2] <- c(4.2,1.3)
xl[3,3] <- "versicolor"
xl[4,1:2] <- c(4.2,1.7)
xl[4,3] <- "versicolor"

xl[5,1:2] <- c(3.6,1.1)
xl[6,1:2] <- c(3.6,1.9)
xl[7,1:2] <- c(4.4,1.1)
xl[8,1:2] <- c(4.4,1.9)

xl[9,1:2] <- c(4.0,0.9) 
xl[10,1:2] <- c(4.0,2.1)
xl[11,1:2] <- c(3.4,1.5)
xl[12,1:2] <- c(4.6,1.5)

xl <- iris[1:12, c(3,4,5)]

optimizedCharge(xl,20,1)

margin <- 0.3
xright <- max(xl[, 1]) + margin
xleft <-max(min(xl[, 1]) - margin,0)
ytop <- max(xl[, 2]) + margin
ybot <- max(min(xl[, 2]) - margin,0)
par(xpd=FALSE,oma=c(0,0,0,10))

plot(main="parsenWindowFloat, h(k), k=4", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
     xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")

for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
  for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
    class <- parsenWindowFloat(xl, c(j,i), 4,ker.type[1])
    if(class!="") points(j, i, pch =1, col =fontcolors[class]) # cex=6, lwd=4, asp=1
  }
}

points(xl[, c(1,2)], pch =21, bg = colors[xl[,3]], col ="black", asp=1)

 legend(x=xright, y=ytop*2/3, legend=names(colors[1:2]), #"topleft", inset=c(1.1,0.2)
        cex=1.5, pch = c(21,21,21),
       text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]]),
       xpd=NA,horiz=FALSE, bty="n", bg="white") #xpd=TRUE, bty="y"


margin <- 0.3
xright <- max(xl[, 1]) + margin
xleft <-max(min(xl[, 1]) - margin,0)
ytop <- max(xl[, 2]) + margin
ybot <- max(min(xl[, 2]) - margin,0)
par(xpd=FALSE,oma=c(0,0,0,10))

plot(main="parsenWindowFix, h=1, Ker: gaussian", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
     xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")

for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
  for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
    class <- parsenWindowFix(xl, c(j,i), 1, ker.type[7])
    if(class!="") points(j, i, pch =1, col =fontcolors[class]) # cex=6, lwd=4, asp=1
  }
}

points(xl[, c(1,2)], pch =21, bg = colors[xl[,3]], col ="black", asp=1)
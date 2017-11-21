margin <- 0.3
xright <- max(xl[, 1]) + margin
xleft <-max(min(xl[, 1]) - margin,0)
ytop <- max(xl[, 2]) + margin
ybot <- max(min(xl[, 2]) - margin,0)
par(xpd=FALSE,oma=c(0,0,0,10))

# Создаем пустой холст
plot(main="title", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
     xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")
	 
# # Классификация поля объектов
for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
  for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
    class <- kwNN(xl, c(j,i), 50)
    if(class!="") points(j, i, pch =1, col =fontcolors[class]) # cex=6, lwd=4, asp=1
  }
}
# Рисуем выборку
points(iris[, c(a,b)], pch =21, bg = colors[iris$Species], col ="black", asp=1)
points(iris[, c(a,b)], pch =1, col = colors[iris$Species])
points(res2[, c(1,2)], pch =21, bg = colors[res2$Species], col ="black",cex=1.5)

legend(x=xright, y=ytop*2/3, legend=names(colors), #"topleft", inset=c(1.1,0.2)
        cex=1.5, pch = c(21,21,21),
        text.font=6, pt.bg=c(colors[names(colors)[1]],colors[names(colors)[2]],colors[names(colors)[3]]),
        xpd=NA,horiz=FALSE, bty="n", bg="white") #xpd=TRUE, bty="y"

legend(x=41, y=0.15*2/3, lty=1, lwd=3, col=veC, legend=c("kNN", "kwNN",
      "parsenWindowFix", "parsenWindowFloat"),
       cex=1.2, text.font=6, xpd=NA,bty="n",pch=20) #cex=1.5


# res1 <- 1
# for(i in 1:40)
# {
#   res1[i] <- loo(i,xl,parsenWindowFloat)
# }
# res1
# 
# par(xpd=FALSE,oma=c(0,0,0,10)) #oma=c(0,0,0,10)
# plot(x=1, type="n", xlab="h", ylab="LOO",
#      xlim=c(0, 41), ylim=c(0, 2), xaxs="i", yaxs="i")
# points(1:40,res2[1:40])


# res <- min(loo(1:40,xl))


veC=c("grey","black","brown","red")
plot(x=1, type="n", xlab="parametr", ylab="LOO",
     xlim=c(0, 41), ylim=c(0, 0.15), xaxs="i", yaxs="i")

lines(1:40,res4,col=veC[1],lwd=3)
lines(1:40,res3,col=veC[2],lwd=3)
lines(1:40,res1,col=veC[3],lwd=3)
lines(1:40,res2,col=veC[4],lwd=3)



# Marga <- NA
# for(i in 1:l)
# {
#   Marga[i] <- Margin(xl,i)
# }
# Marga <- Marga[order(Marga)]
# plot(x=1, type="n", xlab="x_i", ylab="Margin",
#      xlim=c(0, l), ylim=c(-10, 30), xaxs="i", yaxs="i")
# lines(1:l,Marga,lwd=2)
# abline(h=0)

time2 <- system.time( res2 <- STOLP(xl,0,4) )
time1; time2
res; res2 
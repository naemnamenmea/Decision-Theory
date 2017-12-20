library(shiny)

shinyServer(function(input, output) {
  
  output$irisPlot = reactivePlot(function(){
    FP <-  input$first_param
    SP <-  input$second_param
    Xmax <- input$Xnum
    Ymax <- input$Ynum
    
    plot(main="", x=1, type="n", xlab=FP, ylab=SP, xlim = c(0,Xmax), ylim = c(0,Ymax), asp = 1)
    
    # for(i in seq(from=0.1, to=Ymax-0.1, by=0.1)) {
    #   for(j in seq(from=0.1, to=Xmax-0.1, by=0.1)) {
    #     class <- naiveBayes(MERGE(iris[ ,c(match(FP,colnames(iris)),match(SP,colnames(iris))),iris[,5])],c(j,i), lambda = c(1,1,1), h = c(1,1))
    #     if(class!="") points(j, i, pch =1, col =fontcolors[class])
    #   }
    # }
    points(iris[ , match(c(FP,SP),colnames(iris))], pch =21, bg = colors[iris$Species], col ="black", asp=1)
  }
  )
})
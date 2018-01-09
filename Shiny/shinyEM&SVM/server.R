library(shiny)

shinyServer(function(input, output) {
  
  output$EMPlot = reactivePlot(function(){
    
    p1 <- which(colnames(iris)==input$first_param)
    p2 <- which(colnames(iris)==input$second_param)
    
    require(mclust) #Mclust
    
    data(iris)
    
    mc <- Mclust(iris[,1:4],G=input$G) # 3 clusters
    plot(mc, what=c("classification"), dimens=c(p1,p2)) # using 1st and 3rd column of the iris dataset
  }
  )
})
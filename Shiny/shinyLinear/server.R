library(shiny)

shinyServer(function(input, output) {
  
  output$linerPlot = reactivePlot(function(){
    
	drawLinear(ObjectsCountOfEachClass=input$ObjectsCountOfEachClass)
  }
  )
})
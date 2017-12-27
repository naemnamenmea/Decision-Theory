library(shiny)

shinyServer(function(input, output) {
  
  output$linerPlot = reactivePlot(function(){
    FP <-  input$first_param
    SP <-  input$second_param
    Xmax <- input$Xnum
    Ymax <- input$Ynum
    
	drawLinear()
  }
  )
})
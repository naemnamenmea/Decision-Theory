library(shiny)

shinyServer(function(input, output) {
  
  output$LDFPlot = reactivePlot(function(){
    
	drawLDF()
	#drawPlugIn()
  }
  )
})
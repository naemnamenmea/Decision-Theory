library(shiny)

shinyServer(function(input, output) {
  
  output$MapPlot = reactivePlot(function(){
    
	drawMap(kNN,list(xl=xl,k=6),"kNN")
  }
  )
})
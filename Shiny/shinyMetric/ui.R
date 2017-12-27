library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Iris interactive plot"),
  sidebarPanel(
  ),
  mainPanel(
    plotOutput("MapPlot", height = "500px", width = "500px")
  )
  
))
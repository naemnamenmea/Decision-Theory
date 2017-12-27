library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("LDF interactive plot"),
  sidebarPanel(
  
  ),
  mainPanel(
    plotOutput("LDFPlot", height = "500px", width = "500px")
  )
  
))
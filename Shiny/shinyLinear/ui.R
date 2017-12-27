library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Linear interactive plot"),
  sidebarPanel(
  
  ),
  mainPanel(
    plotOutput("linerPlot", height = "500px", width = "500px")
  )
  
))
library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Iris interactive plot"),
  sidebarPanel(
    selectInput(inputId = "first_param",
                label = "Select first attribute:",
                choices = list("Sepal.Length", 
                               "Sepal.Width",
                               "Petal.Length", 
                               "Petal.Width"),
                selected = "Petal.Length"
                )
  ),
  mainPanel(
    plotOutput("MapPlot", height = "500px", width = "500px")
  )
  
))
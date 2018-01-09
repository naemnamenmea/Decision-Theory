library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("SVM implementation"),
  sidebarPanel(
    selectInput(inputId = "kernel",
                label = "Select kernel type:",
                choices = list("linea",
                               "polynomial", 
                               "sigmoid", 
                               "radial"),
                selected = "linea"
    )
  ),
  mainPanel(
    plotOutput("SVM", height = "500px", width = "500px")
  )
  
))
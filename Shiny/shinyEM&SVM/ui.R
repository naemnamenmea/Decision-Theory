library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("EM implementation"),
  sidebarPanel(
    selectInput(inputId = "first_param",
                label = "Select first attribute:",
                choices = list("Sepal.Length", 
                               "Sepal.Width",
                               "Petal.Length", 
                               "Petal.Width"),
                selected = "Sepal.Length"
    ),
    
    selectInput(inputId = "second_param",
                label = "Select second attribute:",
                choices = list("Sepal.Length" = "Sepal.Length", 
                               "Sepal.Width" = "Sepal.Width",
                               "Petal.Length" = "Petal.Length", 
                               "Petal.Width" = "Petal.Width"),
                selected = "Petal.Length"
    ),
    sliderInput(inputId = "G",
                label = "number of clusters",
                value = 3,
                min = 1,
                max = 10)
  ),
  mainPanel(
    plotOutput("EMPlot", height = "500px", width = "500px")
  )
  
))
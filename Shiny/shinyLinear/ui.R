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
                ),
  
    selectInput(inputId = "second_param",
                label = "Select second attribute:",
                choices = list("Sepal.Length" = "Sepal.Length", 
                               "Sepal.Width" = "Sepal.Width",
                               "Petal.Length" = "Petal.Length", 
                               "Petal.Width" = "Petal.Width"),
                selected = "Petal.Width"
                ),
    sliderInput(inputId = "Xnum",
                label = "Choose max X",
                value = 7,
                min = 1,
                max = 10),
    sliderInput(inputId = "Ynum",
                label = "Choose max Y",
                value = 4,
                min = 1,
                max = 10)
  ),
  mainPanel(
    plotOutput("linerPlot", height = "500px", width = "500px")
  )
  
))
library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Linear interactive plot"),
  sidebarPanel(
    sliderInput(inputId = "ObjectsCountOfEachClass",
                label = "Choose N objects in each class",
                value = 75,
                min = 5,
                max = 100)
  ),
  mainPanel(
    plotOutput("linerPlot", height = "500px", width = "500px")
  )
  
))
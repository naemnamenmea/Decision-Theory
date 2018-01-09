library(shiny)

shinyServer(function(input, output) {
  
  output$SVM = reactivePlot(function(){
    
    kernel <- input$kernel
    
    require("e1071")
    require("kernlab")
    require("MASS")
    
    
    data(iris)
    attach(iris)
    
    svm.data <- iris[ Species!="setosa" ,]
    svm.data$Species <- factor(svm.data$Species)
    svm.fit <- svm(Species ~ Petal.Length + Petal.Width, data = svm.data, kernel=kernel)
    plot(svm.fit, svm.data, Petal.Width ~ Petal.Length)
  }
  )
})
# setwd("G:/")

library(shiny)

###################METRIC######################
source("Metric/sourses/kNN.R", echo = FALSE)
source("Metric/sourses/kwNN.R", echo = FALSE)
source("Metric/sourses/parsenWindowFix.R", echo = FALSE)
source("Metric/sourses/parsenWindowFloat.R", echo = FALSE)
source("Metric/sourses/potential.R", echo = FALSE)

###################_BAESSIAN_##################
#source("Baessian/sourses/naiveBayes.R", echo = FALSE)
source("Baessian/sourses/plug-in.R", echo = FALSE)
source("Baessian/sourses/LDF.R", echo = FALSE)

####################_LINEAR_###################
source("Linear/sourses/Adaline.R", echo = FALSE)
source("Linear/sourses/Hebb.R", echo = FALSE)
source("Linear/sourses/LogRegression.R", echo = FALSE)

###################_GENERAL_###################
source("general/sourses/Margin.R", echo = FALSE)
source("general/sourses/paint.R", echo = FALSE)
source("general/sourses/progVal.R", echo = FALSE)
source("general/sourses/STOLP.R", echo = FALSE)
source("general/sourses/support.R", echo = FALSE)

###############################################
#source("EM&RBF/sourses/em&rbf.R", echo = FALSE)

###############################################
#source("ROC&PR/sourses/ROC&PR.R", echo = FALSE)

###############################################
#source("SVM/sourses/SVM.R", echo = FALSE)

####################_SHINY_####################
source("Shiny/shinyApp/ui.R", echo = FALSE)
source("Shiny/shinyApp/server.R", echo = FALSE)

source("Shiny/shinyLinear/ui.R", echo = FALSE)
source("Shiny/shinyLinear/server.R", echo = FALSE)

source("Shiny/shinyBaessian/ui.R", echo = FALSE)
source("Shiny/shinyBaessian/server.R", echo = FALSE)

source("Shiny/shinyMetric/ui.R", echo = FALSE)
source("Shiny/shinyMetric/server.R", echo = FALSE)
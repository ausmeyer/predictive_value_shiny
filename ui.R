library(shiny)

## Define question and answer choices

# Define UI for miles per gallon application
shinyUI(fluidPage(
  
  # Application title
  headerPanel("Predictive Value of Tests"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("sens", "Sensitivity",  
                  min = 0, max = 1, value = 0.5),
      sliderInput("spec", "Specificity",  
                  min = 0, max = 1, value = 0.5)
    ),
    
    mainPanel(
      plotOutput('ppvPlot'),
      plotOutput('npvPlot')
    )
  )
))

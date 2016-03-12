library(shiny)

## Define question and answer choices
Question <- "Investigators are studying prostate specific antigen (PSA) as a predictor for prostate cancer. To make the statistics easier, they are going to assume that PSA is a normally distributed population variable. Which of the following is correct under their assumption?"
Answer1 <- "Mode is greater than median"
Answer2 <- "Median is greater than mode"
Answer3 <- "95% CI depends on degrees of freedom"
Answer4 <- "Median is equal to mean"
Answer5 <- "Mean is equal to standard deviation"

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
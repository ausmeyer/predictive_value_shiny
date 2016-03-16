library(shiny)

shinyUI(
  fluidPage(
    
    # Application title
    title = "Predictive Value of Clinical Tests",
    
    fluidRow(column(6, plotOutput('prevalencePlot')), column(3, plotOutput('ppvPlot')), column(3, plotOutput('npvPlot'))),
    
    p("This simple widget is meant to show the effect that changing clinical test accuracy, precision, sensitivity, and specificity has on the other variables. You have to select whether to manipulate the characteristic statistics of the test (sensitivity and specificity) or the direct quantities (non-disease and disease mean and standard deviation, and the cutoff for a positive test). Whichever set you want to manipulate using the sliders, the other quantities will be calculated."),
    
    hr(),
    
    fluidRow(
      column(4,
             wellPanel(
               radioButtons("radio",
                            label = "Which quantities to calculate?",
                            choices = list("Manipulate Sensitivity and Specificity" = 1, "Manipulate Mean, Spread, and Cutoff" = 2), 
                            selected = 1)
             )
      ),
      column(4,
             sliderInput("sens", "Sensitivity",  
                         min = 0, max = 1, value = 0.5, step = 0.01, width = '100%')
      ),
      column(4,
             sliderInput("spec", "Specificity",  
                         min = 0, max = 1, value = 0.5, step = 0.01, width = '100%')
      )
    ),
    
    hr(),
    
    fluidRow(
      column(4,
             sliderInput("n", "Population Size",  
                         min = 0, max = 20000, value = 2000, step = 1, width = '100%')
      ),
      column(4,
             sliderInput("prev", "Prevalence",  
                         min = 0, max = 1, value = 0.1, step = 0.01, width = '100%')
      ),
      column(4,
             sliderInput("cutoff", "Cutoff",  
                         min = 0, max = 20, value = 10, step = 0.01, width = '100%')
      )
    ),
    
    hr(),
    
    fluidRow(
      column(3,
             sliderInput("no_disease_mean", "No Disease Mean",  
                         min = 0, max = 20, value = 10, step = 0.01, width = '100%')
      ),
      column(3,
             sliderInput("no_disease_spread", "No Disease Spread",  
                         min = 0.1, max = 5, value = 2.5, step = 0.01, width = '100%')
      ),
      column(3,
             sliderInput("disease_mean", "Disease Mean",  
                         min = 0, max = 20, value = 10, step = 0.01, width = '100%')
      ),
      column(3,
             sliderInput("disease_spread", "Disease Spread",  
                         min = 0.1, max = 5, value = 2.5, step = 0.01, width = '100%')
      )
    )
  )
)

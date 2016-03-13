library(shiny)

shinyUI(
  fluidPage(
    
    # Application title
    title = "Predictive Value of Clinical Tests",
    
    fluidRow(column(6, plotOutput('prevalencePlot')), column(3, plotOutput('ppvPlot')), column(3, plotOutput('npvPlot'))),
    
    hr(),
    
    fluidRow(
      column(3,
             sliderInput("n", "Population Size",  
                         min = 0, max = 20000, value = 2000, step = 1)
      ),
      column(3,
             sliderInput("prev", "Prevalence",  
                         min = 0, max = 1, value = 0.1, step = 0.01)
      ),
      column(3,
             sliderInput("sens", "Sensitivity",  
                         min = 0, max = 1, value = 0.5, step = 0.01)
      ),
      column(3,
             sliderInput("spec", "Specificity",  
                         min = 0, max = 1, value = 0.5, step = 0.01)
      )
    ),
    
    hr(),
    
    fluidRow(
      column(3,
             sliderInput("no_disease_mean", "No Disease Mean",  
                         min = 0, max = 20, value = 10, step = 0.01)
      ),
      column(3,
             sliderInput("no_disease_spread", "No Disease Spread",  
                         min = 0.1, max = 4, value = 2, step = 0.01)
      ),
      column(3,
             sliderInput("disease_mean", "Disease Mean",  
                         min = 0, max = 20, value = 10, step = 0.01)
      ),
      column(3,
             sliderInput("disease_spread", "Disease Spread",  
                         min = 0.1, max = 4, value = 2, step = 0.01)
      )
    ),
    
    hr(),
    
    fluidRow(
      column(3,
             sliderInput("cutoff", "Cutoff",  
                         min = 0, max = 20, value = 10, step = 0.01)
      )
    ),
    
    p("This simple widget is meant to show the affect that changing prevalence, sensitivity and specificity has on positive predictive value, negative predictive value and the distribution of data. Currently, the means, spreads and cutoffs are only for display. They do not affect the visualization or other parameters. In addition, the test distributions are fit based on the sensitivity and specificity. As a result, I have included the calculated sensitivity and specificity for the actual distributions displayed on the left.")
    
  )
)

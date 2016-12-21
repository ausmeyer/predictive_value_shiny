library(shiny)

shinyUI(
  fluidPage(
    
    # Application title
    title = "Predictive Value of Clinical Tests",
    
    fluidRow(column(6, plotOutput('prevalencePlot')), column(3, plotOutput('ppvPlot')), column(3, plotOutput('npvPlot'))),
  
    br(),
    
    fluidRow(
      column(4,
             wellPanel(
               radioButtons("radio",
                            label = "Which quantities to manipulate?",
                            choices = list("Manipulate Sensitivity and Specificity" = 1, "Manipulate Mean, Spread, and Cutoff" = 2), 
                            selected = 1)
             )
      ),
      column(4,
             sliderInput("sens", "Sensitivity",  
                         min = 0, max = 1, value = 0.5, step = 0.001, width = '100%')
      ),
      column(4,
             sliderInput("spec", "Specificity",  
                         min = 0, max = 1, value = 0.5, step = 0.001, width = '100%')
      )
    ),
    
    hr(),
    
    fluidRow(
      column(4,
             sliderInput("n", "Population Size",  
                         min = 0, max = 30000, value = 9000, step = 1, width = '100%')
      ),
      column(4,
             sliderInput("prev", "Pretest Probability",  
                         min = 0, max = 1, value = 0.1, step = 0.01, width = '100%')
      ),
      column(4,
             sliderInput("cutoff", "Cutoff",  
                         min = 10, max = 40, value = 10, step = 0.01, width = '100%')
      )
    ),
    fluidRow(
      column(3,
             sliderInput("no_disease_mean", "No Disease Mean",  
                         min = 10, max = 30, value = 20, step = 0.01, width = '100%')
      ),
      column(3,
             sliderInput("no_disease_spread", "No Disease Spread",  
                         min = 0.1, max = 3, value = 1.5, step = 0.01, width = '100%')
      ),
      column(3,
             sliderInput("disease_mean", "Disease Mean",  
                         min = 10, max = 30, value = 20, step = 0.01, width = '100%')
      ),
      column(3,
             sliderInput("disease_spread", "Disease Spread",  
                         min = 0.1, max = 3, value = 1.5, step = 0.01, width = '100%')
      )
    ),
    
    strong("Caption:"),
    
    p("This widget is meant to show the effect that changing clinical test accuracy, precision, sensitivity, and specificity has on other variables. You have to select whether to manipulate the characteristic statistics of the test (sensitivity and specificity) or the direct quantities (non-disease and disease mean and standard deviation, and the cutoff for a positive test). Whichever set you want to manipulate using the sliders, the other quantities will be calculated. More information is available at the bottom of this page."),
    hr(),
    br(),
    br(),
    
    strong("Further Technical Explanation:"),
    p("From a technical standpoint, the manipulated quantities are the target values for optimization of the other opposing variables; therefore, it is difficult to hit the exact targets specified. As a result, I provide the analytically calculated sensitivity and specificity above the population plot. In addition, I provide the positive predictive value and negative predictive value for the specified pretest probability above their respective plots. In either mode, it is always possible to change the population size and the pretest probability. The population plot on the left is just an example population that fits the specified parameters. In principle, there are infinitely many populations that are compatible with any particular test sensitivity and specificity. I recommend changing the population size only if you need a more accurate calculation. Otherwise, it will slow down the app some.")
  )
)

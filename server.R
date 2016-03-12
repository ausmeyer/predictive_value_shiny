library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  output$ppvPlot <- renderPlot({
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(cowplot)
    
    sensitivity <- input$sens
    specificity <- input$spec
    
    prevalence <- seq(0, 1, by = 0.01)
    n <- 1000
    
    TP <- n * prevalence
    TN <- n - TP
    FP <- n * (1 - specificity)
    FN <- n * prevalence * (1 - sensitivity)
    PPV <- TP / (TP + FP)
    NPV <- TN / (TN + FN)
    
    data <- data.frame(x = prevalence, y = PPV)
    ppvplot <- ggplot(data, aes(x = x, y = y)) + 
      geom_line() +
      ylim(0, 1) + 
      ggtitle("Positive Predictive Value versus Prevalence") +
      labs(x="Prevalence", y="Positive Predictive Value")
    
    show(ppvplot)
  })
  
  output$npvPlot <- renderPlot({
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(cowplot)
    
    sensitivity <- input$sens
    specificity <- input$spec
    
    prevalence <- seq(0, 1, by = 0.01)
    n <- 1000
    
    TP <- n * prevalence
    TN <- n - TP
    FP <- n * (1 - specificity)
    FN <- n * prevalence * (1 - sensitivity)
    PPV <- TP / (TP + FP)
    NPV <- TN / (TN + FN)
    
    data <- data.frame(x = prevalence, y = NPV)
    npvplot <- ggplot(data, aes(x = x, y = y)) + 
      geom_line() +
      ylim(0, 1) + 
      ggtitle("Negative Predictive Value versus Prevalence") +
      labs(x="Prevalence", y="Negative Predictive Value")
    
    show(npvplot)
  })
})
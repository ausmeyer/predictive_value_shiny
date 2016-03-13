library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  calc.metrics <- reactive({
    prevalence.point <- input$prev
    n <- input$n
    with.disease <- prevalence.point * n
    without.disease <- n - with.disease
    
    calc.sensitivity <- function() {
      with.disease <- prevalence.point * n
      with.disease.distribution <- rnorm(with.disease, mean = input$disease_mean, sd = input$disease_spread)
      TP <- sum(with.disease.distribution >= input$cutoff)
      TP / with.disease
    }
    
    calc.specificity <- function() {
      without.disease <- n - prevalence.point * n
      without.disease.distribution <- rnorm(without.disease, mean = input$no_disease_mean, sd = input$no_disease_spread)
      TN <- sum(without.disease.distribution < input$cutoff)
      TN / without.disease
    }
    
    calculated.sensitivity <- calc.sensitivity()
    calculated.specificity <- calc.specificity()
    
    return(c(calculated.sensitivity, calculated.specificity))
  })
  
  calc.NPV <- reactive({
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- input$prev
    n <- input$n
    
    sensitivity <- input$sens
    specificity <- input$spec
    
    TP <- n * prevalence * sensitivity                           # people testing correctly positive
    TN <- (n - (n * prevalence)) * specificity                   # people testing correctly negative
    FP <- (n - (n * prevalence)) * (1 - specificity)             # people who do not have disease but test positive
    FN <- n * prevalence * (1 - sensitivity)                     # people who have disease but test negative
    NPV <- TN / (TN + FN)
  })
  
  calc.PPV <- reactive({
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- input$prev
    n <- input$n
    
    sensitivity <- input$sens
    specificity <- input$spec
    
    TP <- n * prevalence * sensitivity                           # people testing correctly positive
    TN <- (n - (n * prevalence)) * specificity                   # people testing correctly negative
    FP <- (n - (n * prevalence)) * (1 - specificity)             # people who do not have disease but test positive
    FN <- n * prevalence * (1 - sensitivity)                     # people who have disease but test negative
    PPV <- TP / (TP + FP)
  })
  
  renderPop.changedPop <- reactive({
    library(ggplot2)
    library(cowplot)
    library(grid)
    
    prevalence.point <- input$prev
    n <- input$n
    with.disease <- prevalence.point * n
    without.disease <- n - with.disease
    
    sensitivity <- round(calc.metrics()[1], digits = 2)
    specificity <- round(calc.metrics()[2], digits = 2)
    
    plot.function <- function(disease.distribution, no.disease.distribution, sens, spec, test_cutoff) {
      prevalence.plot <- ggplot(data.frame(data = c(disease.distribution, no.disease.distribution), 
                                           Groups = c(rep('Disease', length(disease.distribution)), 
                                                      rep('No Disease', length(no.disease.distribution)))), 
                                aes(x=data, fill=Groups)) + 
        geom_histogram(alpha = 0.2, position="identity", bins = 50) + 
        geom_vline(xintercept = test_cutoff, linetype = 'solid', lwd = 0.5) +
        theme(plot.margin = unit(c(2,1,1,1), "lines")) + 
        scale_fill_manual(values = c("No Disease" = "blue", "Disease" = "red")) +
        xlim(-5, 25) +
        labs(x="Clinical Test Result", y="Number of Patients") +
        annotate("text", label = paste("Calculated Sensitivity: ", sens, sep = ""), x = -Inf, y = Inf, hjust = 0, vjust = -1) +
        annotate("text", label = paste("Calculated Specificity: ", spec, sep = ""), x = Inf, y = Inf, hjust = 1, vjust = -1)
      
      gg2 <- ggplot_gtable(ggplot_build(prevalence.plot))
      gg2$layout$clip[gg2$layout$name == "panel"] <- "off"
      grid.draw(gg2)
    }
    
    with.disease.distribution <- rnorm(with.disease, mean = input$disease_mean, sd = input$disease_spread)
    without.disease.distribution <- rnorm(without.disease, mean = input$no_disease_mean, sd = input$no_disease_spread)
    
    plot.function(with.disease.distribution, without.disease.distribution, sensitivity, specificity, input$cutoff)
    
    
  })
  
  renderPop.constantPop <- reactive({
    library(ggplot2)
    library(cowplot)
    library(grid)
    
    prevalence.point <- input$prev
    n <- input$n
    with.disease <- prevalence.point * n
    without.disease <- n - with.disease
    
    sensitivity <- input$sens
    specificity <- input$spec
    
    plot.function <- function(disease.distribution, no.disease.distribution, sens, spec, test_cutoff) {
      prevalence.plot <- ggplot(data.frame(data = c(disease.distribution, no.disease.distribution), 
                                           Groups = c(rep('Disease', length(disease.distribution)),
                                                      rep('No Disease', length(no.disease.distribution)))), 
                                aes(x=data, fill=Groups)) + 
        geom_histogram(alpha = 0.2, position="identity", bins = 50) + 
        geom_vline(xintercept = test_cutoff, linetype = 'solid', lwd = 0.5) +
        theme(plot.margin = unit(c(2,1,1,1), "lines")) + 
        scale_fill_manual(values = c("No Disease" = "blue", "Disease" = "red")) +
        xlim(-5, 25) +
        labs(x="Clinical Test Result", y="Number of Patients") +
        annotate("text", label = paste("Calculated Sensitivity: ", sens, sep = ""), x = -Inf, y = Inf, hjust = 0, vjust = -1) +
        annotate("text", label = paste("Calculated Specificity: ", spec, sep = ""), x = Inf, y = Inf, hjust = 1, vjust = -1)
      
      gg2 <- ggplot_gtable(ggplot_build(prevalence.plot))
      gg2$layout$clip[gg2$layout$name == "panel"] <- "off"
      grid.draw(gg2)
    }
    
    fn <- function(x) {
      tmp.with.disease <- rnorm(with.disease, mean = x[1], sd = x[2])
      TP <- sum(tmp.with.disease >= x[5])         # people testing correctly positive
      
      tmp.without.disease <- rnorm(without.disease, mean = x[3], sd = x[4])
      TN <- sum(tmp.without.disease < x[5])       # people testing correctly negative
      
      return((sensitivity - TP / with.disease)^2 + (specificity - TN / without.disease)^2)
    }
    
    optim.distribution <- optim(par = c(10, 2, 10, 2, 10), fn)
    
    with.disease.distribution <- rnorm(with.disease, mean = optim.distribution$par[1], sd = optim.distribution$par[2])
    without.disease.distribution <- rnorm(without.disease, mean = optim.distribution$par[3], sd = optim.distribution$par[4])
    
    print(mean(with.disease.distribution))
    TP <- sum(with.disease.distribution >= optim.distribution$par[5])
    calculated.sensitivity <- round(TP / with.disease, digits = 2)
    print(mean(without.disease.distribution))
    TN <- sum(without.disease.distribution < optim.distribution$par[5])
    calculated.specificity <- round(TN / without.disease, digits = 2)
    
    updateSliderInput(session, "disease_mean", value = mean(with.disease.distribution))
    updateSliderInput(session, "disease_spread", value = sd(with.disease.distribution))
    updateSliderInput(session, "no_disease_mean", value = mean(without.disease.distribution))
    updateSliderInput(session, "no_disease_spread", value = sd(without.disease.distribution))
    updateSliderInput(session, "cutoff", value = optim.distribution$par[5])
    
    plot.function(with.disease.distribution, without.disease.distribution, calculated.sensitivity, calculated.specificity, optim.distribution$par[5])
  })
  
  renderPPV.changedPop <- reactive({
    library(ggplot2)
    library(cowplot)
    
    plot.function <- function(prev, ppv, ppv.point) {
      data <- data.frame(x = prevalence, y = PPV)
      npvplot <- ggplot(data, aes(x = x, y = y)) + 
        geom_line() +
        geom_segment(x = prevalence.point, xend = prevalence.point, y = 0, yend = ppv.point, color = 'red', linetype = 'dotted') +
        geom_segment(x = 0, xend = prevalence.point, y = ppv.point, yend = ppv.point, color = 'red', linetype = 'dotted') +
        ylim(0, 1) + 
        labs(x="Prevalence", y="Positive Predictive Value")
      
      show(npvplot)
    }
    
    calc.PPV.local <- function(sens, spec) {
      prevalence <- seq(0, 1, by = 0.01)
      prevalence.point <- input$prev
      n <- input$n
      
      sensitivity <- sens
      specificity <- spec
      
      TP <- n * prevalence * sensitivity                           # people testing correctly positive
      TN <- (n - (n * prevalence)) * specificity                   # people testing correctly negative
      FP <- (n - (n * prevalence)) * (1 - specificity)             # people who do not have disease but test positive
      FN <- n * prevalence * (1 - sensitivity)                     # people who have disease but test negative
      PPV <- TP / (TP + FP)
    }
    
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- input$prev
    
    sensitivity <- round(calc.metrics()[1], digits = 2)
    specificity <- round(calc.metrics()[2], digits = 2)
    
    PPV <- calc.PPV.local(sensitivity, specificity)
    PPV.point <- PPV[prevalence.point * length(prevalence) + 1]
    
    updateSliderInput(session, "sens", value = sensitivity)
    updateSliderInput(session, "spec", value = specificity)
    
    plot.function(prevalence, PPV, PPV.point)
  })
  
  renderPPV.constantPop <- reactive({
    library(ggplot2)
    library(cowplot)
    
    plot.function <- function(prev, ppv, ppv.point) {
      data <- data.frame(x = prevalence, y = PPV)
      npvplot <- ggplot(data, aes(x = x, y = y)) + 
        geom_line() +
        geom_segment(x = prevalence.point, xend = prevalence.point, y = 0, yend = ppv.point, color = 'red', linetype = 'dotted') +
        geom_segment(x = 0, xend = prevalence.point, y = ppv.point, yend = ppv.point, color = 'red', linetype = 'dotted') +
        ylim(0, 1) + 
        labs(x="Prevalence", y="Positive Predictive Value")
      
      show(npvplot)
    }
    
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- input$prev
    
    PPV <- calc.PPV()
    PPV.point <- PPV[prevalence.point * length(prevalence) + 1]
    
    plot.function(prevalence, PPV, PPV.point)
  })
  
  renderNPV.changedPop <- reactive({
    library(ggplot2)
    library(cowplot)
    
    plot.function <- function(prev, npv, npv.point) {
      data <- data.frame(x = prevalence, y = NPV)
      npvplot <- ggplot(data, aes(x = x, y = y)) + 
        geom_line() +
        geom_segment(x = prevalence.point, xend = prevalence.point, y = 0, yend = npv.point, color = 'red', linetype = 'dotted') +
        geom_segment(x = 0, xend = prevalence.point, y = npv.point, yend = npv.point, color = 'red', linetype = 'dotted') +
        ylim(0, 1) + 
        labs(x="Prevalence", y="Negative Predictive Value")
      
      show(npvplot)
    }
    
    calc.NPV.local <- function(sens, spec) {
      prevalence <- seq(0, 1, by = 0.01)
      prevalence.point <- input$prev
      n <- input$n
      
      sensitivity <- sens
      specificity <- spec
      
      TP <- n * prevalence * sensitivity                           # people testing correctly positive
      TN <- (n - (n * prevalence)) * specificity                   # people testing correctly negative
      FP <- (n - (n * prevalence)) * (1 - specificity)             # people who do not have disease but test positive
      FN <- n * prevalence * (1 - sensitivity)                     # people who have disease but test negative
      NPV <- TN / (TN + FN)
    }
    
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- input$prev
    
    sensitivity.global <- input$sens
    specificity.global <- input$spec
    sensitivity.local <- round(calc.metrics()[1], digits = 2)
    specificity.local <- round(calc.metrics()[2], digits = 2)
    
    if(sensitivity.global == sensitivity.local & specificity.global == specificity.local) {
      NPV <- calc.NPV()
      NPV.point <- NPV[prevalence.point * length(prevalence) + 1]
      
      plot.function(prevalence, NPV, NPV.point)
    }
    else {
      NPV <- calc.NPV.local(sensitivity.local, specificity.local)
      NPV.point <- NPV[prevalence.point * length(prevalence) + 1]
      
      updateSliderInput(session, "sens", value = sensitivity.local)
      updateSliderInput(session, "spec", value = specificity.local)
      
      plot.function(prevalence, NPV, NPV.point)
    }
  })
  
  renderNPV.constantPop <- reactive({
    library(ggplot2)
    library(cowplot)
    
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- input$prev
    NPV <- calc.NPV()
    
    plot.function <- function(prev, npv, npv.point) {
      data <- data.frame(x = prevalence, y = NPV)
      npvplot <- ggplot(data, aes(x = x, y = y)) + 
        geom_line() +
        geom_segment(x = prevalence.point, xend = prevalence.point, y = 0, yend = npv.point, color = 'red', linetype = 'dotted') +
        geom_segment(x = 0, xend = prevalence.point, y = npv.point, yend = npv.point, color = 'red', linetype = 'dotted') +
        ylim(0, 1) + 
        labs(x="Prevalence", y="Negative Predictive Value")
      
      show(npvplot)
    }
    
    NPV <- calc.NPV()
    NPV.point <- NPV[prevalence.point * length(prevalence) + 1]
    
    plot.function(prevalence, NPV, NPV.point)
  })
  
  output$prevalencePlot <- renderPlot(renderPop.changedPop())
  output$ppvPlot <- renderPlot(renderPPV.changedPop())
  output$npvPlot <- renderPlot(renderNPV.changedPop())
})
library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  dataInputFitSensSpec <- reactive({
    data.frame(n = input$n, 
               prev = input$prev,
               cutoff = input$cutoff,
               disease_mean = input$disease_mean,
               disease_spread = input$disease_spread,
               no_disease_mean = input$no_disease_mean,
               no_disease_spread = input$no_disease_spread,
               sensitivity = isolate({input$sens}),
               specificity = isolate({input$spec})
    )
  })
  
  dataInputChangedSensSpec <- reactive({
    data.frame(n = input$n, 
               prev = input$prev,
               cutoff = isolate({input$cutoff}),
               disease_mean = isolate({input$disease_mean}),
               disease_spread = isolate({input$disease_spread}),
               no_disease_mean = isolate({input$no_disease_mean}),
               no_disease_spread = isolate({input$no_disease_spread}),
               sensitivity = input$sens,
               specificity = input$spec
    )
  })
  
  calc.metrics.changedPop <- function() {
    prevalence.point <- dataInputFitSensSpec()$prev
    n <- dataInputFitSensSpec()$n 
    with.disease <- prevalence.point * n
    without.disease <- n - with.disease
    
    calc.sensitivity <- function() {
      with.disease <- prevalence.point * n
      with.disease.distribution <- rnorm(with.disease, mean = dataInputFitSensSpec()$disease_mean, sd = dataInputFitSensSpec()$disease_spread)
      TP <- sum(with.disease.distribution >= dataInputFitSensSpec()$cutoff)
      TP / with.disease
    }
    
    calc.specificity <- function() {
      without.disease <- n - prevalence.point * n
      without.disease.distribution <- rnorm(without.disease, mean = dataInputFitSensSpec()$no_disease_mean, sd = dataInputFitSensSpec()$no_disease_spread)
      TN <- sum(without.disease.distribution < dataInputFitSensSpec()$cutoff)
      TN / without.disease
    }
    
    calculated.sensitivity <- calc.sensitivity()
    calculated.specificity <- calc.specificity()
    
    return(c(calculated.sensitivity, calculated.specificity))
  }
  
  calc.NPV.changedPop <- function() {
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- dataInputFitSensSpec()$prev
    n <- dataInputFitSensSpec()$n
    
    sensitivity <- calc.metrics.changedPop()[1]
    specificity <- calc.metrics.changedPop()[2]
    
    TP <- n * prevalence * sensitivity                           # people testing correctly positive
    TN <- (n - (n * prevalence)) * specificity                   # people testing correctly negative
    FP <- (n - (n * prevalence)) * (1 - specificity)             # people who do not have disease but test positive
    FN <- n * prevalence * (1 - sensitivity)                     # people who have disease but test negative
    NPV <- TN / (TN + FN)
  }
  
  calc.PPV.changedPop <- function() {
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- dataInputFitSensSpec()$prev
    n <- dataInputFitSensSpec()$n
    
    sensitivity <- calc.metrics.changedPop()[1]
    specificity <- calc.metrics.changedPop()[2]
    
    TP <- n * prevalence * sensitivity                           # people testing correctly positive
    TN <- (n - (n * prevalence)) * specificity                   # people testing correctly negative
    FP <- (n - (n * prevalence)) * (1 - specificity)             # people who do not have disease but test positive
    FN <- n * prevalence * (1 - sensitivity)                     # people who have disease but test negative
    PPV <- TP / (TP + FP)
  }
  
  calc.metrics.constantPop <- function() {
    prevalence.point <- dataInputChangedSensSpec()$prev
    n <- dataInputChangedSensSpec()$n 
    with.disease <- prevalence.point * n
    without.disease <- n - with.disease
    
    calc.sensitivity <- function() {
      with.disease <- prevalence.point * n
      with.disease.distribution <- rnorm(with.disease, mean = dataInputChangedSensSpec()$disease_mean, sd = dataInputChangedSensSpec()$disease_spread)
      TP <- sum(with.disease.distribution >= dataInputChangedSensSpec()$cutoff)
      TP / with.disease
    }
    
    calc.specificity <- function() {
      without.disease <- n - prevalence.point * n
      without.disease.distribution <- rnorm(without.disease, mean = dataInputChangedSensSpec()$no_disease_mean, sd = dataInputChangedSensSpec()$no_disease_spread)
      TN <- sum(without.disease.distribution < dataInputChangedSensSpec()$cutoff)
      TN / without.disease
    }
    
    calculated.sensitivity <- calc.sensitivity()
    calculated.specificity <- calc.specificity()
    
    return(c(calculated.sensitivity, calculated.specificity))
  }
  
  calc.NPV.constantPop <- function() {
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- dataInputChangedSensSpec()$prev
    n <- dataInputChangedSensSpec()$n
    
    sensitivity <- dataInputChangedSensSpec()$sensitivity
    specificity <- dataInputChangedSensSpec()$specificity
    
    TP <- n * prevalence * sensitivity                           # people testing correctly positive
    TN <- (n - (n * prevalence)) * specificity                   # people testing correctly negative
    FP <- (n - (n * prevalence)) * (1 - specificity)             # people who do not have disease but test positive
    FN <- n * prevalence * (1 - sensitivity)                     # people who have disease but test negative
    NPV <- TN / (TN + FN)
  }
  
  calc.PPV.constantPop <- function() {
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- dataInputChangedSensSpec()$prev
    n <- dataInputChangedSensSpec()$n
    
    sensitivity <- dataInputChangedSensSpec()$sensitivity
    specificity <- dataInputChangedSensSpec()$specificity
    
    TP <- n * prevalence * sensitivity                           # people testing correctly positive
    TN <- (n - (n * prevalence)) * specificity                   # people testing correctly negative
    FP <- (n - (n * prevalence)) * (1 - specificity)             # people who do not have disease but test positive
    FN <- n * prevalence * (1 - sensitivity)                     # people who have disease but test negative
    PPV <- TP / (TP + FP)
  }
  
  renderPop.changedPop <- function() {
    library(ggplot2)
    library(cowplot)
    library(grid)
    
    prevalence.point <- dataInputFitSensSpec()$prev
    n <- dataInputFitSensSpec()$n
    with.disease <- prevalence.point * n
    without.disease <- n - with.disease
    
    plot.function <- function(disease.distribution, no.disease.distribution, sens, spec, test_cutoff) {
      prevalence.plot <- ggplot(data.frame(data = c(disease.distribution, no.disease.distribution), 
                                           Groups = c(rep('Disease', length(disease.distribution)), 
                                                      rep('No Disease', length(no.disease.distribution)))), 
                                aes(x=data, fill=Groups)) + 
        geom_histogram(alpha = 0.2, position="identity", bins = 50) + 
        geom_vline(xintercept = test_cutoff, linetype = 'solid', lwd = 0.5) +
        theme(plot.margin = unit(c(2,1,1,1), "lines")) + 
        scale_fill_manual(values = c("No Disease" = "blue", "Disease" = "red")) +
        xlim(-5, 45) +
        labs(x="Clinical Test Result", y="Number of Patients") +
        annotate("text", label = paste("Sensitivity: ", sens, sep = ""), x = -Inf, y = Inf, hjust = 0, vjust = -1) +
        annotate("text", label = paste("Specificity: ", spec, sep = ""), x = Inf, y = Inf, hjust = 1, vjust = -1)
      
      gg2 <- ggplot_gtable(ggplot_build(prevalence.plot))
      gg2$layout$clip[gg2$layout$name == "panel"] <- "off"
      grid.draw(gg2)
    }
    
    sensitivity.local <- round(calc.metrics.changedPop()[1], digits = 3)
    specificity.local <- round(calc.metrics.changedPop()[2], digits = 3)
    
    with.disease.distribution <- rnorm(with.disease, mean = dataInputFitSensSpec()$disease_mean, sd = dataInputFitSensSpec()$disease_spread)
    without.disease.distribution <- rnorm(without.disease, mean = dataInputFitSensSpec()$no_disease_mean, sd = dataInputFitSensSpec()$no_disease_spread)
    
    plot.function(with.disease.distribution, without.disease.distribution, sensitivity.local, specificity.local, dataInputFitSensSpec()$cutoff)
  }
  
  renderPop.constantPop <- function () {
    library(ggplot2)
    library(cowplot)
    library(grid)
    
    prevalence.point <- dataInputChangedSensSpec()$prev
    n <- dataInputChangedSensSpec()$n
    with.disease <- prevalence.point * n
    without.disease <- n - with.disease
    
    sensitivity <- dataInputChangedSensSpec()$sensitivity
    specificity <- dataInputChangedSensSpec()$specificity
    
    plot.function <- function(disease.distribution, no.disease.distribution, sens, spec, test_cutoff) {
      prevalence.plot <- ggplot(data.frame(data = c(disease.distribution, no.disease.distribution), 
                                           Groups = c(rep('Disease', length(disease.distribution)),
                                                      rep('No Disease', length(no.disease.distribution)))), 
                                aes(x=data, fill=Groups)) + 
        geom_histogram(alpha = 0.2, position="identity", bins = 50) + 
        geom_vline(xintercept = test_cutoff, linetype = 'solid', lwd = 0.5) +
        theme(plot.margin = unit(c(2,1,1,1), "lines")) + 
        scale_fill_manual(values = c("No Disease" = "blue", "Disease" = "red")) +
        xlim(-5, 45) +
        labs(x="Clinical Test Result", y="Number of Patients") +
        annotate("text", label = paste("Sensitivity: ", sens, sep = ""), x = -Inf, y = Inf, hjust = 0, vjust = -1) +
        annotate("text", label = paste("Specificity: ", spec, sep = ""), x = Inf, y = Inf, hjust = 1, vjust = -1)
      
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
    
    optim.distribution <- optim(par = c(20, 1, 20, 1, 20), fn)
    
    with.disease.distribution <- rnorm(with.disease, mean = optim.distribution$par[1], sd = optim.distribution$par[2])
    without.disease.distribution <- rnorm(without.disease, mean = optim.distribution$par[3], sd = optim.distribution$par[4])
    
    TP <- sum(with.disease.distribution >= optim.distribution$par[5])
    calculated.sensitivity <- round(TP / with.disease, digits = 3)
    
    TN <- sum(without.disease.distribution < optim.distribution$par[5])
    calculated.specificity <- round(TN / without.disease, digits = 3)
    
    updateSliderInput(session, "disease_mean", value = mean(with.disease.distribution))
    updateSliderInput(session, "disease_spread", value = sd(with.disease.distribution))
    updateSliderInput(session, "no_disease_mean", value = mean(without.disease.distribution))
    updateSliderInput(session, "no_disease_spread", value = sd(without.disease.distribution))
    updateSliderInput(session, "cutoff", value = optim.distribution$par[5])
    
    plot.function(with.disease.distribution, without.disease.distribution, calculated.sensitivity, calculated.specificity, optim.distribution$par[5])
  }
  
  renderPPV.changedPop <- function() {
    library(ggplot2)
    library(cowplot)
    library(grid)
    
    plot.function <- function(prev, ppv, ppv.point) {
      data <- data.frame(x = prevalence, y = PPV)
      ppv.plot <- ggplot(data, aes(x = x, y = y)) + 
        geom_line() +
        geom_segment(x = prevalence.point, xend = prevalence.point, y = 0, yend = ppv.point, color = 'red', linetype = 'dotted') +
        geom_segment(x = 0, xend = prevalence.point, y = ppv.point, yend = ppv.point, color = 'red', linetype = 'dotted') +
        theme(plot.margin = unit(c(2,1,1,1), "lines")) + 
        annotate("text", label = paste("Positive Predictive Value: ", round(ppv.point, digits = 3), sep = ""), x = 0.5, y = Inf, vjust = -1) +
        ylim(0, 1) + 
        labs(x="Pretest Probability", y="Positive Predictive Value")
      
      gg2 <- ggplot_gtable(ggplot_build(ppv.plot))
      gg2$layout$clip[gg2$layout$name == "panel"] <- "off"
      grid.draw(gg2)
    }
    
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- dataInputFitSensSpec()$prev
    
    sensitivity <- round(calc.metrics.changedPop()[1], digits = 3)
    specificity <- round(calc.metrics.changedPop()[2], digits = 3)
    
    PPV <- calc.PPV.changedPop()
    PPV.point <- PPV[prevalence.point * length(prevalence) + 1]
    
    updateSliderInput(session, "sens", value = sensitivity)
    updateSliderInput(session, "spec", value = specificity)
    
    plot.function(prevalence, PPV, PPV.point)
  }
  
  renderPPV.constantPop <- function() {
    library(ggplot2)
    library(cowplot)
    library(grid)
    
    plot.function <- function(prev, ppv, ppv.point) {
      data <- data.frame(x = prevalence, y = PPV)
      ppv.plot <- ggplot(data, aes(x = x, y = y)) + 
        geom_line() +
        geom_segment(x = prevalence.point, xend = prevalence.point, y = 0, yend = ppv.point, color = 'red', linetype = 'dotted') +
        geom_segment(x = 0, xend = prevalence.point, y = ppv.point, yend = ppv.point, color = 'red', linetype = 'dotted') +
        theme(plot.margin = unit(c(2,1,1,1), "lines")) + 
        annotate("text", label = paste("Positive Predictive Value: ", round(ppv.point, digits = 3), sep = ""), x = 0.5, y = Inf, vjust = -1) +
        ylim(0, 1) + 
        labs(x="Pretest Probability", y="Positive Predictive Value")
      
      gg2 <- ggplot_gtable(ggplot_build(ppv.plot))
      gg2$layout$clip[gg2$layout$name == "panel"] <- "off"
      grid.draw(gg2)
    }
    
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- dataInputFitSensSpec()$prev
    
    PPV <- calc.PPV.constantPop()
    PPV.point <- PPV[prevalence.point * length(prevalence) + 1]
    
    plot.function(prevalence, PPV, PPV.point)
  }
  
  renderNPV.changedPop <- function() {
    library(ggplot2)
    library(cowplot)
    library(grid)
    
    plot.function <- function(prev, npv, npv.point) {
      data <- data.frame(x = prevalence, y = NPV)
      npv.plot <- ggplot(data, aes(x = x, y = y)) + 
        geom_line() +
        geom_segment(x = prevalence.point, xend = prevalence.point, y = 0, yend = npv.point, color = 'red', linetype = 'dotted') +
        geom_segment(x = 0, xend = prevalence.point, y = npv.point, yend = npv.point, color = 'red', linetype = 'dotted') +
        theme(plot.margin = unit(c(2,1,1,1), "lines")) + 
        annotate("text", label = paste("Negative Predictive Value: ", round(npv.point, digits = 3), sep = ""), x = 0.5, y = Inf, vjust = -1) +
        ylim(0, 1) + 
        labs(x="Pretest Probability", y="Negative Predictive Value")
      
      gg2 <- ggplot_gtable(ggplot_build(npv.plot))
      gg2$layout$clip[gg2$layout$name == "panel"] <- "off"
      grid.draw(gg2)
    }
    
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- dataInputFitSensSpec()$prev
    
    sensitivity <- round(calc.metrics.changedPop()[1], digits = 3)
    specificity <- round(calc.metrics.changedPop()[2], digits = 3)
    
    NPV <- calc.NPV.changedPop()
    NPV.point <- NPV[prevalence.point * length(prevalence) + 1]
    
    updateSliderInput(session, "sens", value = sensitivity)
    updateSliderInput(session, "spec", value = specificity)
    
    plot.function(prevalence, NPV, NPV.point)
  }
  
  renderNPV.constantPop <- function() {
    library(ggplot2)
    library(cowplot)
    library(grid)
    
    prevalence <- seq(0, 1, by = 0.01)
    prevalence.point <- dataInputChangedSensSpec()$prev
    NPV <- calc.NPV.constantPop()
    
    plot.function <- function(prev, npv, npv.point) {
      data <- data.frame(x = prevalence, y = NPV)
      npv.plot <- ggplot(data, aes(x = x, y = y)) + 
        geom_line() +
        geom_segment(x = prevalence.point, xend = prevalence.point, y = 0, yend = npv.point, color = 'red', linetype = 'dotted') +
        geom_segment(x = 0, xend = prevalence.point, y = npv.point, yend = npv.point, color = 'red', linetype = 'dotted') +
        theme(plot.margin = unit(c(2,1,1,1), "lines")) + 
        annotate("text", label = paste("Negative Predictive Value: ", round(npv.point, digits = 3), sep = ""), x = 0.5, y = Inf, vjust = -1) +
        ylim(0, 1) + 
        labs(x="Pretest Probability", y="Negative Predictive Value")
      
      gg2 <- ggplot_gtable(ggplot_build(npv.plot))
      gg2$layout$clip[gg2$layout$name == "panel"] <- "off"
      grid.draw(gg2)
    }
    
    NPV <- calc.NPV.constantPop()
    NPV.point <- NPV[prevalence.point * length(prevalence) + 1]
    
    plot.function(prevalence, NPV, NPV.point)
  }
  
  observeEvent(input$radio, {
    if(input$radio == 1) {
      output$prevalencePlot <- renderPlot(renderPop.constantPop())
      output$ppvPlot <- renderPlot(renderPPV.constantPop())
      output$npvPlot <- renderPlot(renderNPV.constantPop())
    }
    if(input$radio == 2) {
      output$prevalencePlot <- renderPlot(renderPop.changedPop())
      output$ppvPlot <- renderPlot(renderPPV.changedPop())
      output$npvPlot <- renderPlot(renderNPV.changedPop())
    }
  })
})
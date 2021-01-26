library(shiny)
library(tidyverse)
shinyServer(function(input, output, session) {
  counter <- reactiveValues(countervalue = 0)
  draw_sample <- eventReactive(input$dgp_button,{
    samp.size <- input$number
    is.linear <- ifelse(input$linear == "Yes", T, F)
    support <- input$common_support
    support <- ifelse(support == 'Moderate', 'mod', support)
    support <- ifelse(support == 'Strong', 'strong', support)
    support <- ifelse(support == 'Weak', 'weak', support)
    dat <- dgp(n = samp.size, lin = is.linear, com.sup = support, seed = 1 + counter$countervalue)
    return(dat)
  }, ignoreNULL = F)

  
  plt_lm <- eventReactive(input$fit_button,{
    out <- modeler(draw_sample())
    plts <- ploter(out)
    plt <- plts[[3]]
    return(plt)
  }, ignoreNULL = F)
  
  
  plt_iptw <- eventReactive(input$fit_button,{
    out <- modeler(draw_sample())
    plts <- ploter(out)
    plt <- plts[[2]]
    return(plt)
  }, ignoreNULL = F)
  
  plt_rf <- eventReactive(input$fit_button,{
    out <- modeler(draw_sample())
    plts <- ploter(out)
    plt <- plts[[4]]
    return(plt)
  }, ignoreNULL = F)
  
  
  plt_bart <- eventReactive(input$fit_button,{
    out <- modeler(draw_sample())
    plts <- ploter(out)
    plt <- plts[[1]]
    return(plt)
  }, ignoreNULL = F)
  
  output$data_draw_plot <- renderPlot({
    dat <- draw_sample()
    ggplot(dat, aes(X, y, col = factor(z))) + 
      geom_point() + 
      geom_point(alpha = .5) + 
      scale_color_manual(labels = c("Control", "Treatment"), values = c('coral3','steelblue')) + 
      scale_x_continuous(limits = c(0, 60)) +
      theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "bottom") + 
      ylim(55, 120)
    
  })
  
  output$lm_plot <- renderPlot({
    plt_lm()
    
  }, height = 700)
  
  output$iptw_plot <- renderPlot({
    plt_iptw()
    
  }, height = 700)
  
  output$rf_plot <- renderPlot({
    plt_rf()
    
  }, height = 700)
  
  
  output$bart_plot <- renderPlot({
    plt_bart()
    
  }, height = 700)
  
})
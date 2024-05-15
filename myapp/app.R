library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Normal Distribution Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("mean", label = HTML("&mu; (Mean):"), value = "0"),
      textInput("sd", label = HTML("&sigma; (Standard Deviation):"), value = "1"),
      uiOutput("lower_bound_ui"),  
      uiOutput("upper_bound_ui"),  
      checkboxInput("standard_normal", "Use Standard Normal Curve", value = TRUE),
      actionButton("update_plot", "Update Plot")
    ),
    mainPanel(
      plotOutput("norm_plot")
    )
  )
)

server <- function(input, output) {
  output$lower_bound_ui <- renderUI({
    mean_val <- if(input$standard_normal) 0 else as.numeric(input$mean)
    sd_val <- if(input$standard_normal) 1 else as.numeric(input$sd)
    sliderInput("lower_bound", "Lower Bound:", min = mean_val - 4 * sd_val, max = mean_val + 4 * sd_val, value = mean_val - sd_val)
  })
  
  output$upper_bound_ui <- renderUI({
    mean_val <- if(input$standard_normal) 0 else as.numeric(input$mean)
    sd_val <- if(input$standard_normal) 1 else as.numeric(input$sd)
    sliderInput("upper_bound", "Upper Bound:", min = mean_val - 4 * sd_val, max = mean_val + 4 * sd_val, value = mean_val + sd_val)
  })
  
  output$norm_plot <- renderPlot({
    mean_val <- 0
    sd_val <- 1
    lower_bound <- -1
    upper_bound <- 1
    
    data <- data.frame(x = c(mean_val - 4 * sd_val, mean_val + 4 * sd_val))
    p <- ggplot(data, aes(x)) +
      stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val)) +
      geom_area(stat = "function", fun = dnorm, args = list(mean = mean_val, sd = sd_val),
                xlim = c(lower_bound, upper_bound), fill = "blue", alpha = 0.5) +
      labs(title = "Standard Normal Distribution Curve") +
      theme_minimal() +
      xlab("x") + ylab("Density") +
      geom_text(aes(x = (lower_bound + upper_bound) / 2, y = dnorm((lower_bound + upper_bound) / 2, mean = mean_val, sd = sd_val)), 
                label = sprintf("Shaded Area: %.4f", diff(pnorm(c(lower_bound, upper_bound), mean = mean_val, sd = sd_val))), vjust = -1)
    
    return(p)
  })
  
  observeEvent(input$update_plot, {
    output$norm_plot <- renderPlot({
      mean_val <- if(input$standard_normal) 0 else as.numeric(input$mean)
      sd_val <- if(input$standard_normal) 1 else as.numeric(input$sd)
      
      data <- data.frame(x = c(mean_val - 4 * sd_val, mean_val + 4 * sd_val))
      p <- ggplot(data, aes(x)) +
        stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val)) +
        geom_area(stat = "function", fun = dnorm, args = list(mean = mean_val, sd = sd_val), 
                  xlim = c(input$lower_bound, input$upper_bound), fill = "blue", alpha = 0.5) +
        labs(title = "Normal Distribution Curve") +
        theme_minimal() +
        xlab("x") + ylab("Density") +
        geom_text(aes(x = (input$lower_bound + input$upper_bound) / 2, y = dnorm((input$lower_bound + input$upper_bound) / 2, mean = mean_val, sd = sd_val)), 
                  label = sprintf("Shaded Area: %.4f", diff(pnorm(c(input$lower_bound, input$upper_bound), mean = mean_val, sd = sd_val))), vjust = -1)
      
      return(p)
    })
  })
}

shinyApp(ui, server)

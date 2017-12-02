library (shiny)
library (glmnet)

source("functions.R")

data <- simulate_data()

lambda_max <- 1
lambda_min <- 0.001
lambda_by <- 0.001
lambdas <- seq(lambda_max, lambda_min, by = -lambda_by)

preds <- predict_shrunk(lambdas, data)

server <- function (input, output) {
  
  output$lambda_slider <- renderUI({
    
    max <- switch(input$type,
                  ridge = lambda_max,
                  lasso = lambda_max / 10)
    
    sliderInput("lambda",
                "lambda:",
                min = lambda_min,
                max = max,
                value = lambda_min,
                step = lambda_by)
    
  })
  
  output$plot <- renderPlot(
    plot_shrunk(input, lambdas, data, preds),
    height = reactive(ifelse(!is.null(input$innerWidth),
                             input$innerWidth * 0.5,
                             0))
  )
  
  output$beta_plot <- renderPlot(
    plot_betas(input, lambdas, data, preds)
  )
 
}

shinyServer(server)

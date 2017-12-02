library (shiny)
library (mvtnorm)
library (glmnet)
  
source("functions.R")

data <- simulate_data()
fit <- fit_ls(data)
density <- density_surface(fit)

lambda_max <- 4
lambda_min <- 0.01
lambda_by <- 0.01
lambdas <- seq(lambda_max, lambda_min, by = -lambda_by)

ridge <- plotting_data(lambdas, "ridge")
lasso <- plotting_data(lambdas, "lasso")


server <- function (input, output) {
  
  output$lambda_slider <- renderUI({
    max <- switch(input$type,
                  ridge = lambda_max,
                  lasso = lambda_max / 4)
    sliderInput("lambda",
                "lambda:",
                min = lambda_min,
                max = max,
                value = 0.5,
                step = lambda_by)
  })
  
  output$plot <- renderPlot(plot_all(input),
                            height = reactive(ifelse(!is.null(input$innerWidth),
                                                     input$innerWidth * 0.5,
                                                     0)),
                            width = reactive(ifelse(!is.null(input$innerWidth),
                                                    input$innerWidth * 0.5,
                                                    0)))
}

shinyServer(server)

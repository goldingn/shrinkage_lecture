library(shiny)
shinyUI(
  
  fluidPage(
    
    titlePanel(" "),
    
    sidebarLayout(
      
      sidebarPanel(
        tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
        radioButtons("type",
                     "model type:",
                     choices = c("ridge", "lasso")),
        uiOutput("lambda_slider"),
        br(),
        br(),
        br(),
        h4("constraint", style = "color:red"),
        h4("density of likelihood", style = "color:gray"),
        h4("contour of likelihood equivalent to solution", style = "color:blue")
        
      ),

      mainPanel(
        plotOutput("plot", width = "500px", height = "500px")
      )
      
    )
  )
)
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

        plotOutput("beta_plot")
        
      ),
      
      mainPanel(
        plotOutput("plot")
      )
      
    )
  )
)

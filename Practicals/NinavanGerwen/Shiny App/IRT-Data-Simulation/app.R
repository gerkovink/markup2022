#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

## Functions to generate the data
one.pl <- function(theta, beta){
  prob <- exp(theta - beta) / (1 + exp(theta - beta))
  return(prob)
}

two.pl <- function(theta, alpha, beta){
  prob <- exp((alpha * theta) + beta) / (1 + exp((alpha * theta) + beta))
  return(prob)
}

three.pl <- function(theta, alpha, beta, gamma){
  prob <- gamma + (1 - gamma) * (exp((alpha * theta) + beta) / (1 + exp((alpha + theta) + beta)))
  return(prob)
}

data.gen <- function(n, k, model = "1PL", alpha, gamma, beta){
  
  theta <- matrix(data = rep(rnorm(n), k), nrow = n, ncol = k)
  
  if(model == "1PL"){
    Z <- one.pl(theta = theta, beta = beta)
    
  }
  if(model == "2PL"){
    
    Z <- two.pl(theta = theta, alpha = alpha, beta = beta)
    
  }
  if(model == "3PL"){
    
    Z <- three.pl(theta = theta, alpha = alpha, beta = beta, gamma = gamma)
  }
  
  data <- matrix(data = rbinom(n = n * k, size = 1, prob = Z), ncol = k, nrow = n)
  
  return(data)
  
}

## Conditional UI tabsetting
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("1PL"),
  tabPanel("2PL",
           numericInput("alpha", "alpha", value = 1)
  ),
  tabPanel("3PL",
           numericInput("alpha", "alpha", value = 1),
           numericInput("gamma", "gamma", value = .25)
  )
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dichotomous Questionnaire Data Simulation"),
    
    titlePanel(tags$h5("Hi there! With this Shiny App, you can very easily 
                       simulate dichotomous questionnaire data according to your 
                       own needs. Below you can change values such as the number 
                       of items the questionnaire consists of, the sample size 
                       and the model type. Please note that currently, you can
                       only change the model's parameter values for all items and not
                       for each individual item. Furthermore, difficulty
                       parameter values are randomly sampled from a standard
                       normal distribution. Please enjoy!")),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("item",
                        "Number of items:",
                        min = 1,
                        max = 50,
                        value = 30
                        ),
            sliderInput("ss",
                        "Number of observations:",
                        min = 10,
                        max = 1000,
                        value = 500,
                        step = 5
                        ),
            selectInput("model",
                        "Model type:",
                        choices = c("1PL", "2PL", "3PL")
                        ),
            parameter_tabs,
            actionButton("action", "Update the parameters")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("table"),
           
           downloadButton('download', "Download the simulated data")
        )
    )
    

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$model, {
    updateTabsetPanel(inputId = "params", selected = input$model)
  })
  
  
  beta_vals <- reactive({
    matrix(data = rep(rnorm(as.numeric(input$ss)), as.numeric(input$item)), 
                      ncol = as.numeric(input$item))
  })
  
  # beta_vals <- eventReactive(input$action, {
  #   beta_vals <- matrix(data = rep(rnorm(as.numeric(input$ss)), as.numeric(input$item)),
  #          ncol = as.numeric(input$item))
  # 
  # })

  alpha_vals <- reactive({
    if(input$model == "1PL"){
      alpha_vals <- matrix(data = 1, nrow = as.numeric(input$ss), ncol = as.numeric(input$item))
    } else {
      alpha_vals <- matrix(data = as.numeric(input$alpha), nrow = as.numeric(input$ss), 
                           ncol = as.numeric(input$item))
    }
  })
  
  # alpha_vals <- eventReactive(input$action, {
  #   if(input$model == "1PL"){
  #   alpha_vals <- matrix(data = 1, nrow = as.numeric(input$ss), ncol = as.numeric(input$item))
  #   } else {
  #   alpha_vals <- matrix(data = as.numeric(input$alpha), nrow = as.numeric(input$ss), 
  #          ncol = as.numeric(input$item))
  #   }
  # })
  
  gamma_vals <- reactive({
    if(input$model == "3PL"){
      gamma_vals <- matrix(data = as.numeric(input$gamma), nrow = as.numeric(input$ss), ncol = 
                             as.numeric(input$item))
    } else {
      gamma_vals <- matrix(data = 0, nrow = as.numeric(input$ss), ncol = as.numeric(input$item))
    }
  })
  

  
  # gamma_vals <- eventReactive(input$action, {
  #   if(input$model == "3PL"){
  #     gamma_vals <- matrix(data = as.numeric(input$gamma), nrow = as.numeric(input$ss), ncol = 
  #                            as.numeric(input$item))
  #   } else {
  #     gamma_vals <- matrix(data = 0, nrow = as.numeric(input$ss), ncol = as.numeric(input$item))
  #   }
  # })
  
  thedata <- eventReactive(input$action, {
    thedata <- data.gen(n = as.numeric(input$ss), k = as.numeric(input$item), model = 
               as.character(input$model), 
             alpha = alpha_vals(), gamma = gamma_vals(), beta = beta_vals())
  })

  # Name it as our output in the way we want it
  output$table <- renderDataTable(as.data.frame(thedata())
  )
  
  output$download <- downloadHandler(
    filename = function(){"simirtdata.csv"},
    content = function(fname){
      write.csv(thedata(), fname)
    }
  
    # generate IRT data based on the inputs through reactive coding
   ## irt.data <- data.gen(n = input$ss, k = input$item,
    ##                     model = input$model)
  

    )
}

# Run the application 
shinyApp(ui = ui, server = server)

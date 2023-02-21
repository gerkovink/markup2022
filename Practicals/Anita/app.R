
library(shiny)

gala<-faraway::gala
  
ui <- navbarPage(title="Multiple Linear Regression",
                h4("This shiny app can perform multiple linear regression with any combination of the variables as outcome and predictors."), 
                 sidebarLayout(
                   sidebarPanel(width = 4,
                                radioButtons("outcome_input",
                                             "Outcome", 
                                             choices = colnames(gala)),
                                uiOutput(outputId = "dynamic_vars_input")
                                
                   ),
                   mainPanel(width = 8,
                             verbatimTextOutput("regr"),
                             verbatimTextOutput("test")
                             
                   )
                 )
  
)

server <- function(input, output, session) {
  
  output$test<-renderPrint({colnames(gala)!=input$outcome_input})
  ### Dynamic variable choice ----
  output$dynamic_vars_input<-renderUI({
    if(!is.null(input$outcome_input)){
      preds<-colnames(gala)[colnames(gala)!=input$outcome_input]
      
      checkboxGroupInput(inputId = "vars_input",
                         label = "Predictors",
                         choices = preds)
    }
    
    
                         
  })
  
  output$regr<-renderPrint({
    dat<-subset(gala, select = c(input$outcome_input, input$vars_input))
    mod<-lm(eval(parse(text = input$outcome_input))~., data = dat)
    summary(mod)
    
  })
}

shinyApp(ui, server)
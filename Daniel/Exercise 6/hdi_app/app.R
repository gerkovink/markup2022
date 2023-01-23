library(shiny)
library(shinyWidgets)

data <- read.csv('data/hdi.csv')

ui <- fluidPage(
  
  titlePanel("Human Development Index"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      pickerInput("selectDev","Country Development Level",choices = c(unique(data$Development_level)), options = list(`actions-box` = TRUE), multiple = T,selected = "Very high"),
      pickerInput("selectIndicator","Indicator",choices = c(colnames(data)[c(3:8)]), options = list(`actions-box` = TRUE), multiple = F)
    ), mainPanel(
      plotOutput("hist")
    )
  )
  
)

server <- function(input, output) {
  
  # data download

  datasetInput <- reactive({
     data_subset <- data[data$Development_level %in% input$selectDev,input$selectIndicator]
  })

  output$hist <- renderPlot({
    hist(datasetInput(),main = input$selectIndicator,xlab="")
  })
}

shinyApp(ui, server)
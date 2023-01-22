# Install and load Shiny
#install.packages("shiny")
library(shiny)

# Define the user interface
ui <- fluidPage(
  fluidRow(
    column(12, align = "center",
      
      h1("Make Michael say things."),
      br(),br(),
      
      # Put nice image
      tags$img(src = "michael.png", width = "700px", height = "500px"),
      br(), 
      div(style="font-size:10px;","(C) 2005 NBCUniversal Media, LLC"),
      br(),br(),
      
      # Create a button that users can click to generate a quote
      actionButton("generateQuote", "Generate Quote"),
      br(),br(),
      
      # Create an output area where the generated quote will be displayed
      h4(textOutput("quoteOutput"))
    )
  )
)

# Define the server code
server <- function(input, output) {
  # Read vector of quotes to choose from
  quotes <- readLines("www/quotes.txt", encoding = "UTF-8")

  # Create a reactive expression that generates a random quote
  quote <- reactive({
    # Check if the generate quote button has been clicked
    if (input$generateQuote == 0) return(NULL)

    # Generate a random quote
    quotes[sample(1:length(quotes), size = 1)]
  })

  output$quoteOutput <- renderText({
     quote()
  })
 }

 # Run the app
 shinyApp(ui, server)

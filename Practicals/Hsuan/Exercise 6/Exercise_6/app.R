library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Petal Length Distribution of Three Distinct Species of Iris"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Species",
                        label = "Choose a Species to See Its Petal Length Distribution :",
                        choices = list("Setosa", 
                                       "Versicolor",
                                       "Virginica"),
                        selected = "Setosa")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        if(input$Species == "Setosa") {
          iris %>%
            filter(Species == "setosa") %>%
            ggplot(aes(x = Petal.Length)) +
            geom_bar()
        } else if (input$Species == "Versicolor") {
          iris %>%
            filter(Species == "versicolor") %>%
            ggplot(aes(x = Petal.Length)) +
            geom_bar()
        } else {
          iris %>%
            filter(Species == "virginica") %>%
            ggplot(aes(x = Petal.Length)) +
            geom_bar()}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Author: Shannon Dickson
# Purpose: Markup 2022 Week 6 Exercise

# dependencies
library(shiny)
library(dplyr)
library(readr)
library(shinythemes)

sleep <- read_csv("sleep.csv", show_col_types = FALSE)

#  define ui 
ui <- fluidPage(theme = shinytheme("yeti"),

    # application title
    titlePanel("What lifestyle choices correlate with sleep quality?"),

        # user input options - predictor
        sidebarPanel(
            selectInput("Predictor",
                        "What do you think affects sleep?",
                        choices = list("Daily steps"    = "step_count",
                                       "Daily activity (minutes)" = "active_mins",
                                       "Screentime (minutes)"     = "screen_mins"),
                        selected = NULL,
                        multiple = FALSE),
            
            selectInput("Outcome",
                        "What outcome are you interested in?",
                        choices = list("Sleep duration (minutes)" = "sleep_duration",
                                        "Sleep latency (minutes)" = "sleep_onset"),
                        selected = NULL,
                        multiple = FALSE)),
            
        mainPanel(plotOutput("plot"))
)

# define the output scatterplot
server <- function(input, output, session) {
    
    df <- reactive({sleep[, c(input$Predictor, input$Outcome)]})
    output$plot <- renderPlot({plot(df(), pch = 21, cex = 1, alpha = 0.6, col = "darkseagreen")})
}

# host the output on shinyapps.io 
shinyApp(ui = ui, server = server)

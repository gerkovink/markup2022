# load required libraries
library(tidyr)
library(data.table)
library(shiny)
library(maps)
library(mapproj)
source("helpers.R")

# data preprocessing
data <- read.csv("data/mort.csv", header=TRUE)
data <- subset(data, select=c("Location", "Category", "Mortality.Rate..2014."))

data_wide <- spread(data, Category, Mortality.Rate..2014.)
data_wide <- data_wide[data_wide$Location %like% ",", ]
data_wide$Location <- tolower(data_wide$Location)
data_wide <- data_wide %>% 
  separate(Location, c("County","State"), sep=",")
data_wide$County <- gsub(' county','',data_wide$County)
data_wide$location <- paste(data_wide$State, data_wide$County, sep=",")
data_wide <- data_wide[,-(1:2)]
data_wide$total <- rowSums(data_wide[,(1:21)])
data_wide[,(1:21)] <- data_wide[,(1:21)]/data_wide$total*100
head(data_wide)
  
# user interface
ui <- fluidPage(
  titlePanel("Causes of Death in the United States in 2014"),
  
  sidebarLayout(
    sidebarPanel(
      p("Display maps with information on the causes of death in the US in 2014. 
        Values are presented as percentages calculated of the total amount of deaths
        within each county. The Data is retrieved from the", 
        a("Institute for Health Metrics and Evaluation.",
          href="https://www.healthdata.org/")),
      
      selectInput("var", 
                  label = "Cause of Death",
                  choices = c("Cardiovascular diseases", 
                              "Chronic respiratory diseases",
                              "Cirrhosis and other chronic liver diseases", 
                              "Diabetes, urogenital, blood, and endocrine diseases",
                              "Diarrhea, lower respiratory, and other common infectious diseases",
                              "Digestive diseases",
                              "Forces of nature, war, and legal intervention",
                              "HIV/AIDS and tuberculosis",
                              "Maternal disorders",
                              "Mental and substance use disorders",
                              "Musculoskeletal disorders",
                              "Neglected tropical diseases and malaria",
                              "Neonatal disorders",
                              "Neoplasms",
                              "Neurological disorders",
                              "Nutritional deficiencies",
                              "Other communicable, maternal, neonatal, and nutritional diseases",
                              "Other non-communicable diseases",
                              "Self-harm and interpersonal violence",
                              "Transport injuries",
                              "Unintentional injuries"),
                  selected = "Cardiovascular diseases"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# server logic
server <- function(input, output) {
  output$map <- renderPlot({
    args <- switch(input$var,
                   "Cardiovascular diseases" = list(data_wide$`Cardiovascular diseases`, "darkblue", "Percent of total amount of deaths"),
                   "Chronic respiratory diseases" = list(data_wide$`Chronic respiratory diseases`, "darkblue", "Percent of total amount of deaths"),
                   "Cirrhosis and other chronic liver diseases" = list(data_wide$`Cirrhosis and other chronic liver diseases`, "darkblue", "Percent of total amount of deaths"),
                   "Diabetes, urogenital, blood, and endocrine diseases" = list(data_wide$`Diabetes, urogenital, blood, and endocrine diseases`, "darkblue", "Percent of total amount of deaths"),
                   "Diarrhea, lower respiratory, and other common infectious diseases" = list(data_wide$`Diarrhea, lower respiratory, and other common infectious diseases`, "darkblue", "Percent of total amount of deaths"),
                   "Digestive diseases" = list(data_wide$`Digestive diseases`, "darkblue", "Percent of total amount of deaths"),
                   "Forces of nature, war, and legal intervention" = list(data_wide$`Forces of nature, war, and legal intervention`, "darkblue", "Percent of total amount of deaths"),
                   "HIV/AIDS and tuberculosis" = list(data_wide$`HIV/AIDS and tuberculosis`, "darkblue", "Percent of total amount of deaths"),
                   "Maternal disorders" = list(data_wide$`Maternal disorders`, "darkblue", "Percent of total amount of deaths"),
                   "Mental and substance use disorders" = list(data_wide$`Mental and substance use disorders`, "darkblue", "Percent of total amount of deaths"),
                   "Musculoskeletal disorders" = list(data_wide$`Musculoskeletal disorders`, "darkblue", "Percent of total amount of deaths"),
                   "Neglected tropical diseases and malaria" = list(data_wide$`Neglected tropical diseases and malaria`, "darkblue", "Percent of total amount of deaths"),
                   "Neonatal disorders" = list(data_wide$`Neonatal disorders`, "darkblue", "Percent of total amount of deaths"),
                   "Neoplasms" = list(data_wide$`Neoplasms`, "darkblue", "Percent of total amount of deaths"),
                   "Neurological disorders" = list(data_wide$`Neurological disorders`, "darkblue", "Percent of total amount of deaths"),
                   "Nutritional deficiencies" = list(data_wide$`Nutritional deficiencies`, "darkblue", "Percent of total amount of deaths"),
                   "Other communicable, maternal, neonatal, and nutritional diseases" = list(data_wide$`Other communicable, maternal, neonatal, and nutritional diseases`, "darkblue", "Percent of total amount of deaths"),
                   "Other non-communicable diseases" = list(data_wide$`Other non-communicable diseases`, "darkblue", "Percent of total amount of deaths"),
                   "Self-harm and interpersonal violence" = list(data_wide$`Self-harm and interpersonal violence`, "darkblue", "Percent of total amount of deaths"),
                   "Transport injuries" = list(data_wide$`Transport injuries`, "darkblue", "Percent of total amount of deaths"),
                   "Unintentional injuries" = list(data_wide$`Unintentional injuries`, "darkblue", "Percent of total amount of deaths"),)
    
    args$min <- input$range[1]
    args$max <- input$range[2]
    
    do.call(percent_map, args)
  })
}

# run app
shinyApp(ui, server)

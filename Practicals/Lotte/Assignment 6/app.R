#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(eurostat)
library(tmap)
library(s2)
library(sf)
library(ggplot2)
library(dplyr)
migr_data <- get_eurostat("migr_imm8")
migr_data <- migr_data %>%
  filter(age == "TOTAL", sex== "T", agedef == "COMPLET")

geo_data <- st_read("https://raw.githubusercontent.com/eurostat/Nuts2json/master/pub/v2/2021/4326/20M/nutsrg_0.json", quiet = T)

migr_data_adj <- migr_data %>% 
  rename(id = geo) 

tot_data <- inner_join(geo_data, migr_data_adj, by = "id")

pop_data <- get_eurostat("tps00001")

pop_data <- pop_data %>%
  rename(id = geo) 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Immigration in the EU"),
    
    sidebarPanel(
    # Sidebar with a slider input for number of bins 
    selectInput("country", "Countries to select:",
                       tot_data$na),
    
    selectInput("year", "Years to select:",
                c("2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", 
                  "2015-01-01", "2016-01-01", "2017-01-01","2018-01-01", "2019-01-01")),
    ),
        # Show a plot of the generated distribution
        mainPanel(
          p("This Shiny app can be used to get a more detailed overview into immigration in Europe. 
            The top plot shows immigration over the years for the selected country. The bottom plot 
            shows the immigration per capita for Europe in the selected year.
            Use the drop-down menus to select the country and the year you would like to see in the plots."),
          plotOutput("timeseriesplot"),
          plotOutput("impcmap"))

)

    

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    output$timeseriesplot <- renderPlot({

        country <- input$country
        
        tot_data_country <- tot_data %>%
          filter(na == country)
        
        ggplot(tot_data_country, aes(x = time, y = values)) +
          geom_line() +
          ggtitle(paste("Immigration over time for", country)) +
          ylab("Absolute immigration counts") +
          xlab("Time")
    
    })
    
    
    output$impcmap <- renderPlot({
      
      year <- input$year
      
      tot_data <- tot_data %>%
        filter(time == year)
      
      pop_data <- pop_data %>%
        filter(time == year)
      
      tot_pop_data <- inner_join(tot_data, pop_data, by = "id")
      tot_pop_data$imm_pc <- tot_pop_data$values.x/tot_pop_data$values.y
      
      ggplot(tot_pop_data, aes(fill=imm_pc)) +
        geom_sf() + 
        coord_sf() +
        scale_fill_viridis_c() +
        ggtitle(paste("Immigration per capita per country in", year)) +
        labs(fill="Immigration per capita") 
      
    })
    
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)

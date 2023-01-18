#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(tidyverse)
require(ggplot2)


shinyUI(fluidPage(
    
    # Application title
    br(),
    titlePanel(h1("Immigration in Europe", align = "center")),
    
    #Line break
    br(), 
    
    
    
    br(), 
    br(), 
    
    # Sidebar with interactive elements
    sidebarLayout(
        sidebarPanel(
            
            # Time-series plot choices:
          
            helpText(strong("Time-series plot parameters"), style = "font-size:16px;"), 
            
            selectizeInput(
                inputId = "selectcountry",  # selectedstat
                label = "Select country:",
                choices = c("Austria","Belgium","Bulgaria","Switzerland", "Cyprus","Czechia","Germany","Denmark",
                            "Estonia","Spain","Finland","France","Great Brittain","Greece","Croatia","Hungary",
                            "Ireland","Iceland", "Italy","Liechtenstein", "Lithuania","Luxembourg","Latvia","Montenegro",    
                            "North Macedonia", "Malta","Netherlands","Norway","Poland","Portugal","Romania","Sweden", "Slovenia", "Slovakia"), 
                selected = c("Austria","Belgium","Bulgaria","Switzerland", "Cyprus","Czechia","Germany","Denmark",
                             "Estonia","Spain","Finland","France","Great Brittain","Greece","Croatia","Hungary",
                             "Ireland","Iceland", "Italy","Liechtenstein", "Lithuania","Luxembourg","Latvia","Montenegro",    
                             "North Macedonia", "Malta","Netherlands","Norway","Poland","Portugal","Romania","Sweden", "Slovenia", "Slovakia"),
                multiple = TRUE, 
            ),
          
            
            selectizeInput(
                inputId = "selectregion",  
                label = "View by region:",
                choices = c("Not by region", "By region"), # 
                selected = "Not by region", # selected first
                multiple = FALSE, # allows no multiple items to be selected 
            ), 
            
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            
            helpText(strong("Map  parameters"), style = "font-size:16px;"), 
            
            selectizeInput(
              inputId = "selectyear", 
              label = "Select year:",
              choices = c(2010:2019), # choices are all different periods
              selected = "2019", # selected first
              multiple = FALSE, # allows no multiple items to be selected 
              options = list(maxItems = 1) # limits the selection to 
            ),
            
            selectizeInput(
              inputId = "selectstat", 
              label = "Select statistic:",
              choices = c("Total number", "Per capita"), # choices are all different periods
              selected = "Per capita", # selected first
              multiple = FALSE, # allows no multiple items to be selected 
              options = list(maxItems = 1) # limits the selection to 
            ),
            
            
        ),
        
        
        
        mainPanel(
            
            
            h4("Time-series plot"), # header directly above the plot
            plotOutput("plot", width = "80%", height = 400), # the output for the plot
            
            span(textOutput("txt1"), style = "font-style:italic"),
            
            br(),
            br(),
          
            
            h4("Map of Europe"), # header directly above the plot
            plotOutput("map", width = "80%", height = 400), # the output for the plot
          
            span(textOutput("txt2"), style = "font-style:italic"),
            
            br(),
            
        )
        
)))
    
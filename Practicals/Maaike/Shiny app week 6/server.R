#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(magrittr) # Special pipes
require(dplyr) # Data manipulation
require(data.table) # data manipulation
require(ggplot2) # Plots
require(readr) # Read csv
require(ggpubr) # Arranging plots
require(RColorBrewer)
library(eurostat)
library(tmap)
library(rjson)
library(RJSONIO)
library(sf)

# Data prep

# PLOT #
imgr <- get_eurostat("migr_imm8")
imgr %<>% filter(age == "TOTAL", sex == "T", agedef == "COMPLET")

# Read region name with country codes
cc <- read_csv(url("https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv"))

# Some data adjustments 
cc$reg <- cc$`Sub-region Name` # rename vars with lengthy names
cc$land <- cc$official_name_en 
cc$land <- as.factor(cc$land)
cc$code <- cc$`ISO3166-1-Alpha-2`
# names(c)[names(cc) == "Sub-region Name"] <- "reg"
# cc <- select(cc, code, reg, land)
cc$reg[cc$code == "CY"] <- "Southern Europe"
cc$land[cc$code == "GB"] <- "Great Brittain"
cc$land[cc$code == "MK"] <- "North Macedonia"
imgr$geo[imgr$geo == "UK"] <- "GB"
imgr$geo[imgr$geo == "EL"] <- "GR"

imgr_reg <- merge(imgr, cc, by.x = "geo", by.y = "code", all.x = T) 
regions <- unique(imgr_reg$reg)


# MAP #

europe <- read_sf("https://raw.githubusercontent.com/eurostat/Nuts2json/master/pub/v2/2021/3035/20M/0.json")

# total 
imgr2 <- get_eurostat("migr_imm8") # load again
imgr2 %<>% filter(age == "TOTAL", sex == "T", agedef == "COMPLET")
eu <- merge(imgr2, europe, by.x = "geo", by.y = "id", all.x = T)
eu$year <- format(as.Date(eu$time, format = "%d/%m/%Y"), "%Y")
eu <- st_as_sf(eu)

# per capita
pop_tot <- get_eurostat("demo_gind") # obtain population totals 
n <- pop_tot %>%
  group_by("geo") %>%
  filter(indic_de == "JAN") # on january 1st 2019
names(n)[names(n) == "values"] <- "pop_size" # rename 'values'

eu_n <- merge(eu, n, by = c("geo", "time"), all.x = T) # Re-use eu data created in 5c
eu_n$im_cap <- eu_n$values / eu_n$pop_size * 100 
eu_n <- st_as_sf(eu_n)
cols <- brewer.pal(n = 9, name = "GnBu")[c(4,6,8)] 

########################### THE ACTUAL APP #################################

# Define server 
server <- function(input, output) {
    
    observe({
        
        c <- input$selectcountry 
        r <- input$selectregion
        
        # creates plot time series
        output$plot <- renderPlot({
            
          if (r == "Not by region") {
            
         # this plot plots 
         
            g1 <- filter(imgr_reg, land %in% c) %>%
              ggplot(aes(x = time, y = values)) +
              geom_line() +
              ylab("Immigration count") +
              facet_wrap(~ land) 
                
           print(g1)
       
            }
          
          if (r == "By region") {
            
            # Western Europe
           gg1 <- filter(imgr_reg, land %in% c, reg == regions[1]) %>%
              ggplot(., aes(x = time, y = values , col = land)) +
              geom_line() + 
              ylab("Immigration count") +
              scale_color_manual(values = brewer.pal(n = length(unique(imgr_reg$geo[imgr_reg$reg == regions[1]])), "Set1"))
            
            # Eastern Europe
            gg2 <- filter(imgr_reg, land %in% c, reg == regions[2]) %>%
              ggplot(., aes(x = time, y = values , col = land)) +
              geom_line() +
              ylab("Immigration count") + 
              scale_color_manual(values = brewer.pal(n = length(unique(imgr_reg$geo[imgr_reg$reg == regions[2]])), "Set2"))
            
            # Southern Europe
            gg3 <- filter(imgr_reg, land %in% c, reg == regions[3]) %>%
              ggplot(., aes(x = time, y = values , col = land)) +
              geom_line() +
              ylab("Immigration count") + 
              scale_color_manual(values = brewer.pal(n = length(unique(imgr_reg$geo[imgr_reg$reg == regions[3]])), "Paired"))
            
            # Northern Europe
            gg4 <- filter(imgr_reg, land %in% c, reg == regions[4]) %>%
              ggplot(., aes(x = time, y = values , col = land)) +
              geom_line() + 
              ylab("Immigration count") + 
              scale_color_manual(values = brewer.pal(n = length(unique(imgr_reg$geo[imgr_reg$reg == regions[4]])), "RdYlGn"))
            
            print(ggarrange(gg1, gg2, gg3, gg4, 
                      labels = c(regions[1], regions[2], regions[3], regions[4]),
                      ncol = 2, nrow = 2)) 
            
          }

    
        })
        
    })
    
    
    # Interpretation of the plot
    
    
    output$txt1 <- renderText({ 

        paste("Plots of immigration in Europe for years 1990-2020")
    
    })
    
    
    observe({
      
      y <- input$selectyear 
      s <- input$selectstat
      
      # creates map
      output$map <- renderPlot({
        
        # totals
        if (s == "Total number") {
          
        filter(eu, year == y) %>%
        tm_shape(.) +
          tm_fill("values",
                  title = "Total immigration",
                  palette = "PuRd") +
          tm_layout("Immigration",
                    legend.title.size = 1,
                    legend.text.size = 0.6,
                    legend.position = c("left","top"),
                    legend.bg.color = "white",
                    legend.bg.alpha = 1) 
          
        }
        
        if (s == "Per capita") {
          
          # per capita
          
          filter(eu_n, year == y) %>%
          tm_shape(.) +
            tm_fill("im_cap",
                    title = "Immigration per capita (in %)",
                    palette = cols) +
            tm_layout("",
                      legend.title.size = 1,
                      legend.text.size = 0.6,
                      legend.position = c("left","top"),
                      legend.bg.color = "white",
                      legend.bg.alpha = 1)
          
        }
        
        
      })
      
    })
    
    
    # Interpretation of the map
        
    observe({
      
        y <- input$selectyear 
      
        output$txt2 <- renderText({ 
        
        paste0("Map of immigration in Europe for year ", y)
        
        })
        
    })
    

}


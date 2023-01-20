#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## dependencies
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(forcats)
library(DT)
library(plotly)
library(shinythemes)

wines <- read_csv("winemag.csv") %>% 
  filter(is.na(price) == FALSE) %>% 
  filter(is.na(points) == FALSE) %>% 
  filter(is.na(country) == FALSE) %>% 
  mutate(country = as.factor(country))

nobel_grapes <- c("Chardonnay", "Gewürztraminer", "Riesling", "Moscato",
                  "Lambrusco", "Pinot Noir", "Gamay", "Sangiovese")

white <- c("Chardonnay", "Gewürztraminer", "Riesling", "Moscato") 

my_custom_stopwords <- c("wine",
                         "flavor",
                         "flavors",
                         "flavour",
                         "flavours",
                         "palate",
                         "chardonnay",
                         "riesling",
                         "cabernet",
                         "pinot",
                         "sangiovese",
                         "moscato",
                         "syrah",
                         "fruit",
                         stopwords("english"))

makeWordCloud <- function(documents, col_palette) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, my_custom_stopwords )
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[50]],
            colors=brewer.pal(8, col_palette),
            random.color= FALSE ,random.order=FALSE) 
}

wine_countries<-wines %>% 
  filter(variety %in% nobel_grapes) %>% 
  group_by(country) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 

# make colour palette 
country_vec <-unique(wine_countries$country)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
names(col_vector) <- country_vec

############################################################################################
## define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                
                # application title
                titlePanel("Grapes! "),
                br(),
                "Please select a grape",
                br(),
                
                #user inputs
                sidebarLayout(
                  sidebarPanel(
                    selectInput("varietals", "Grapes",
                                choices = nobel_grapes),
                    br()
                  ),
                  
                  
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                
                                tabPanel("Flavour Profile", plotOutput("word_cloud")),
                                
                                tabPanel("Growing Region", br(), br(), br(), br(), plotOutput("bar"))
                                
                    )
                  )
                )
)


## define server
server <- function(input, output, session) {
  
  # make word cloud
  output$word_cloud <- renderPlot({
    grape <-  wines %>% filter(variety == input$varietals)
    if (input$varietals %in% white) {
      makeWordCloud(grape[["description"]][1:50], "Greens")
    }
    else {
      makeWordCloud(grape[["description"]][1:50], "Reds")
    }
    
  })
  
  
  # make bar plot quantity
  output$bar <- renderPlot({
    grape <- wines %>%  
      filter(variety == input$varietals) %>% 
      group_by(province, country) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)) %>% 
      head(6) %>% 
      droplevels()
    
    ggplot(grape, aes(x = fct_reorder(province, count, max), y = count)) +
      geom_col(aes(fill = country), alpha = 0.8)+
      coord_flip() +
      scale_fill_manual(name = "country", values = col_vector) +
      labs(x = "region", title = "Top 6 Regions:  Quantity") +
      theme_minimal()
  })
}
## run application
shinyApp(ui = ui, server = server)
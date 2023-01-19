## load necessary packages
library(readxl)
library(tidyverse)
library(lubridate)
library(shiny)
library(bslib)
library(DT)

## load data
dat <- read_excel("data/chat_analyze.xlsx")

## clean the data
dat <- dat %>%
  mutate(Time = format(Time, "%H:%M %p"),
         AHTm = as.numeric(AHTm),
         Employee = gsub("([A-Z])[^A-Z]+", "\\1", Employee),
         Category = tolower(Category)) 



######## Shiny App ########

ui <- navbarPage(title =  div(img(src = "examplelogo.png", id = "yource", height = "40px",width = "100px",style = "position: relative; margin:-10px 15px; display:right-align;", "By Kyuri Park")), 
                 
                 theme = bs_theme(bootswatch = "flatly"),
                 
                 tabPanel("Main Analysis",
                          headerPanel(h2("Chat Usage in 2022")),
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput("Employee", label="Employee Name:", choices = list("ALL" = "all", "Sheldon El Geden" = "SEG", "Peter Campbell" = "PC", "Laura Watson" = "LW", "Chris Gomez" = "CG", "Esmeralda Waning" ="EW",  "Christel Palasuka" ="CP" , "Souhaila Afenich" = "SA",  "Golud Jadroun" ="GJ", "Dianne Spearson" ="DS", "Romeo Benzo"="RB" , "Ian Ingmar" ="II",  "Rachel Nuebo" ="RN",  "Lucy Ganne"= "LG", "Sara Oesterman" ="SO", "Dennis Jackson" = "DJ", "Sherry Cashew" ="SC", "Kate Ohsehhen" ="KO",  "Michael Onnet"= "MO",  "Caroline Hasher" = "CH", "Salman Abraken" ="SA",  "Yuri Ahnson" ="YA", "Michael Bennet" = "MB", "Emanuella Modick" = "EM", "Lara Soloman" ="LS"),options = list(maxItems = 5, placeholder = 'Select a name'), selected = "all"),
                              
                              radioButtons("choicevar", "Choose a variable to display:", choices= list("N.A." = "NA", "Category" = "Cat", "Missing System" = "MS"), selected = NULL),
                              
                              selectInput("Team", label="Team:",
                                          choices = c("ALL", unique(dat$Team)), selected="all"),
                              
                              checkboxInput("checkbox", label="Compare Teams", value=FALSE),
                              
                              selectInput("Date", label = "Date:", 
                                          choices = list("ALL" = "all", 
                                                         "2022-07-18"= "2022-07-18 UTC",
                                                         "2022-07-19" = "2022-07-19 UTC" , 
                                                         "2022-07-20" = "2022-07-20 UTC", 
                                                         "2022-07-21" = "2022-07-21 UTC" , 
                                                         "2022-07-22"="2022-07-22 UTC"), selected = "all"),
                              hr(style="border-color: grey;"),
                              helpText(HTML("&copy;2022 Kyuri Park <br>"),
                                       HTML("You can find the source code in my <a href='https://github.com/KyuriP/Shiny_Yource'>github</a>.")),
                            ),
                            
                            mainPanel(
                              plotOutput("result"),
                              textOutput("text")
                            ),
                          )
                 ),
                 
                 tabPanel("Daily Volumn",
                          headerPanel(h2("Analysis per Hour")),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("Dates", label = "Date:", 
                                          choices = c("Total", unique(as.character(dat$Date)))),
                            ),
                            mainPanel(
                              plotOutput("daily")
                            ),
                          ),
                 ),
                 tabPanel("Coaching Analysis",
                          headerPanel(h2("Coaching data")),
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput("Coach", label="Coach:", choices = c("ALL", unique(dat$Coach))),
                              checkboxInput("checkbox2", label="Compare Coaches", value=FALSE),
                            ),
                            mainPanel(
                              plotOutput("coach")
                            ),
                          ),
                 ),
                 
                 tabPanel("Data Summary",
                          DT::dataTableOutput("table")
                 )
)

server <- function(input, output, session) {
  
  # input data
  dataset1 <- reactive({dat %>%
      filter(if(any(input$Employee %in% "all")) TRUE else Employee %in% input$Employee) %>%
      filter(if(input$Date != "all") Date == as.POSIXct(input$Date, tz= "UCT") else TRUE) %>% 
      filter(if(input$Team != "ALL") Team == input$Team else TRUE) 
  })
  
  dataset2 <- reactive({dat %>%
      filter(Coach != "FALSE") %>% 
      filter(if(input$Coach != "ALL") Coach == input$Coach else TRUE) 
  })
  
  dataset3 <- reactive({dat %>% 
      mutate(Dates = as.Date(Date), Hour = hour(hm(Time))) %>% 
      add_count(Hour, name = "totaln") %>% 
      add_count(Dates , Hour, name = "dailyn") %>%
      group_by(Dates) %>% 
      complete(Hour = 7:24, fill = list(dailyn=0, totaln=0)) %>% 
      select(Dates, Hour, dailyn, totaln) %>% 
      filter(if(input$Dates != "Total") Dates == input$Dates else TRUE) 
  })
  
  
  MyTheme <- theme_classic() + theme(axis.title.x = element_text(size = 15, vjust= -3),
                                     axis.title.y = element_text(size = 15, vjust= 3),
                                     axis.text.x = element_text(size=14, angle = 80, vjust = .5, hjust = .5),
                                     axis.text.y = element_text(size=14),
                                     plot.title = element_text(face = "bold", vjust=2, size=20),
                                     legend.text = element_text(size=14),
                                     legend.title = element_text(size=14),
                                     plot.margin = margin(4, 1, 15, 5)
  )
  
  
  output$text <- renderText({paste("This is for", input$Team, "team(s).")})
  output$result <- renderPlot({
    
    if (input$choicevar== "Cat"){
      if (input$checkbox == TRUE){
        p <- dataset1() %>% 
          ggplot(aes(x=Category, fill=Team)) + geom_bar()+
          labs(title="Quesitons per category") + 
          MyTheme + scale_y_continuous(expand = expansion(c(0, 0.1))) 
      } else {
        
        p <- dataset1() %>% ggplot(aes(x=Category)) + geom_bar()+ 
          geom_text(stat='count', aes(label=..count..), vjust=-1) + 
          labs(title="Quesitons per category") +
          MyTheme + scale_y_continuous(expand = expansion(c(0, 0.1)))         
      }
    } 
    if(input$choicevar == "MS"){
      if (input$checkbox == TRUE){
        p <- dataset1() %>% 
          ggplot(aes(x=`Missing System`, fill=Team)) + geom_bar() +
          labs(title="Systems that are not utilized") + 
          MyTheme + scale_y_continuous(expand = expansion(c(0, 0.1))) 
      } else {
        p <-  dataset1() %>% ggplot(aes(x=`Missing System`)) +
          geom_bar(position= "dodge2") +
          geom_text(stat='count', aes(label=..count..), vjust=-1) +
          labs(title="Systems that are not utilized") + MyTheme + 
          scale_y_continuous(expand = expansion(c(0, 0.1)))     
      }
      
    }
    if(input$choicevar == "NA"){
      if(input$checkbox == TRUE){
        p <- dataset1() %>% 
          ggplot(aes(x=Date, fill=Team)) +
          labs(title="Number of questions asked") + 
          MyTheme + scale_y_continuous(expand = expansion(c(0, 0.1))) +
          geom_bar(position = position_dodge2(width = 0.9, preserve = "single"))
        
      } else {
        p <- dataset1() %>% 
          ggplot(aes(x=Date)) + geom_bar(position="dodge2") + 
          geom_text(aes(label = ..count..), stat = "count",vjust=-1) +
          labs(title="Number of questions asked") + 
          MyTheme + scale_y_continuous(expand = expansion(c(0, 0.1)))  
      }
    }
    p
  })
  
  output$daily <- renderPlot({
    if(input$Dates == "Total"){
      dataset3() %>% ungroup() %>% 
        select(Hour, totaln) %>%
        distinct(Hour, .keep_all=T) %>% 
        ggplot(aes(x=Hour, y = totaln)) +
        geom_line(color="darkslateblue", size=1) + geom_point(color="darkblue") + MyTheme + 
        scale_x_continuous(breaks = 7:24, labels=paste0(7:24, "h")) +
        labs(title="Number of questions throughout the total period", y = "")
    } else {
      dataset3() %>% 
        ggplot(aes(x=Hour, y = dailyn)) +
        geom_line(color="darkslateblue", size=1) + geom_point(color="darkblue") +
        MyTheme + scale_x_continuous(breaks = 7:24, labels=paste0(7:24, "h"))+
        labs(title=paste("Number of questions on", input$Dates), y="")
      
    }
  })
  
  output$coach <- renderPlot({
    if (input$checkbox2 == TRUE){
      p <- dataset2() %>% 
        ggplot(aes(x=Help, fill = Coach)) + geom_bar(poisiont="dodge2") + 
        MyTheme + labs(title = "Compare coaches")
    } else {
      p <- dataset2() %>% 
        ggplot(aes(x=Help)) + geom_bar(poisiont="dodge2") + 
        geom_text(aes(label = ..count..), stat = "count",vjust=-1) +
        MyTheme + labs(title = input$Coach) +
        scale_y_continuous(expand = expansion(c(0, 0.1)))  
    }
    p
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(dat)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

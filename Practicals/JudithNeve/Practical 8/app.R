library(shiny)
library(tidyverse)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("How do different hyperparameter combinations impact model predictive performance?"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      checkboxGroupInput(
        inputId = "measure",
        label = "Performance measure",
        choices = list(
          "Discrimination (AUC)" = "AUC",
          "Calibration (slope)" = "CalibrationSlope",
          "Calibration (intercept)" = "CalibrationIntercept",
          "Brier score" = "BrierScore",
          "Logarithmic loss" = "LogarithmicLoss",
          "Accuracy" = "Accuracy",
          "Cohen's Kappa" = "CohensKappa"
         ),
        selected = "CalibrationSlope"),
      radioButtons(
        inputId = "unit",
        label = "Unit of time",
        choices = list(
          "Seconds" = "seconds",
          "Minutes" = "minutes",
          "Hours" = "hours"))
      
    ),
    
    mainPanel(
      plotOutput(outputId = "perfplot")
    )
  )
)

server <- function(input, output) {
  load("appdata.RData")
  dat$`Tuned hyperparameters` <- ifelse(dat$`Tuned hyperparameters` == "", "None", dat$`Tuned hyperparameters`)
  df_scale_setter <- tibble(
    Accuracy = rep(c(0, 1), each = 9),
    AUC = rep(c(0.5, 1), each = 9),
    BrierScore = rep(c(0, 1), each = 9),
    CalibrationIntercept = rep(c(-1, 1), each = 9),
    CalibrationSlope = rep(c(0, 2), each = 9),
    CohensKappa = rep(c(0, 1), each = 9),
    LogarithmicLoss = rep(c(0, 1), each = 9),
    Runtime = 0,
    `Tuned hyperparameters` = rep(unique(dat$`Tuned hyperparameters`), 2)
  ) %>%
    pivot_longer(Accuracy:LogarithmicLoss,
                 names_to = "Metric",
                 values_to = "Performance") %>% 
    mutate(Metric = factor(Metric, levels = c(
      "Accuracy", "AUC", "CalibrationSlope", "BrierScore",
      "CalibrationIntercept", "LogarithmicLoss", "CohensKappa")))
  
  Metric.labs <- c("Calibration slope", "Calibration intercept", "Brier score",
                   "Logarithmic loss", "Cohen's kappa", "AUC", "Classification accuracy")
  names(Metric.labs) <- c("CalibrationSlope", "CalibrationIntercept", "BrierScore",
                          "LogarithmicLoss", "CohensKappa", "AUC", "Accuracy")
  # change labels so the plot looks nice
  HP.labs <- unique(dat$`Tuned hyperparameters`)
  names(HP.labs) <- HP.labs
  names(HP.labs)[2] <- "mtry + sample.fraction + replace\n+ min.node.size + splitrule"
  HP.labs <- c(HP.labs[1], sort(HP.labs[2:9]))
  # define the colour palette
  HPcomb_pal <- viridis::turbo(n = 9)
  df_targets <- tibble(
    Accuracy = 1,
    AUC = 1,
    BrierScore = 0,
    CalibrationIntercept = 0,
    CalibrationSlope = 1,
    CohensKappa = 1,
    LogarithmicLoss = 0,
    Runtime = 0,
    `Tuned hyperparameters` = unique(dat$`Tuned hyperparameters`)
  ) %>%
    pivot_longer(Accuracy:LogarithmicLoss,
                 names_to = "Metric",
                 values_to = "Target") %>% 
    mutate(Metric = factor(Metric, levels = c("Accuracy", "AUC", "CalibrationSlope",
                                              "BrierScore", "CalibrationIntercept", "LogarithmicLoss",
                                              "CohensKappa")))
  
  output$perfplot <- renderPlot({
    
    dat %>%
      mutate(Runtime = ifelse(rep(input$unit == "minutes", nrow(dat)), Runtime,
                              ifelse(rep(input$unit == "hours", nrow(dat)), Runtime/60,
                                     Runtime*60))) %>% 
      pivot_longer(AUC:CohensKappa,
                   names_to = "Metric",
                   values_to = "Performance") %>%
      mutate(Metric = factor(Metric, levels = c("AUC", "CalibrationSlope", "BrierScore",
                                                "CalibrationIntercept", "Accuracy", "LogarithmicLoss",
                                                "CohensKappa"))) %>% 
      filter(Metric %in% input$measure) %>%
      ggplot(aes(x = Runtime,
                 y = Performance,
                 group = `Tuned hyperparameters`)) +
      geom_point(aes(col = `Tuned hyperparameters`,
                     shape = `Tuned hyperparameters`)) +
      geom_point(data = df_scale_setter %>%
                   filter(Metric %in% input$measure),
                 alpha = 0) +
      geom_hline(data = df_targets %>%
                   filter(Metric %in% input$measure),
                 aes(yintercept = Target),
                 col = "red", lty = "dotted") +
      facet_wrap(~ Metric,
                 scales = "free_y",
                 labeller = labeller(Metric = Metric.labs)) +
      theme_classic() +
      scale_color_manual(breaks = HP.labs,
                         labels = names(HP.labs),
                         values = HPcomb_pal) +
      scale_shape_manual(breaks = HP.labs,
                         labels = names(HP.labs),
                         values = c(1, rep(16, 8))) +
      labs(x = paste0("Runtime (", input$unit, ")"))
    
  })
  
}


shinyApp(ui = ui, server = server)
---
title: "OM 617 Project"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
date: "2024-05-22"
runtime: shiny
---
```{r setup, include=TRUE}
library(ggplot2)
library(caret)
library(ggcorrplot)
library(flexdashboard)
library(tidyverse)
library(shiny)
library(ggthemes)
library(plotly)
library(scales)
library(fmsb)
library(dplyr)




working_directory <- dirname(rstudioapi::getSourceEditorContext()$path)


file_path <- file.path(working_directory, 'Baseball Data Project.csv')


baseball_data_project <- read_csv(file_path)

```

Win Prediction Model
===========================
### Slider Inputs 
```{r}


# Normalize predictor variables
baseball_data_project_2013 <- baseball_data_project %>% filter(Year == 2013)
baseball_data_project_2013 <- baseball_data_project_2013 %>%
  mutate(
    avgH = scale(avgH),
    avgG = scale(avgG),
    OBP = scale(OBP),
    avgRBI = scale(avgRBI)
  )

# Train the regression model with normalized data
model <- lm(W ~ avgH + avgG + OBP + avgRBI, data = baseball_data_project_2013)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Win Prediction Model"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("players_input", "Select Players (Max 3):", choices = unique(baseball_data_project_2013$Player), multiple = TRUE, options = list(maxItems = 3)),
      actionButton("predict_button", "Predict Wins")
    ),
    mainPanel(
      h3("Wins Prediction"),
      verbatimTextOutput("prediction_output"),
      plotlyOutput("scatterplot_avgH"),
      plotlyOutput("scatterplot_avgG"),
      plotlyOutput("scatterplot_OBP"),
      plotlyOutput("scatterplot_avgRBI")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict_button, {
    req(input$players_input)
    
    players_data <- baseball_data_project_2013 %>%
      filter(Player %in% input$players_input)
    
    if (nrow(players_data) > 0) {
      summary_stats <- players_data %>%
        summarise(
          avgH = mean(avgH),
          avgG = mean(avgG),
          OBP = mean(OBP),
          avgRBI = mean(avgRBI)
        )
      
      prediction <- predict(model, newdata = summary_stats)
      prediction_162 <- round(prediction * (162 / mean(baseball_data_project_2013$G)))  # Scaling the prediction to 162 games
      
      output$prediction_output <- renderPrint({
        paste("Predicted Wins for selected players out of 162 games:", prediction_162)
      })
      
      plot_data <- players_data %>%
        mutate(predicted_Wins = predict(model, newdata = players_data) * (162 / mean(baseball_data_project_2013$G)))
      
      output$scatterplot_avgH <- renderPlotly({
        p <- ggplot(plot_data, aes(x = avgH, y = predicted_Wins, color = Pos, label = Player, text = paste("Player:", Player, "<br>avgH:", avgH, "<br>Predicted Wins:", predicted_Wins))) +
          geom_point() +
          geom_text(vjust = -0.5) +
          geom_smooth(method = 'lm', se = FALSE) +
          theme(legend.position = "right") +
          theme_bw() +
          labs(title = "Predicted Wins vs. Average Hits", x = "Average Hits", y = "Predicted Wins")
        
        ggplotly(p, tooltip = "text")
      })
      
      output$scatterplot_avgG <- renderPlotly({
        p <- ggplot(plot_data, aes(x = avgG, y = predicted_Wins, color = Pos, label = Player, text = paste("Player:", Player, "<br>avgG:", avgG, "<br>Predicted Wins:", predicted_Wins))) +
          geom_point() +
          geom_text(vjust = -0.5) +
          geom_smooth(method = 'lm', se = FALSE) +
          theme(legend.position = "right") +
          theme_bw() +
          labs(title = "Predicted Wins vs. Average Games", x = "Average Games", y = "Predicted Wins")
        
        ggplotly(p, tooltip = "text")
      })
      
      output$scatterplot_OBP <- renderPlotly({
        p <- ggplot(plot_data, aes(x = OBP, y = predicted_Wins, color = Pos, label = Player, text = paste("Player:", Player, "<br>OBP:", OBP, "<br>Predicted Wins:", predicted_Wins))) +
          geom_point() +
          geom_text(vjust = -0.5) +
          geom_smooth(method = 'lm', se = FALSE) +
          theme(legend.position = "right") +
          theme_bw() +
          labs(title = "Predicted Wins vs. On-base Percentage", x = "On-base Percentage", y = "Predicted Wins")
        
        ggplotly(p, tooltip = "text")
      })
      
      output$scatterplot_avgRBI <- renderPlotly({
        p <- ggplot(plot_data, aes(x = avgRBI, y = predicted_Wins, color = Pos, label = Player, text = paste("Player:", Player, "<br>avgRBI:", avgRBI, "<br>Predicted Wins:", predicted_Wins))) +
          geom_point() +
          geom_text(vjust = -0.5) +
          geom_smooth(method = 'lm', se = FALSE) +
          theme(legend.position = "right") +
          theme_bw() +
          labs(title = "Predicted Wins vs. Average RBIs", x = "Average RBIs", y = "Predicted Wins")
        
        ggplotly(p, tooltip = "text")
      })
    } else {
      output$prediction_output <- renderPrint({
        "No players selected or invalid selection. Please select up to 3 players."
      })
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)




```

library(shiny)
library(tidyverse)
library(ggplot2)
library(cluster)
library(ggiraph)
library(broom)

# Load and prepare the dataset
data <- read_csv("working_data_with_points.csv")

# Data preparation: Aggregate at the team-game level
team_data <- data %>%
  group_by(gameId, homeTeamAbbr) %>%
  summarise(
    total_tackles = sum(tackle, na.rm = TRUE),
    total_assists = sum(assist, na.rm = TRUE),
    total_fumbles = sum(forcedFumble, na.rm = TRUE),
    total_missed = sum(pff_missedTackle, na.rm = TRUE),
    points_scored = first(homeFinalScore),  # Score at the game level
    points_allowed = first(visitorFinalScore) # Opponent's score at the game level
  ) %>%
  ungroup()

# Further aggregate at the team-week level
team_week_data <- team_data %>%
  group_by(homeTeamAbbr) %>%
  summarise(
    avg_tackles = mean(total_tackles, na.rm = TRUE),
    avg_assists = mean(total_assists, na.rm = TRUE),
    avg_fumbles = mean(total_fumbles, na.rm = TRUE),
    avg_missed = mean(total_missed, na.rm = TRUE),
    avg_points_scored = mean(points_scored, na.rm = TRUE),
    avg_points_allowed = mean(points_allowed, na.rm = TRUE)
  )

# Shiny App
ui <- fluidPage(
  titlePanel("Defensive Metrics Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team_filter", "Select Team:", choices = c("All", unique(team_week_data$homeTeamAbbr))),
      sliderInput("num_clusters", "Number of Clusters:", min = 2, max = 6, value = 3),
      selectInput("heatmap_metric", "Select Metric:", choices = c("avg_tackles", "avg_assists", "avg_fumbles", "avg_missed")),
      downloadButton("download_data", "Download Data"),
      h4("Regression Model Coefficients"),
      tableOutput("regression_table"),
      h4("Cluster Information"),
      tableOutput("cluster_summary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot",
                 ggiraphOutput("scatterplot")
        ),
        tabPanel("Heatmap",
                 plotOutput("heatmap")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # Filtered data based on user input
  filtered_data <- reactive({
    data <- team_week_data
    if (input$team_filter != "All") {
      data <- data %>% filter(homeTeamAbbr == input$team_filter)
    }
    data
  })
  
  # Clustering: Dynamically adjust clusters based on user input
  clustered_data <- reactive({
    set.seed(123)
    clusters <- kmeans(filtered_data() %>% select(avg_tackles, avg_assists, avg_fumbles, avg_missed), 
                       centers = input$num_clusters)
    data <- filtered_data()
    data$cluster <- as.factor(clusters$cluster)
    data
  })
  
  # Regression coefficients
  output$regression_table <- renderTable({
    bind_rows(
      tidy(lm(avg_points_scored ~ avg_tackles + avg_assists + avg_fumbles + avg_missed, data = filtered_data())) %>% 
        mutate(model = "Points Scored"),
      tidy(lm(avg_points_allowed ~ avg_tackles + avg_assists + avg_fumbles + avg_missed, data = filtered_data())) %>% 
        mutate(model = "Points Allowed")
    )
  })
  
  # Scatterplot with dynamic clusters
  output$scatterplot <- renderGirafe({
    gg <- ggplot(clustered_data(), aes(x = avg_tackles, y = avg_points_scored, color = cluster, tooltip = homeTeamAbbr)) +
      geom_point_interactive(size = 5, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Scatterplot of Tackles vs Points Scored", x = "Average Tackles", y = "Average Points Scored", color = "Cluster") +
      theme_minimal()
    girafe(ggobj = gg)
  })
  
  # Heatmap of selected metric
  output$heatmap <- renderPlot({
    metric <- input$heatmap_metric
    clustered_data() %>%
      ggplot(aes(x = homeTeamAbbr, y = metric, fill = !!sym(metric))) +
      geom_tile() +
      labs(title = paste("Heatmap of", metric, "by Team"), x = "Team", y = "Metric", fill = "Value") +
      theme_minimal() +
      scale_fill_gradient(low = "lightblue", high = "darkblue")
  })
  
  # Cluster summary table
  output$cluster_summary <- renderTable({
    clustered_data() %>%
      group_by(cluster) %>%
      summarise(
        avg_points_scored = mean(avg_points_scored, na.rm = TRUE),
        avg_points_allowed = mean(avg_points_allowed, na.rm = TRUE)
      )
  })
  
  # Download handler for exporting filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("team_data_filtered.csv")
    },
    content = function(file) {
      write.csv(clustered_data(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
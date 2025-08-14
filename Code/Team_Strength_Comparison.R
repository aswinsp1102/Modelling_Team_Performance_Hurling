# Function to plot the comparative team strength 

Strength_Comparison_chart <- function (team_names,full_results)
{
  plot_data <- as.data.frame(t(full_results$history.means))
  colnames(plot_data) <- names(team_indices)  
  plot_data$Time <- 1:nrow(plot_data)
  
  highlight_teams <- team_names 
  
  
  plot_data_long <- plot_data %>%
    pivot_longer(
      cols = -Time,
      names_to = "Team",
      values_to = "Strength"
    ) %>%
    filter(Team %in% highlight_teams)
  
  
  ggplot(plot_data_long, aes(x = Time, y = Strength, color = Team)) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    labs(
      title = "Team Strength Comparison",
      x = "Match number",
      y = "Strength Estimate",
      color = "Team"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    scale_color_brewer(palette = "Set1")  
}
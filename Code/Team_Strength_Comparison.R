# Function to plot the comparative team strength 

library(tidyr)
library(dplyr)

# Function to plot the comparative team strength 

Strength_Comparison_chart <- function (team_names,full_results)
{
  plot_data <- as.data.frame(t(full_results$history.means))
  colnames(plot_data) <- names(team_indices)  
  plot_data$Time <- 1:nrow(plot_data)
  team_se_fit <- as.data.frame(t(full_results$se.fit))
  team_se_fit$Time <- 1:nrow(team_se_fit)
  colnames(team_se_fit) <- c("Values","Time")
  # team_se_predict <- as.data.frame(t(full_results$se.predict))
  # colnames(team_se_predict) <- names(full_results$team_indices)
  # team_se_predict$Time <- 1:nrow(team_se_predict)
  
  highlight_teams <- team_names 
  
  
  plot_data_long <- plot_data %>%
    pivot_longer(
      cols = -Time,
      names_to = "Team",
      values_to = "Strength"
    ) %>%
    filter(Team %in% highlight_teams)
  
  # se_fit_long <- team_se_fit %>%
  #   pivot_longer(
  #     cols = -Time,
  #     names_to = "Team",
  #     values_to = "SE_Fit"
  #   ) %>%
  #   filter(Team %in% highlight_teams)
  
  # se_predict_long <- team_se_predict %>%
  #   pivot_longer(
  #     cols = -Time,
  #     names_to = "Team",
  #     values_to = "SE_Predict"
  #   ) %>%
  #   filter(Team %in% highlight_teams)
  
  plot_data_complete <- plot_data_long %>%
    left_join(team_se_fit, by = c("Time")) %>%
    # left_join(se_predict_long, by = c("Time", "Team")) %>%
    mutate(
      lower_fit = Strength - 1.96 * Values, 
      upper_fit = Strength + 1.96 * Values
      # lower_predict = Strength - 1.96 * SE_Predict,  
      # upper_predict = Strength + 1.96 * SE_Predict
    )
  
  
  
  ggplot(plot_data_complete, aes(x = Time, y = Strength, color = Team)) +
    geom_ribbon(aes(ymin = lower_fit, ymax = upper_fit, fill = Team), 
                alpha = 0.2, color = NA) +
    # geom_ribbon(aes(ymin = lower_predict, ymax = upper_predict, fill = Team), 
    #             alpha = 0.1, color = NA, linetype = "dotted") +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    labs(
      title = "Team Strength Comparison",
      x = "Match number",
      y = "Strength Estimate",
      color = "Team",
      fill = "Team"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    guides(fill = guide_legend(override.aes = list(alpha = 0.3))) 
}
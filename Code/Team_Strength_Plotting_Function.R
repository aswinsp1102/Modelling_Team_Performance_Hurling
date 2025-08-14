# Function to plot the team strength variation of a particular team over the years

Team_Strength_Plot <- function (id,full_results,cov_info_df)
{
  team_id <- id
  team <- (cov_info_df$i_value == team_id) | (cov_info_df$j_value == team_id)
  team_fitted <- t(full_results$history.means)[-1,team_id]
  team_se_fit <- full_results$se.fit
  team_se_predict <- full_results$se.predict
  
  lower_fit_team <- team_fitted - 1.96 * team_se_fit
  upper_fit_team <- team_fitted + 1.96 * team_se_fit
  
  
  lower_predict_team <- team_fitted - 1.96 * team_se_predict
  upper_predict_team <- team_fitted + 1.96 * team_se_predict
  
  
  
  library(ggplot2)
  options(repr.plot.width=15, repr.plot.height=8)
  team_plot_data <- data.frame(
    # Time = which(team),
    Time = seq(length(full_results$se.fit)),
    Fitted = as.numeric(team_fitted),
    Lower_Fit = as.numeric(lower_fit_team),
    Upper_Fit = as.numeric(upper_fit_team),
    Lower_Predict = as.numeric(lower_predict_team),
    Upper_Predict = as.numeric(upper_predict_team)
  )
  ggplot(team_plot_data, aes(x = Time)) +
    geom_line(aes(y = Fitted, color = paste("Team Strength" ))) +
    geom_ribbon(aes(ymin = Lower_Fit, ymax = Upper_Fit, fill = "State Uncertainty Bands"), alpha = 0.3) +
    geom_ribbon(aes(ymin = Lower_Predict, ymax = Upper_Predict, fill = "Prediction Uncertainty Bands"), alpha = 0.2) +
    labs(
      title = paste("Team Strength of ",names(team_indices)[unlist(team_indices) == team_id]),
      y = "Performance",
      color = "", fill = ""
    ) +
    scale_color_manual(values = "blue") +
    scale_fill_manual(values = c("green", "cyan")) +
    theme_minimal()
}

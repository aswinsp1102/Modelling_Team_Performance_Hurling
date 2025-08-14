# Data Preprocessing

df <- as.data.frame(read.csv("https://raw.githubusercontent.com/aswinsp1102/Modelling_Team_Performance_Hurling/refs/heads/main/Data/Hurling_ELO_Ratings.csv"))
head(df)
str(df)
df$Date <- as.Date(df$Date , format = '%d-%m-%Y')
df <- df[order(df$Date), , drop = FALSE]
columns_to_check <- c("Weight", "Grade", "Margin","Team.1","Team.2","Sc_1","Sc_2", "Home")
df <- na.omit(df, cols = columns_to_check)
row.names(df) <- NULL
df
df_2024 <- df[df$Date > as.Date("2024-01-01"), ]
df_2024[df_2024$Team.1 == 'Cork',]
df_2024[df_2024$Team.2 == 'Cork',]

library(miscFuncs)

print(sort(unique(df$Team.1)))
print(sort(unique(df$Team.2)))
cols_to_replace <- c("Team.1", "Team.2")
df[cols_to_replace] <- lapply(df[cols_to_replace], function(x) ifelse(x == "FIngal", "Fingal", x))
m <- length(unique(df$Team.1))
unique_teams <- sort(unique(df$Team.1))
team_indices <- setNames(seq_along(unique_teams), unique_teams)
team_indices

# setting up the covariate matrix
cov_info_df <- data.frame(
  i_value = team_indices[df$Team.1],
  j_value = team_indices[df$Team.2],
  h_value = as.numeric(df$Home == "Y")
)
(cov_info_df)
Y <- matrix(as.numeric(df$Sc_1) - as.numeric(df$Sc_2) , nrow = nrow(df) , ncol = 1)
Y

#------------------------------------------------------------------------------

source("https://raw.githubusercontent.com/aswinsp1102/Modelling_Team_Performance_Hurling/refs/heads/main/Code/KF_Fit_Baseline.R")
source("https://raw.githubusercontent.com/aswinsp1102/Modelling_Team_Performance_Hurling/refs/heads/main/Code/KF_Parest_Baseline.R")

# Baseline Model run: with all initial parameters at 0
init_baseline = c(0,0,0,0)
results_baseline <- KFparest(data = Y , m = m ,cov_info =  cov_info_df, initial_values = init_baseline)

# Results of the Baseline Model run
results_baseline
round(results_baseline$hessian,6)
inv_hessian_results <- solve(results_baseline$hessian)
eigen(solve(results_baseline$hessian))
inv_hessian_results
# print("Confidence Interval of the estimates :")
# for (i in 1:4){
#   print(paste(results_baseline$par[i] , " : " , (results_baseline$par[i] - (1.96 * inv_hessian_results[i,i])) ,  " to " , (results_baseline$par[i] + (1.96 * inv_hessian_results[i,i]))))
# }
std_errors <- sqrt(diag(inv_hessian_results))
t_stats <- results_baseline$par / std_errors
p_values <- 2 * pnorm(-abs(t_stats))  # Wald test
round(p_values,5)
data.frame(
  Estimate = results_baseline$par,
  SE = std_errors,
  t = round(t_stats,5),
  p_value = round(p_values,5),
  CI_lower = results_baseline$par - 1.96 * std_errors,
  CI_upper = results_baseline$par + 1.96 * std_errors
)
AIC_baseline <- 2 * results_baseline$value + 2 * 4
eigen(results_baseline$hessian)$values


#----------------------------------------------------------------------------

# Run 2: Optimized initial values
init_optimized = c(0,2,0,2.5)
results_optimized_baseline <- KFparest(data = Y , m = m ,cov_info =  cov_info_df, initial_values = init_optimized)
results_optimized_baseline
round(results_optimized_baseline$hessian,5)
inv_hessian_results <- solve(results_optimized_baseline$hessian)
inv_hessian_results
eigen(solve(results_optimized_baseline$hessian))
# print("Confidence Interval of the estimates :")
# for (i in 1:4){
#   print(paste(results_optimized_baseline$par[i] , " : " , (results_optimized_baseline$par[i] - (1.96 * inv_hessian_results[i,i])) ,  " to " , (results_optimized_baseline$par[i] + (1.96 * inv_hessian_results[i,i]))))
# }
std_errors <- sqrt(diag(inv_hessian_results))
t_stats <- results_optimized_baseline$par / std_errors
p_values <- 2 * pnorm(-abs(t_stats))  # Wald test
round(p_values,5)
data.frame(
  Estimate = results_optimized_baseline$par,
  SE = std_errors,
  t = round(t_stats,5),
  p_value = round(p_values,5),
  CI_lower = results_optimized_baseline$par - 1.96 * std_errors,
  CI_upper = results_optimized_baseline$par + 1.96 * std_errors
)

AIC_optimized_baseline <- 2 * results_optimized_baseline$value + 2 * 4
print(AIC_optimized_baseline)
eigen(results_optimized_baseline$hessian)$values

full_results <- KFfit(
  param = results_optimized_baseline$par,
  data = Y,
  cov_info = cov_info_df,
  m = m,
  fit = TRUE,
  se.fit = TRUE,
  se.predict = TRUE,
  optim = FALSE,
  history.means = TRUE,
  history.vars = TRUE
)


# Individual Team Strength plotting
source("https://raw.githubusercontent.com/aswinsp1102/Modelling_Team_Performance_Hurling/refs/heads/main/Code/Team_Strength_Plotting_Function.R")
Team_Strength_Plot(team_indices[['Cork']],full_results,cov_info_df)
Team_Strength_Plot(team_indices[['Limerick']],full_results,cov_info_df)
Team_Strength_Plot(team_indices[['Tipperary']],full_results,cov_info_df)

source("https://raw.githubusercontent.com/aswinsp1102/Modelling_Team_Performance_Hurling/refs/heads/main/Code/Team_Strength_Comparison.R")
team_names_for_comparison <- c("Cork", "Limerick","Tipperary") 
Strength_Comparison_chart(team_names_for_comparison,full_results)

#--------------------------------------------------------------------------

# # Analysing the effect of momentum - Version 1.1
# source("KF_advance_version_2.R")
# source("New_Covariate_info.R")
# library(dplyr)
# new_cov_info_lookup <- new_cov_info_lookup %>%
#   select(-Score_Diff, -year_start_flag, -Year)
# colnames(new_cov_info_lookup)  <- c('i_value','j_value','h_value','momentum')
# new_cov_info_lookup
# source("KF_fit_version_2.R")
# source("KF_parest_version_2.R")
# 
# init = c(9.76,-1.73,1.44,2.01,-5)
# results <- KFparest_updated(data = Y , m = m ,cov_info =  new_cov_info_lookup, initial_values = init)
# results


# Analysing the effect of momentum - Version 1.2

#initialising rt table
# rt_lookup_table<- matrix(cbind(seq(0,m-1),0,0,0),nrow = m , ncol = 4 )
# colnames(rt_lookup_table) <- c("team_id" , "rt_1" , "rt_2" , "rt_3")
# rt_lookup_table
source("KF_fit_version_3.R")
source("KF_parest_version_2.R")
init = c(0,2,0,2.5)
results <- KFparest(data = Y , m = m ,cov_info =  cov_info_df, initial_values = init)
results
active_hessian <- results$hessian
active_hessian
inv_active_hessian <- solve(active_hessian)
inv_active_hessian
std_errors <- sqrt(diag(inv_active_hessian)) 
print(std_errors)
print("Confidence Interval of the estimates :")
for (i in 1:4){
  print(paste(results$par[i] , " : " , (results$par[i] - (1.96 * inv_active_hessian[i,i])) ,  " to " , (results$par[i] + (1.96 * inv_active_hessian[i,i]))))
}

#--------------------------------------------------------------
# Test on Momentum Influence without any parameter interpretation - Version 1.3 
results$value # 9661.87
results$par
source("KF_fit_version_3.R")
source("KF_parest_version_2.R")
source("KF_advance_version_2.R")
init = results$par
results <- KFparest_updated(data = Y , m = m ,cov_info =  cov_info_df, initial_values = init)
results
active_hessian <- results$hessian
active_hessian
inv_active_hessian <- solve(active_hessian)
inv_active_hessian
std_errors <- sqrt(diag(inv_active_hessian)) 
print(std_errors)
print("Confidence Interval of the estimates :")
for (i in 1:4){
  print(paste(results$par[i] , " : " , (results$par[i] - (1.96 * inv_active_hessian[i,i])) ,  " to " , (results$par[i] + (1.96 * inv_active_hessian[i,i]))))
}


# Test of Momentum influence with parameter monitoring the 
# weight of the momentum additive value in the observation equation - Version 1.4

source("KF_fit_version_3.R")
source("KF_parest_version_2.R")
source("KF_advance_version_2.R")
init = c(11.773572, -1.729861 , 1.304171  , 2.007035,0)
results <- KFparest_updated(data = Y , m = m ,cov_info =  cov_info_df, initial_values = init)
results
active_hessian <- results$hessian
active_hessian
inv_active_hessian <- solve(active_hessian)
inv_active_hessian
std_errors <- sqrt(diag(inv_active_hessian)) 
print(std_errors)
print("Confidence Interval of the estimates :")
for (i in 1:5){
  print(paste(results$par[i] , " : " , (results$par[i] - (1.96 * inv_active_hessian[i,i])) ,  " to " , (results$par[i] + (1.96 * inv_active_hessian[i,i]))))
}



# introducing delta_t into the model which is the time period taken between the games 

cov_info_df$time_period <- c(0,round(diff(df$Date , units = "weeks"),2)) # to be considered as "days" or "weeks" ???
source("KF_fit_version_4.R")
source("KF_parest_version_2.R")
source("KF_advance_version_2.R")
init = c(11.773572, -1.729861 , 1.304171  , 2.007035,0)
results <- KFparest_updated(data = Y , m = m ,cov_info =  cov_info_df, initial_values = init)
results
active_hessian <- results$hessian
active_hessian
inv_active_hessian <- solve(active_hessian)
inv_active_hessian
std_errors <- sqrt(diag(inv_active_hessian)) 
print(std_errors)
print("Confidence Interval of the estimates :")
for (i in 1:5){
  print(paste(results$par[i] , " : " , (results$par[i] - (1.96 * inv_active_hessian[i,i])) ,  " to " , (results$par[i] + (1.96 * inv_active_hessian[i,i]))))
}
full_results <- KFfit_updated(
  param = results$par,
  data = Y,
  cov_info = cov_info_df,
  m = m,
  fit = TRUE,
  se.fit = TRUE,
  se.predict = TRUE,
  history.means = TRUE,
  history.vars = TRUE,
  optim = FALSE
)
print("Team Strength at the end of 2024 season : ")
cbind(names(team_indices),(full_results$mean))
full_results$var
cov_matrix <- solve(results$hessian)
std_errors <- sqrt(diag(cov_matrix))
t_stats <- results$par / std_errors
p_values <- 2 * pnorm(-abs(t_stats))  # Wald test
round(p_values,5)
data.frame(
  Estimate = results$par,
  SE = std_errors,
  t = t_stats,
  p_value = p_values,
  CI_lower = results$par - 1.96 * std_errors,
  CI_upper = results$par + 1.96 * std_errors
)
residuals <- Y - t(full_results$fit)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)
plot(t(full_results$fit), residuals, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")
acf(residuals)
pacf(residuals)
AIC <- 2 * results$value + 2 * 5
eigen(results$hessian)$values

source("Strength_Comparison_chart.R")
team_names_for_comparison <- c("Cork", "Limerick", "Galway","Kilkenny","Dublin") 
Strength_Comparison_chart(team_names_for_comparison)


test_df <- as.data.frame(read.csv("https://raw.githubusercontent.com/aswinsp1102/DataAnalyticsDatasets/refs/heads/main/Hurling_Test_Data.csv"))
test_df
print(sort(unique(test_df$Team.1)))
print(sort(unique(test_df$Team.2)))
filtered_test_df <- (test_df %>% filter(test_df$Team.2 %in% names(team_indices)))
print(sort(unique(filtered_test_df$Team.1)))
print(sort(unique(filtered_test_df$Team.2)))

filtered_test_df$Date <- as.Date(filtered_test_df$Date , format = '%d-%m-%Y')
columns_to_check <- c("Weight", "Grade", "Margin","Team.1","Team.2","Sc_1","Sc_2", "Home")
filtered_test_df <- na.omit(filtered_test_df, cols = columns_to_check)
filtered_test_df

test_Y <- matrix(as.numeric(filtered_test_df$Sc_1) - as.numeric(filtered_test_df$Sc_2) , nrow = nrow(filtered_test_df) , ncol = 1)
test_Y

test_cov_info_df <- data.frame(
  i_value = team_indices[filtered_test_df$Team.1],
  j_value = team_indices[filtered_test_df$Team.2],
  h_value = as.numeric(filtered_test_df$Home == "Y")
)
test_cov_info_df$time_period <- c(0,round(diff(filtered_test_df$Date , units = "weeks"),2)) 

test_full_results <- KFfit_updated(
  param = results$par,
  data = test_Y,
  cov_info = test_cov_info_df,
  m = m,
  fit = TRUE,
  se.fit = TRUE,
  se.predict = TRUE,
  history.means = TRUE,
  history.vars = TRUE,
  optim = FALSE,
  prior.mean = full_results$mean,
  prior.var = full_results$var
)

Team_Strength_Plot(team_indices[['Cork']],test_full_results,test_cov_info_df)
source("Strength_Comparison_chart.R")
team_names_for_comparison <- c("Cork", "Limerick","Tipperary") 
Strength_Comparison_chart(team_names_for_comparison,test_full_results)
test_full_results$mean

print("Team Strength at the end of 2025 season : ")
cbind(names(team_indices),(test_full_results$mean))
residuals <- test_Y - t(test_full_results$fit)
qqnorm(residuals)
qqline(residuals,col = "red")
shapiro.test(residuals)
plot(t(test_full_results$fit), residuals, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")
acf(residuals)

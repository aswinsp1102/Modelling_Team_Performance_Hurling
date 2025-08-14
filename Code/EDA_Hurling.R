# Exploratory data analysis done on Hurling dataset

df <- as.data.frame(read.csv("https://raw.githubusercontent.com/aswinsp1102/DataAnalyticsDatasets/refs/heads/main/Hurling_ELO_Ratings.csv"))
head(df)
str(df)
df$Date <- as.Date(df$Date , format = '%d-%m-%Y')
df <- df[order(df$Date), , drop = FALSE]
columns_to_check <- c("Weight", "Grade", "Margin","Team.1","Team.2","Sc_1","Sc_2", "Home")
df <- na.omit(df, cols = columns_to_check)
row.names(df) <- NULL
df

# Analysing no of unique teams played over the period of 2009-2024

print(sort(unique(df$Team.1)))
print(sort(unique(df$Team.2)))
cols_to_replace <- c("Team.1", "Team.2")
df[cols_to_replace] <- lapply(df[cols_to_replace], function(x) ifelse(x == "FIngal", "Fingal", x))
m <- length(unique(df$Team.1))
unique_teams <- sort(unique(df$Team.1))
team_indices <- setNames(seq_along(unique_teams), unique_teams)
team_indices

# Date , time between the seasons and number of matches played over the seasons
match_counts <- df %>%
  group_by(Year) %>%
  summarise(Matches_Played = n())

print(match_counts)
barplot(
  height = match_counts$Matches_Played,
  names.arg = match_counts$Year,
  xlab = "Year",
  ylab = "Total Matches Played",
  main = "Total Matches over seasons",
  col = "red",
  las = 2,           # Vertical x-axis labels
  cex.names = 0.8,   # Adjust label size
  ylim = c(0, max(match_counts$Matches_Played) * 1.1)  # Add 10% headroom
)

mean(match_counts$Matches_Played)
sd(match_counts$Matches_Played)
range(match_counts$Matches_Played)
c(0,round(diff(df$Date , units = "days"),2)) # to be considered as "days" or "weeks" ???
# analysing the scores of teams

# number of home matches played over these time periods
df$Year <- format(as.Date(df$Date), "%Y")

home_counts <- table(df$Home)
names(home_counts) <- c("AWAY","HOME")
print(home_counts)
pct <- round(home_counts / sum(home_counts) * 100)
library(RColorBrewer)

home_matches <- df %>%
  filter(Home == 'Y') %>% 
  count(Year, Home = Team.1, name = "Home_Matches") %>%
  arrange(Year, desc(Home_Matches))

# Custom colors
colors <- brewer.pal(length(home_counts), "Set1")

# Total home matches played plot
pie(home_counts, 
    labels = paste(names(pct) , pct, "%", sep = " "),  
    col = colors,
    main = "Home Matches Distribution")

# Add a legend with counts and labels
legend("bottomright", 
       legend = paste(names(home_counts), " (", home_counts, ")", sep = ""),
       fill = colors)
home_counts_by_year <- table(df$Home, df$Year)
print(home_counts_by_year)

library(dplyr)

team_total_home <- home_matches %>%
  group_by(Home) %>%
  summarise(Total_Home_Matches = sum(Home_Matches)) %>%
  arrange(desc(Total_Home_Matches))

print(team_total_home)
top_teams <- home_matches %>%
  group_by(Home) %>%
  summarise(Total_Matches = sum(Home_Matches)) %>%
  arrange(desc(Total_Matches)) 

home_matches <- df %>%
  filter(Home == 'Y') %>% 
  count(Year, Home = Team.1, name = "Home_Matches") %>%
  arrange(Year, desc(Home_Matches))
home_matches_top15 <- home_matches %>%
  filter(Home %in% top_teams)


barplot(
  height = team_total_home$Total_Home_Matches[1:15],
  names.arg = team_total_home$Home[1:15],
  xlab = "Team",
  ylab = "Total Home Matches",
  main = "Total Home Matches by Team",
  col = "blue",
  las = 2,           # Vertical x-axis labels
  cex.names = 0.8,   # Adjust label size
  ylim = c(0, max(team_total_home$Total_Home_Matches) * 1.3)  # Add 10% headroom
)

# Distribution of score differences over the period

Y <- matrix(as.numeric(df$Sc_1) - as.numeric(df$Sc_2) , nrow = nrow(df) , ncol = 1)
Y
hist(Y, main = 'Score Difference Distribution')
mean(Y)
sd(Y)
range(Y)
Y == -41
which(Y == -41)
df[449,]
which(Y == 44)
df[597,]
length(which(Y < 0)) / length(Y)
length(which(Y > 0)) / length(Y)
length(which(Y == 0)) / length(Y)

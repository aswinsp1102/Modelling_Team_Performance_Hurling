new_cov_info_lookup <- cbind(cov_info_df,Y,as.numeric(format(as.Date(df$Date), "%Y")),0)
colnames(new_cov_info_lookup) <- c('Team_i' , 'Team_j' , 'home' , 'Score_Diff' ,'Year' ,'momentum')


current_year = new_cov_info_lookup$Year[1]
for (i in 1 : nrow(new_cov_info_lookup))
{
  if ((new_cov_info_lookup$Year[i] == current_year) && (i != 1)){
    new_cov_info_lookup$year_start_flag[i] = FALSE
  }
  else {new_cov_info_lookup$year_start_flag[i] = TRUE}
  current_year = new_cov_info_lookup$Year[i]
}

team_result_lookup <- cbind(unname(team_indices),'O')
colnames(team_result_lookup) <- c('Team' ,'Prev_result' )
for (i in 1 : nrow(new_cov_info_lookup))
{
  if(new_cov_info_lookup$year_start_flag[i])
  {
    team_result_lookup <- cbind(unname(team_indices),'O')
    colnames(team_result_lookup) <- c('Team' ,'Prev_result' )
    if(new_cov_info_lookup$Score_Diff[i] > 0)
    {
      team_result_lookup[new_cov_info_lookup$Team_i[i] + 1, 'Prev_result'] <- 'W'
      team_result_lookup[new_cov_info_lookup$Team_j[i] + 1, 'Prev_result'] <- 'L'
    }
    
    if(new_cov_info_lookup$Score_Diff[i] < 0)
    {
      team_result_lookup[new_cov_info_lookup$Team_i[i] + 1, 'Prev_result'] <- 'L'
      team_result_lookup[new_cov_info_lookup$Team_j[i] + 1, 'Prev_result'] <- 'W'
    }
    new_cov_info_lookup$momentum[i] <- 0
  }
  else
  {
    if(team_result_lookup[new_cov_info_lookup$Team_i[i] + 1 , 'Prev_result'] == 'O' || team_result_lookup[new_cov_info_lookup$Team_j[i] + 1 , 'Prev_result'] == 'O')
    {
      new_cov_info_lookup$momentum[i] <- 0
      if(new_cov_info_lookup$Score_Diff[i] > 0)
      {
        team_result_lookup[new_cov_info_lookup$Team_i[i] + 1, 'Prev_result'] <- 'W'
        team_result_lookup[new_cov_info_lookup$Team_j[i] + 1, 'Prev_result'] <- 'L'
      }
      
      if(new_cov_info_lookup$Score_Diff[i] < 0)
      {
        team_result_lookup[new_cov_info_lookup$Team_i[i] + 1, 'Prev_result'] <- 'L'
        team_result_lookup[new_cov_info_lookup$Team_j[i] + 1, 'Prev_result'] <- 'W'
      }
    }
    else if((team_result_lookup[new_cov_info_lookup$Team_i[i] + 1 , 'Prev_result'] == 'W' && team_result_lookup[new_cov_info_lookup$Team_j[i] + 1 , 'Prev_result'] == 'W')
            ||
            (team_result_lookup[new_cov_info_lookup$Team_i[i] + 1 , 'Prev_result'] == 'L' && team_result_lookup[new_cov_info_lookup$Team_j[i] + 1 , 'Prev_result'] == 'L'))
    {
      new_cov_info_lookup$momentum[i] <- 0
      if(new_cov_info_lookup$Score_Diff[i] > 0)
      {
        team_result_lookup[new_cov_info_lookup$Team_i[i] + 1, 'Prev_result'] <- 'W'
        team_result_lookup[new_cov_info_lookup$Team_j[i] + 1, 'Prev_result'] <- 'L'
      }
      
      if(new_cov_info_lookup$Score_Diff[i] < 0)
      {
        team_result_lookup[new_cov_info_lookup$Team_i[i] + 1, 'Prev_result'] <- 'L'
        team_result_lookup[new_cov_info_lookup$Team_j[i] + 1, 'Prev_result'] <- 'W'
      }
    }
    else if(team_result_lookup[new_cov_info_lookup$Team_i[i] + 1 , 'Prev_result'] != 'O' && team_result_lookup[new_cov_info_lookup$Team_j[i] + 1 , 'Prev_result'] != 'O')
    {
      new_cov_info_lookup$momentum[i] <- ifelse(team_result_lookup[new_cov_info_lookup$Team_i[i] + 1, 'Prev_result'] == 'W', 1, -1)
      if(new_cov_info_lookup$Score_Diff[i] > 0)
      {
        team_result_lookup[new_cov_info_lookup$Team_i[i] + 1, 'Prev_result'] <- 'W'
        team_result_lookup[new_cov_info_lookup$Team_j[i] + 1, 'Prev_result'] <- 'L'
      }
      
      if(new_cov_info_lookup$Score_Diff[i] < 0)
      {
        team_result_lookup[new_cov_info_lookup$Team_i[i] + 1, 'Prev_result'] <- 'L'
        team_result_lookup[new_cov_info_lookup$Team_j[i] + 1, 'Prev_result'] <- 'W'
      }
    }
  }
}

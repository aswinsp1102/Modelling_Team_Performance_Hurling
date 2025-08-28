Team_Strength_Season_Map <- function(year , full_results){
  year_row = year_lookup$End_Row[which(year_lookup$Year == year)]
  ireland_counties = st_read("C:/Users/aswin/Downloads/gadm41_IRL_shp/gadm41_IRL_1.shp")
  table(ireland_counties$NAME_1)
  ireland_counties$NAME_1 <- ifelse((ireland_counties$NAME_1 == 'NA'), "Cork", ireland_counties$NAME_1)
  final_strength = as.data.frame(cbind(t(full_results$history.means)[year_row,],names(team_indices)))
  colnames(final_strength) <- c("Strength","Team")
  ireland_counties <- ireland_counties %>%
    left_join(final_strength, by = c("NAME_1" = "Team"))
  strength_range <- range(as.numeric(ireland_counties$Strength))
  breaks <- seq(strength_range[1], strength_range[2], length.out = 10)
  ireland_counties <- ireland_counties %>%
    mutate(Strength_Category = cut(as.numeric(Strength),
                                   breaks = breaks,
                                   labels = c("Extremely Weak", "Very Weak", "Weak",
                                              "Moderately Weak", "Neutral",
                                              "Moderately Strong", "Strong",
                                              "Very Strong", "Extremely Strong"),
                                   include.lowest = TRUE))
  color_palette <- colorRampPalette(c("blue", "white", "red"))(9)
  plot(ireland_counties["Strength_Category"],
       main = paste("Team Strength at the end of ",year, " season"),
       key.pos = 4,
       axes = TRUE,
       pal = color_palette,
       breaks = "equal", 
       nbreaks = 10)    
}
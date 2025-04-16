create_age_plots <- function(x_var, 
                            x_min = NULL, 
                            x_max = NULL, 
                            startyr = NULL, 
                            endyr = NULL, 
                            yrjump = NULL) {
  # Generate sequence of years at which data should be observed
  years <- seq(startyr, endyr, by = yrjump)
  
  # Create empty lists for each age group
  plots_0_14 <- plots_15_64 <- plots_65_plus <- list()
  
  # Generate plots for each year
  for(y in 1:length(years)) {
    current_year <- years[y]
    
    # Create plots for each age group
    plots_0_14[[y]] <- ggplot(data = subset(WDI, year == current_year & get(x_var) >= x_min & get(x_var) <= x_max)) + 
      geom_point(aes(x = get(x_var), y = pop_0_14_pct, color = Income)) + 
      ggtitle(paste(current_year)) +
      labs(color = "Income") + 
      theme(legend.position = "right", legend.direction = "vertical") +
      # Conditional axis titles
      {if(y == 1) ylab("pop_0_14_pct") else theme(axis.title.y = element_blank())} +
      theme(axis.title.x = element_blank())  # No x labels for top rows
    
    
    plots_15_64[[y]] <- ggplot(data = subset(WDI, year == current_year & get(x_var) >= x_min & get(x_var) <= x_max)) + 
      geom_point(aes(x = get(x_var), y = pop_15_64_pct, color = Income)) + 
      labs(color = "Income") + 
      theme(legend.position = "right", legend.direction = "vertical") +
      # Conditional axis titles
      {if(y == 1) ylab("pop_15_64_pct") else theme(axis.title.y = element_blank())} +
      theme(axis.title.x = element_blank())  # No x labels for top rows
    
    
    plots_65_plus[[y]] <- ggplot(data = subset(WDI, year == current_year & get(x_var) >= x_min & get(x_var) <= x_max)) + 
      geom_point(aes(x = get(x_var), y = pop_65_plus_pct, color = Income)) + 
      labs(color = "Income") + #Rename legend title
      theme(legend.position = "right", legend.direction = "vertical") + #Move legend to bottom
      # Conditional axis titles
      {if(y == 1) ylab("pop_65_plus_pct") else theme(axis.title.y = element_blank())} +
      xlab(x_var)
    
  }
  
  # Create row expressions for patchwork
  row1 <- plots_0_14[[1]]
  row2 <- plots_15_64[[1]]
  row3 <- plots_65_plus[[1]]
  
  for(i in 2:length(years)) {
    row1 <- row1 | plots_0_14[[i]]
    row2 <- row2 | plots_15_64[[i]]
    row3 <- row3 | plots_65_plus[[i]]
  }
  
  # Return the combined plot
  return(row1 / row2 / row3 + 
         plot_layout(guides = "collect") +  # Collect all legends
         theme(legend.position = "right", 
                legend.direction = "vertical"))  # Place at bottom
}


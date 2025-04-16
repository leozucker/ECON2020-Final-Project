create_popgrowth_plots <- function(var1, x1_min = NULL, x1_max = NULL, 
                                    var2, x2_min = NULL, x2_max = NULL, 
                                    var3, x3_min = NULL, x3_max = NULL, 
                                    startyr = NULL, endyr = NULL, yrjump = NULL){

    # Generate sequence of years at which data should be observed
    years <- seq(startyr, endyr, by = yrjump)

    # Create empty lists for each age group
    plots_1 <- plots_2 <- plots_3 <- list()
  
  # Generate plots for each year
    for(y in 1:length(years)) {
        current_year <- years[y]
    
        # Create plots for each age group
        plots_1[[y]] <- ggplot(data = subset(WDI, year == current_year & 
                                            get(var1) >= x1_min & 
                                            get(var1) <= x1_max & 
                                            pop_growth >= -2 & 
                                            pop_growth <= 5)) + 
            geom_point(aes(x = get(var1), y = pop_growth, color = Income)) + 
            ggtitle(paste(current_year)) +
            xlab(var1) + 
            labs(color = "Income") + 
            theme(legend.position = "right", legend.direction = "vertical") +
            # Conditional axis titles
            {if(y == 1) ylab("pop_growth") else theme(axis.title.y = element_blank())}
                    
        plots_2[[y]] <- ggplot(data = subset(WDI, year == current_year & 
                                            get(var2) >= x2_min & 
                                            get(var2) <= x2_max & 
                                            pop_growth >= -2 & 
                                            pop_growth <= 5)) + 
            geom_point(aes(x = get(var2), y = pop_growth, color = Income)) + 
            xlab(var2) + 
            labs(color = "Income") + 
            theme(legend.position = "right", legend.direction = "vertical") +
            # Conditional axis titles
            {if(y == 1) ylab("pop_growth") else theme(axis.title.y = element_blank())}
                    
        plots_3[[y]] <- ggplot(data = subset(WDI, year == current_year & 
                                            get(var3) >= x3_min & 
                                            get(var3) <= x3_max & 
                                            pop_growth >= -2 & 
                                            pop_growth <= 5)) + 
            geom_point(aes(x = get(var3), y = pop_growth, color = Income)) + 
            xlab(var3) + 
            labs(color = "Income") + #Rename legend title
            theme(legend.position = "right", legend.direction = "vertical") #Move legend to bottom +
            # Conditional axis titles
            {if(y == 1) ylab("pop_growth") else theme(axis.title.y = element_blank())}
    }

    # Create row expressions for patchwork
    row1 <- plots_1[[1]]
    row2 <- plots_2[[1]]
    row3 <- plots_3[[1]]
    
    for(i in 2:length(years)) {
        row1 <- row1 | plots_1[[i]]
        row2 <- row2 | plots_2[[i]]
        row3 <- row3 | plots_3[[i]]
    }
    
    # Return the combined plot
    return(row1 / row2 / row3 + 
            plot_layout(guides = "collect") +  # Collect all legends
            theme(legend.position = "right", 
                    legend.direction = "vertical"))  # Place at bottom
}

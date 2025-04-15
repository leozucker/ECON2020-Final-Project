library(ggplot2)
library(patchwork)

Import <- read.csv("/Users/leozucker/Desktop/Grad School/Brown PhD in Economics/Coursework/Applied Economics Analysis (ECON2020)/Final Project/DIRECTORY/ECON2020-Final-Project/P_Data_Extract_From_World_Development_Indicators/8ca27f96-b8dd-4a49-b4d7-36056de6805b_Data.csv")
WDI <- Import[1:(min(which(is.na(Import$Time)))-1),] #Remove rows containing no data at the end of the file
for(i in 3:ncol(WDI)) WDI[[i]] <- as.numeric(WDI[[i]]) #Convert data to numeric
colnames(WDI) <- c(
    "country",      # Country Name
    "ccode",        # Country Code
    "year",         # Time
    "yrcode",       # Time Code
    "gdp_pc",       # GDP per capita
    "life_exp",     # Life expectancy
    "educ_bach",    # Bachelor's education
    "educ_sec",     # Secondary education
    "educ_prim",    # Primary education
    "educ_post",    # Post-secondary education
    "fert_rate",    # Fertility rate
    "pop_0_14_pct", # Population 0-14 %
    "pop_0_14",     # Population 0-14 total
    "pop_15_64_pct",# Population 15-64 %
    "pop_15_64",    # Population 15-64 total
    "pop_65_plus_pct",   # Population 65+ %
    "pop_65_plus",       # Population 65+ total
    "pop_growth",   # Population growth
    "energy_pc",    # Energy use per capita
    "pop_total",    # Total population
    "rural_pct",    # Rural population %
    "rural_growth", # Rural population growth
    "cap_form",     # Capital formation
    "tech_export",  # Tech exports
    "tourism_pct",  # Tourism receipts
    "manuf_export", # Manufacturing exports
    "patent_res",   # Patents by residents
    "patent_nres"   # Patents by non-residents
)

create_age_plots <- function(x_var, x_min = NULL, x_max = NULL, startyr = NULL, endyr = NULL, yrjump = NULL) {
  # Generate sequence of years at which data should be observed
  years <- seq(startyr, endyr, by = yrjump)
  
  # Create empty lists for each age group
  plots_0_14 <- plots_15_64 <- plots_65_plus <- list()
  
  # Generate plots for each year
  for(y in 1:length(years)) {
    current_year <- years[y]
    
    # Create plots for each age group
    plots_0_14[[y]] <- ggplot(data = subset(WDI, year == current_year & get(x_var) >= x_min & get(x_var) <= x_max)) + 
      geom_point(aes(x = get(x_var), y = pop_0_14_pct)) + 
      ggtitle(paste(current_year)) +
      xlab(x_var)
    
    plots_15_64[[y]] <- ggplot(data = subset(WDI, year == current_year & get(x_var) >= x_min & get(x_var) <= x_max)) + 
      geom_point(aes(x = get(x_var), y = pop_15_64_pct)) + 
      ggtitle(paste(current_year)) +
      xlab(x_var)
    
    plots_65_plus[[y]] <- ggplot(data = subset(WDI, year == current_year & get(x_var) >= x_min & get(x_var) <= x_max)) + 
      geom_point(aes(x = get(x_var), y = pop_65_plus_pct)) + 
      ggtitle(paste(current_year)) +
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
  return(row1 / row2 / row3)
}


decade <- seq(1961, 2021, by = 10)

pop_growth_x_pct_0_14 <- pop_growth_x_pct_15_64 <- pop_growth_x_pct_65_plus <- list()
for(y in 1:length(decade)){
    current_year <- decade[y]
    pop_growth_x_pct_0_14[[y]] <- ggplot(data = subset(WDI, year == current_year & pop_growth >= -5 & pop_growth <= 10)) + 
                                    geom_point(aes(x = pop_growth, y = pop_0_14_pct)) + 
                                    ggtitle(paste(current_year))
    pop_growth_x_pct_15_64[[y]] <- ggplot(data = subset(WDI, year == current_year & pop_growth >= -5 & pop_growth <= 10)) +
                                    geom_point(aes(x = pop_growth, y = pop_15_64_pct)) + 
                                    ggtitle(paste(current_year))
    pop_growth_x_pct_65_plus[[y]] <- ggplot(data = subset(WDI, year == current_year & pop_growth >= -5 & pop_growth <= 10)) +
                                    geom_point(aes(x = pop_growth, y = pop_65_plus_pct)) + 
                                    ggtitle(paste(current_year))
}
(pop_growth_x_pct_0_14[[1]] | pop_growth_x_pct_0_14[[2]] | pop_growth_x_pct_0_14[[3]] | pop_growth_x_pct_0_14[[4]] | pop_growth_x_pct_0_14[[5]] | pop_growth_x_pct_0_14[[6]] | pop_growth_x_pct_0_14[[7]]) / 
    (pop_growth_x_pct_15_64[[1]] | pop_growth_x_pct_15_64[[2]] | pop_growth_x_pct_15_64[[3]] | pop_growth_x_pct_15_64[[4]] | pop_growth_x_pct_15_64[[5]] | pop_growth_x_pct_15_64[[6]] | pop_growth_x_pct_15_64[[7]]) / 
    (pop_growth_x_pct_65_plus[[1]] | pop_growth_x_pct_65_plus[[2]] | pop_growth_x_pct_65_plus[[3]] | pop_growth_x_pct_65_plus[[4]] | pop_growth_x_pct_65_plus[[5]] | pop_growth_x_pct_65_plus[[6]] | pop_growth_x_pct_65_plus[[7]])

gdp_pc_x_pct_0_14 <- gdp_pc_x_pct_15_64 <- gdp_pc_x_pct_65_plus <- list()
for(y in 1:length(decade)){
    current_year <- decade[y]
    gdp_pc_x_pct_0_14[[y]] <- ggplot(data = subset(WDI, year == current_year & gdp_pc <= 75000)) + 
                                    geom_point(aes(x = gdp_pc, y = pop_0_14_pct)) + 
                                    ggtitle(paste(current_year))
    gdp_pc_x_pct_15_64[[y]] <- ggplot(data = subset(WDI, year == current_year & gdp_pc <= 75000)) +
                                    geom_point(aes(x = gdp_pc, y = pop_15_64_pct)) + 
                                    ggtitle(paste(current_year))
    gdp_pc_x_pct_65_plus[[y]] <- ggplot(data = subset(WDI, year == current_year & gdp_pc <= 75000)) +
                                    geom_point(aes(x = gdp_pc, y = pop_65_plus_pct)) + 
                                    ggtitle(paste(current_year))
}
(gdp_pc_x_pct_0_14[[1]] | gdp_pc_x_pct_0_14[[2]] | gdp_pc_x_pct_0_14[[3]] | gdp_pc_x_pct_0_14[[4]] | gdp_pc_x_pct_0_14[[5]] | gdp_pc_x_pct_0_14[[6]] | gdp_pc_x_pct_0_14[[7]]) / 
    (gdp_pc_x_pct_15_64[[1]] | gdp_pc_x_pct_15_64[[2]] | gdp_pc_x_pct_15_64[[3]] | gdp_pc_x_pct_15_64[[4]] | gdp_pc_x_pct_15_64[[5]] | gdp_pc_x_pct_15_64[[6]] | gdp_pc_x_pct_15_64[[7]]) / 
    (gdp_pc_x_pct_65_plus[[1]] | gdp_pc_x_pct_65_plus[[2]] | gdp_pc_x_pct_65_plus[[3]] | gdp_pc_x_pct_65_plus[[4]] | gdp_pc_x_pct_65_plus[[5]] | gdp_pc_x_pct_65_plus[[6]] | gdp_pc_x_pct_65_plus[[7]])


#Remember to update lockfile!
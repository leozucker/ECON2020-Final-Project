source("functions/create_age_plots.r")

create_age_plots("pop_growth", x_min = -5, x_max = 10, startyr = 1961, endyr = 2021, yrjump = 10)
    ggsave("Outputs/pop_growth age plot.jpg", plot = last_plot(), scale = 3)
create_age_plots("gdp_pc", x_min = 0, x_max = 70000, startyr = 1961, endyr = 2021, yrjump = 10)
    ggsave("Outputs/gdp_pc age plot.jpg", plot = last_plot(), scale = 3)
create_age_plots("gdp_growth", x_min = -5, x_max = 10, startyr = 1961, endyr = 2021, yrjump = 10)
    ggsave("Outputs/gdp_growth age plot.jpg", plot = last_plot(), scale = 3)
create_age_plots("ln_gdp_pc", x_min = -6, x_max = 12, startyr = 1961, endyr = 2021, yrjump = 10)
    ggsave("Outputs/ln_gdp_pc age plot.jpg", plot = last_plot(), scale = 3)

source("functions/create_popgrowth_plots.r")

create_popgrowth_plots("ln_gdp_pc", x1_min = -6, x1_max = 12, 
                      "gdp_growth", x2_min = -5, x2_max = 10, 
                      "ln_patent_res", x3_min = 0, x3_max = 15, 
                      startyr = 1961, endyr = 2021, yrjump = 10)
    ggsave("Outputs/popgrowth plot ln_gdp_pc gdp_growth ln_patent_res.jpg", plot = last_plot(), scale = 3)

#Plots of working age pop. share and GDPPC growth
# Create coefficient to scale GDP growth to match the population percentage range
pop_range <- range(subset(WDI, country == "Korea, Rep.")$pop_15_64_pct, na.rm = TRUE)
gdp_range <- range(subset(WDI, country == "Korea, Rep.")$gdp_pc, na.rm = TRUE)
coef <- diff(pop_range)/diff(gdp_range)
midpoint <- mean(gdp_range)
intercept <- mean(pop_range) - coef * midpoint

# Create plot with dual y-axes to exemplify the demographic dividend
KOR <- ggplot(data = subset(WDI, country == "Korea, Rep.")) +
  # Plot population on primary y-axis 
  geom_line(aes(x = year, y = pop_15_64_pct, color = "Working Age Population")) +
  # Plot GDP growth on secondary y-axis (with transformation)
  geom_line(aes(x = year, y = gdp_pc * coef + intercept, color = "Real GDP p.c.")) +
  # Label the axes
  labs(x = "Year", y = "Working Age Population (%)") +
  # Set up secondary axis
  scale_y_continuous(
    sec.axis = sec_axis(~ (. - intercept)/coef, name = "Real GDP p.c. ($)") 
  ) +
  # Add colors and legend
  scale_color_manual(values = c("Working Age Population" = "blue", "Real GDP p.c." = "red")) +
  labs(color = "") + 
  theme_bw() +
  # Remove the legend and color axis titles
  theme(
    legend.position = "none",
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  ) + 
  ggtitle("South Korea's Demographic Dividend")
ggsave("Outputs/Korea.jpg", plot = last_plot(), scale = 3)

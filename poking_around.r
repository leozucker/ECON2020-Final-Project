library(ggplot2)
library(patchwork)
library(dplyr)

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
WDI <- WDI %>% # Add GDP growth rate variable (as percentage)
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(gdp_growth = (gdp_pc / lag(gdp_pc) - 1) * 100) %>%
  ungroup()
WDI <- WDI |> 
  mutate(ln_gdp_pc = log(gdp_pc)) |> #  Log GDP per capita
  mutate(ln_patent_res = log(patent_res))
WDI$Income <- cut(WDI$gdp_pc, 
                      breaks = c(0, 1045, 4125, 12735, 999999),
                      labels = c("Low", "Lower Middle", "Upper Middle", "High"),
                      include.lowest = TRUE)

source("functions/create_age_plots.r")

create_age_plots("pop_growth", x_min = -5, x_max = 10, startyr = 1961, endyr = 2021, yrjump = 10)
create_age_plots("gdp_pc", x_min = 0, x_max = 70000, startyr = 1961, endyr = 2021, yrjump = 10)
create_age_plots("gdp_growth", x_min = -5, x_max = 10, startyr = 1961, endyr = 2021, yrjump = 10)
create_age_plots("ln_gdp_pc", x_min = -6, x_max = 12, startyr = 1961, endyr = 2021, yrjump = 10)

source("functions/create_popgrowth_plots.r")

create_popgrowth_plots("ln_gdp_pc", x1_min = -6, x1_max = 12, 
                      "gdp_growth", x2_min = -5, x2_max = 10, 
                      "ln_patent_res", x3_min = 0, x3_max = 15, 
                      startyr = 1961, endyr = 2021, yrjump = 10)

#Remember to update lockfile!
library(ggplot2)
library(patchwork)
library(dplyr)

source("functions/create_age_plots.r")

create_age_plots("pop_growth", x_min = -5, x_max = 10, startyr = 1961, endyr = 2021, yrjump = 10)
create_age_plots("gdp_pc", x_min = 0, x_max = 70000, startyr = 1961, endyr = 2021, yrjump = 10)
create_age_plots("gdp_growth", x_min = -5, x_max = 10, startyr = 1961, endyr = 2021, yrjump = 10)
(create_age_plots("ln_gdp_pc", x_min = -6, x_max = 12, startyr = 1961, endyr = 2021, yrjump = 10))

source("functions/create_popgrowth_plots.r")

create_popgrowth_plots("ln_gdp_pc", x1_min = -6, x1_max = 12, 
                      "gdp_growth", x2_min = -5, x2_max = 10, 
                      "ln_patent_res", x3_min = 0, x3_max = 15, 
                      startyr = 1961, endyr = 2021, yrjump = 10)
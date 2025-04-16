#Don't forget to run renv::restore() in the terminal!
library(ggplot2)
library(patchwork)
library(dplyr)


source("Code/data_cleaning.r") #Clean the data

source("Code/descriptive_plots.r", print.eval = TRUE) #Generate descriptive plots
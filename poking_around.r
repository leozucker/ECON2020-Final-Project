library(ggplot2)

Import <- read.csv("/Users/leozucker/Desktop/Grad School/Brown PhD in Economics/Coursework/Applied Economics Analysis (ECON2020)/Final Project/DIRECTORY/ECON2020-Final-Project/P_Data_Extract_From_World_Development_Indicators/8ca27f96-b8dd-4a49-b4d7-36056de6805b_Data.csv")
WDI <- Import[1:(min(which(is.na(WDI$Time)))-1),] #Remove rows containing no data at the end of the file
for(i in 3:ncol(WDI)) WDI[[i]] <- as.numeric(WDI[[i]]) #Convert data to numeric

ggplot(data = WDI) + 
    geom_point(aes(x = ))



#Remember to update lockfile!
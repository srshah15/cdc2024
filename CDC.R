library(tidyverse)

#healthdata <- read.csv("/Users/unitig/cdc2024/Health_Science_Dataset.csv", header = TRUE, na.strings = c("", "NA"))
healthdata <- read.csv("C:/Users/Raeds/Documents/cdc2024/Health_Science_Dataset.csv", skip = 1, header = TRUE)

# Convert columns to appropriate data types
#healthdata$mmwr_year <- as.numeric(healthdata$mmwr_year)

# Check the structure of the updated data frame
str(healthdata)

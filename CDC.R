library(tidyverse)

healthdata <- read.csv("/Users/unitig/cdc2024/Health_Science_Dataset.csv", skip = 1, header = TRUE)
#healthdata <- read.csv("C:/Users/Raeds/Documents/cdc2024/Health_Science_Dataset.csv", skip = 1, header = TRUE)

# Convert columns to appropriate data types
#healthdata$mmwr_year <- as.numeric(healthdata$mmwr_year)

# Check the structure of the updated data frame
str(healthdata)

library(dplyr)

# Rename columns using dplyr::rename
healthdata <- healthdata %>%
  rename(
    Date_As_Of = Data.As.Of,
    Start_Week = Start.Week,
    End_Week = End.Week,
    MMWR_Year = MMWRyear,
    MMWR_Week = MMWRweek,
    Week_Ending_Date = Week.Ending.Date,
    Group_Type = Group,
    Indicator_Type = Indicator,
    Jurisdiction = Jurisdiction,
    Age_Group = Age.Group,
    COVID_Deaths = COVID.19.Deaths,
    Total_Deaths = Total.Deaths,
    Pneumonia_Deaths = Pneumonia.Deaths,
    Influenza_Deaths = Influenza.Deaths,
    Pneumonia_or_Influenza_Deaths = Pneumonia.or.Influenza,
    Pneu_Influenza_or_COVID_Deaths = Pneumonia..Influenza..or.COVID.19.Deaths
  )

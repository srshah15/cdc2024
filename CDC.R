library(tidyverse)

#healthdata <- read.csv("/Users/unitig/cdc2024/Health_Science_Dataset.csv", header = TRUE)
healthdata <- read.csv("C:/Users/Raeds/Documents/cdc2024/Health_Science_Dataset.csv", skip = 1, header = FALSE)

colnames(healthdata) <- healthdata[1, ]

# Step 3: Remove the now redundant first row (which was used for headers)
healthdata <- healthdata[-1, ]

View(healthdata)
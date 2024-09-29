library(tidyverse)
library(ggplot2)
library(ggridges)
library(dplyr)
library(gridExtra)

setwd("C:/Users/Raeds/Documents/cdc2024")

#healthdata <- read.csv("/Users/unitig/cdc2024/Health_Science_Dataset.csv", skip = 1, header = TRUE)
healthdata <- read.csv("C:/Users/Raeds/Documents/cdc2024/Health_Science_Dataset.csv", skip = 1, header = TRUE)

# Convert columns to appropriate data types
#healthdata$mmwr_year <- as.numeric(healthda  ta$mmwr_year)

# Check the structure of the updated data frame
str(healthdata)


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

# remove missing values

healthdata <- healthdata %>%
  mutate(
    `Week Ending Date` = as.Date(`Week_Ending_Date`, format = "%m/%d/%Y"),
    COVID_Deaths = ifelse(is.na(COVID_Deaths), median(COVID_Deaths, na.rm = TRUE), COVID_Deaths),
    Total_Deaths = ifelse(is.na(Total_Deaths), median(Total_Deaths, na.rm = TRUE), Total_Deaths),
    Pneumonia_Deaths = ifelse(is.na(Pneumonia_Deaths), median(Pneumonia_Deaths, na.rm = TRUE), Pneumonia_Deaths),
    Influenza_Deaths = ifelse(is.na(Influenza_Deaths), median(Influenza_Deaths, na.rm = TRUE), Influenza_Deaths),
    Pneumonia_or_Influenza_Deaths = ifelse(is.na(Pneumonia_or_Influenza_Deaths), median(Pneumonia_or_Influenza_Deaths, na.rm = TRUE), Pneumonia_or_Influenza_Deaths),
    Pneu_Influenza_or_COVID_Deaths = ifelse(is.na(Pneu_Influenza_or_COVID_Deaths), median(Pneu_Influenza_or_COVID_Deaths, na.rm = TRUE), Pneu_Influenza_or_COVID_Deaths),
  )

# remove the first row

healthdata <- healthdata[, -1]

#filtered_data <- healthdata[healthdata$Jurisdiction != "United States" & !grepl("^HHS", healthdata$Jurisdiction), ]

# View the filtered data
view(filtered_data)

# write the csv for tableau 
#write.csv(filtered_data, file = "cleaneddata.csv", row.names = FALSE, na = "NA", sep = ",")

allages <- healthdata %>%
  filter(`Age_Group` == "All Ages", `Jurisdiction` == "United States") %>%
  mutate(MMWR_Week = 1:n())  # Replace MMWR_Week with a sequence from 1 to the number of rows


ggplot(data = allages, aes(x = `MMWR_Week`, y = `COVID_Deaths`)) +
  geom_bar(stat = "identity", fill = "lightblue") +  # Use light blue for bars
  labs(title = "COVID-19 Deaths by MMWR Week (All Ages, United States)",
       x = "MMWR Week",
       y = "Number of COVID-19 Deaths") +
  theme_minimal() +  # Clean theme
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),  # Set background color
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center and enlarge the title
    axis.title.x = element_text(size = 16, face = "bold"),  # Enlarge x-axis title
    axis.title.y = element_text(size = 16, face = "bold"),  # Enlarge y-axis title
    axis.text.x = element_text(size = 14),  # Enlarge x-axis tick labels
    axis.text.y = element_text(size = 14)   # Enlarge y-axis tick labels
  )

us_data_2020 <- healthdata %>%
  filter(Jurisdiction == "United States", `Age_Group` == "All Ages")%>%
  filter(MMWR_Week <= 52)%>%  # Filter to include only the first 52 weeks (year 2020) 
  mutate(MMWR_Week = 1:n())  # Replace MMWR_Week with a sequence from 1 to the number of rows


# Create a new column 'Month' that groups MMWR_Week into months
us_data_2020 <- us_data_2020 %>%
  mutate(
    Month = case_when(
      MMWR_Week >= 1  & MMWR_Week <= 4  ~ "January",
      MMWR_Week >= 5  & MMWR_Week <= 8  ~ "February",
      MMWR_Week >= 9  & MMWR_Week <= 12 ~ "March",
      MMWR_Week >= 13 & MMWR_Week <= 16 ~ "April",
      MMWR_Week >= 17 & MMWR_Week <= 20 ~ "May",
      MMWR_Week >= 21 & MMWR_Week <= 24 ~ "June",
      MMWR_Week >= 25 & MMWR_Week <= 28 ~ "July",
      MMWR_Week >= 29 & MMWR_Week <= 32 ~ "August",
      MMWR_Week >= 33 & MMWR_Week <= 36 ~ "September",  # Changed 37 to 36
      MMWR_Week >= 37 & MMWR_Week <= 41 ~ "October",     # Changed 42 to 41
      MMWR_Week >= 42 & MMWR_Week <= 46 ~ "November",    # Changed 47 to 46
      MMWR_Week >= 47 & MMWR_Week <= 52 ~ "December"     # Changed 48 to 47
    ),
    Month = factor(Month, levels = rev(c("January", "February", "March", "April", "May", 
                                         "June", "July", "August", "September", "October", 
                                         "November", "December")))
  )
# Check the data to ensure months are grouped correctly

us_data_2020_clean <- na.omit(us_data_2020)

# Create the Ridge Plot
ggplot(us_data_2020_clean, aes(x = COVID_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "COVID Deaths", option = "C") +  # You can adjust the color palette here
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(
    title = "COVID Deaths in the US by Month (2020)",
    x = "COVID Deaths per Week",
    y = "Month"
  )


us_data_2021 <- healthdata %>%
  filter(Jurisdiction == "United States", `Age_Group` == "All Ages")%>%
  mutate(MMWR_Week = 1:n())  # Replace MMWR_Week with a sequence from 1 to the number of rows

View(us_data_2021)

# Group MMWR_Week into months for 2021
us_data_2021 <- us_data_2021 %>%
  mutate(
    Month = case_when(
      MMWR_Week >= 52 & MMWR_Week <= 56 ~ "January",
      MMWR_Week >= 57 & MMWR_Week <= 61 ~ "February",
      MMWR_Week >= 62 & MMWR_Week <= 65 ~ "March",
      MMWR_Week >= 66 & MMWR_Week <= 69 ~ "April",
      MMWR_Week >= 70 & MMWR_Week <= 73 ~ "May",
      MMWR_Week >= 74 & MMWR_Week <= 77 ~ "June",
      MMWR_Week >= 78 & MMWR_Week <= 81 ~ "July",
      MMWR_Week >= 82 & MMWR_Week <= 85 ~ "August",
      MMWR_Week >= 86 & MMWR_Week <= 89 ~ "September",
      MMWR_Week >= 90 & MMWR_Week <= 93 ~ "October",
      MMWR_Week >= 94 & MMWR_Week <= 97 ~ "November",
      MMWR_Week >= 98 & MMWR_Week <= 104 ~ "December"
    ),
    Month = factor(Month, levels = rev(c("January", "February", "March", "April", "May", "June", 
                                         "July", "August", "September", "October", "November", "December")))
    
  )

# Remove rows with NA values
us_data_2021_clean <- na.omit(us_data_2021)

# Create the Ridge Plot for 2021
ggplot(us_data_2021_clean, aes(x = COVID_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "COVID Deaths", option = "C") +  # Adjust color palette here
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(
    title = "COVID Deaths in the US by Month (2021)",
    x = "COVID Deaths per Week",
    y = "Month"
  ) 




us_data_2022 <- healthdata %>%
  filter(Jurisdiction == "United States", `Age_Group` == "All Ages")%>%
  mutate(MMWR_Week = 1:n())  # Replace MMWR_Week with a sequence from 1 to the number of rows

# Group MMWR_Week into months for 2022
us_data_2022 <- us_data_2022 %>%
  mutate(
    Month = case_when(
      MMWR_Week >= 104 & MMWR_Week <= 108 ~ "January",
      MMWR_Week >= 109 & MMWR_Week <= 112 ~ "February",
      MMWR_Week >= 113 & MMWR_Week <= 116 ~ "March",
      MMWR_Week >= 117 & MMWR_Week <= 120 ~ "April",
      MMWR_Week >= 121 & MMWR_Week <= 124 ~ "May",
      MMWR_Week >= 125 & MMWR_Week <= 128 ~ "June",
      MMWR_Week >= 129 & MMWR_Week <= 132 ~ "July",
      MMWR_Week >= 133 & MMWR_Week <= 136 ~ "August",
      MMWR_Week >= 137 & MMWR_Week <= 140 ~ "September",
      MMWR_Week >= 141 & MMWR_Week <= 144 ~ "October",
      MMWR_Week >= 145 & MMWR_Week <= 148 ~ "November",
      MMWR_Week >= 149 & MMWR_Week <= 160 ~ "December"  # Adjust this to fit your week count
    ),
    Month = factor(Month, levels = rev(c("January", "February", "March", "April", "May", "June", 
                                         "July", "August", "September", "October", "November", "December")))
    
  )

# Remove rows with NA values
us_data_2022_clean <- na.omit(us_data_2022)

# Create the Ridge Plot for 2022
ggplot(us_data_2022_clean, aes(x = COVID_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "COVID Deaths", option = "C") +  # Adjust color palette here
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(
    title = "COVID Deaths in the US by Month (2022)",
    x = "COVID Deaths per Week",
    y = "Month"
  ) 
  









covid_plot_2020 <- ggplot(us_data_2020_clean, aes(x = COVID_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "COVID Deaths", option = "C") +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(title = "COVID Deaths in the US by Month (2020)", x = "COVID Deaths per Week", y = "Month")

covid_plot_2021 <- ggplot(us_data_2021_clean, aes(x = COVID_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "COVID Deaths", option = "C") +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(title = "COVID Deaths in the US by Month (2021)", x = "COVID Deaths per Week", y = "Month")

covid_plot_2022 <- ggplot(us_data_2022_clean, aes(x = COVID_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "COVID Deaths", option = "C") +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(title = "COVID Deaths in the US by Month (2022)", x = "COVID Deaths per Week", y = "Month")














us_data_2020 <- healthdata %>%
  filter(Jurisdiction == "United States", `Age_Group` == "All Ages") %>%
  filter(MMWR_Week <= 52) %>%  # Filter to include only the first 52 weeks (year 2020)
  mutate(MMWR_Week = 1:n())  # Replace MMWR_Week with a sequence from 1 to the number of rows

# Create a new column 'Month' that groups MMWR_Week into months
us_data_2020 <- us_data_2020 %>%
  mutate(
    Month = case_when(
      MMWR_Week >= 1  & MMWR_Week <= 4  ~ "January",
      MMWR_Week >= 5  & MMWR_Week <= 8  ~ "February",
      MMWR_Week >= 9  & MMWR_Week <= 12 ~ "March",
      MMWR_Week >= 13 & MMWR_Week <= 16 ~ "April",
      MMWR_Week >= 17 & MMWR_Week <= 20 ~ "May",
      MMWR_Week >= 21 & MMWR_Week <= 24 ~ "June",
      MMWR_Week >= 25 & MMWR_Week <= 28 ~ "July",
      MMWR_Week >= 29 & MMWR_Week <= 32 ~ "August",
      MMWR_Week >= 33 & MMWR_Week <= 36 ~ "September",
      MMWR_Week >= 37 & MMWR_Week <= 40 ~ "October",
      MMWR_Week >= 41 & MMWR_Week <= 44 ~ "November",
      MMWR_Week >= 45 & MMWR_Week <= 48 ~ "December"
    ),
    Month = factor(Month, levels = rev(c("January", "February", "March", "April", "May", "June", 
                                         "July", "August", "September", "October", "November", "December")))
    
  )

# Check the data to ensure months are grouped correctly
head(us_data_2020)

us_data_2020_clean <- na.omit(us_data_2020)


# Create the Ridge Plot for Influenza Deaths in 2020
ggplot(us_data_2020_clean, aes(x = Influenza_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Influenza Deaths", option = "C") +  # Adjust color palette here
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(
    title = "Influenza Deaths in the US by Month (2020)",
    x = "Influenza Deaths per Week",
    y = "Month"
  )








us_data_2021 <- healthdata %>%
  filter(Jurisdiction == "United States", `Age_Group` == "All Ages")%>%
  mutate(MMWR_Week = 1:n())  # Replace MMWR_Week with a sequence from 1 to the number of rows

View(us_data_2021)

# Group MMWR_Week into months for 2021
us_data_2021 <- us_data_2021 %>%
  mutate(
    Month = case_when(
      MMWR_Week >= 52 & MMWR_Week <= 56 ~ "January",
      MMWR_Week >= 57 & MMWR_Week <= 61 ~ "February",
      MMWR_Week >= 62 & MMWR_Week <= 65 ~ "March",
      MMWR_Week >= 66 & MMWR_Week <= 69 ~ "April",
      MMWR_Week >= 70 & MMWR_Week <= 73 ~ "May",
      MMWR_Week >= 74 & MMWR_Week <= 77 ~ "June",
      MMWR_Week >= 78 & MMWR_Week <= 81 ~ "July",
      MMWR_Week >= 82 & MMWR_Week <= 85 ~ "August",
      MMWR_Week >= 86 & MMWR_Week <= 89 ~ "September",
      MMWR_Week >= 90 & MMWR_Week <= 93 ~ "October",
      MMWR_Week >= 94 & MMWR_Week <= 97 ~ "November",
      MMWR_Week >= 98 & MMWR_Week <= 104 ~ "December"
    ),
    Month = factor(Month, levels = rev(c("January", "February", "March", "April", "May", "June", 
                                         "July", "August", "September", "October", "November", "December")))
    
  )

# Remove rows with NA values
us_data_2021_clean <- na.omit(us_data_2021)

# Create the Ridge Plot for 2021
ggplot(us_data_2021_clean, aes(x = Influenza_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Flu Deaths", option = "C") +  # Adjust color palette here
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(
    title = "Influenza Deaths in the US by Month (2021)",
    x = "Flu Deaths per Week",
    y = "Month"
  ) 


us_data_2022 <- healthdata %>%
  filter(Jurisdiction == "United States", `Age_Group` == "All Ages")%>%
  mutate(MMWR_Week = 1:n())  # Replace MMWR_Week with a sequence from 1 to the number of rows

# Group MMWR_Week into months for 2022
us_data_2022 <- us_data_2022 %>%
  mutate(
    Month = case_when(
      MMWR_Week >= 104 & MMWR_Week <= 108 ~ "January",
      MMWR_Week >= 109 & MMWR_Week <= 112 ~ "February",
      MMWR_Week >= 113 & MMWR_Week <= 116 ~ "March",
      MMWR_Week >= 117 & MMWR_Week <= 120 ~ "April",
      MMWR_Week >= 121 & MMWR_Week <= 124 ~ "May",
      MMWR_Week >= 125 & MMWR_Week <= 128 ~ "June",
      MMWR_Week >= 129 & MMWR_Week <= 132 ~ "July",
      MMWR_Week >= 133 & MMWR_Week <= 136 ~ "August",
      MMWR_Week >= 137 & MMWR_Week <= 140 ~ "September",
      MMWR_Week >= 141 & MMWR_Week <= 144 ~ "October",
      MMWR_Week >= 145 & MMWR_Week <= 148 ~ "November",
      MMWR_Week >= 149 & MMWR_Week <= 160 ~ "December"  # Adjust this to fit your week count
    ),
    Month = factor(Month, levels = rev(c("January", "February", "March", "April", "May", "June", 
                                         "July", "August", "September", "October", "November", "December")))
    
  )

# Remove rows with NA values
us_data_2022_clean <- na.omit(us_data_2022)

# Create the Ridge Plot for 2022
ggplot(us_data_2022_clean, aes(x = Influenza_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Flu Deaths", option = "C") +  # Adjust color palette here
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(
    title = "Influenza Deaths in the US by Month (2022)",
    x = "Flu Deaths per Week",
    y = "Month"
  ) 
library(ggplot2)
library(ggridges)
library(viridis)
library(gridExtra)
library(showtext)

# Load Google fonts using showtext for a modern aesthetic
showtext_auto()
font_add_google("Roboto", "roboto")  # You can choose another font if you prefer

# Function to set a clean, consistent theme
custom_theme <- function() {
  theme_minimal(base_family = "roboto", base_size = 24) +  # Use the custom font and increase base size
    theme(
      plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      axis.title.x = element_text(size = 24, margin = margin(t = 10)),
      axis.title.y = element_text(size = 24, margin = margin(r = 10)),
      axis.text = element_text(size = 14),
      panel.grid.major = element_line(color = "gray80", size = 0.5),  # Light gray gridlines for a clean look
      panel.grid.minor = element_blank(),  # No minor gridlines for simplicity
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
}

# Create the plots with improved aesthetics
flu_plot_2020 <- ggplot(us_data_2020_clean, aes(x = Influenza_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, size = 0.3) +
  scale_fill_viridis_c(name = "Influenza Deaths", option = "plasma") +  # Use the 'plasma' palette for vibrancy
  labs(title = "Influenza Deaths in the US by Month (2020)", 
       x = "Influenza Deaths per Week", y = "Month") +
  custom_theme()

flu_plot_2021 <- ggplot(us_data_2021_clean, aes(x = Influenza_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, size = 0.3) +
  scale_fill_viridis_c(name = "Flu Deaths", option = "plasma") +  # Same consistent palette
  labs(title = "Influenza Deaths in the US by Month (2021)", 
       x = "Flu Deaths per Week", y = "Month") +
  custom_theme()

flu_plot_2022 <- ggplot(us_data_2022_clean, aes(x = Influenza_Deaths, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, size = 0.3) +
  scale_fill_viridis_c(name = "Flu Deaths", option = "plasma") +  # Same consistent palette
  labs(title = "Influenza Deaths in the US by Month (2022)", 
       x = "Flu Deaths per Week", y = "Month") +
  custom_theme()

# Arrange the COVID and Flu plots side by side
# COVID plot arrangement (assuming covid_plot_2020, covid_plot_2021, covid_plot_2022 are already defined)
grid.arrange(covid_plot_2020 + ggtitle("COVID Deaths in the US by Year (2020)"),
             covid_plot_2021 + ggtitle("COVID Deaths in the US by Year (2021)"),
             covid_plot_2022 + ggtitle("COVID Deaths in the US by Year (2022)"),
             ncol = 3, nrow = 1)

# Flu plot arrangement
grid.arrange(flu_plot_2020 + ggtitle("Influenza Deaths in the US by Year (2020)"),
             flu_plot_2021 + ggtitle("Influenza Deaths in the US by Year (2021)"),
             flu_plot_2022 + ggtitle("Influenza Deaths in the US by Year (2022)"),
             ncol = 3, nrow = 1)


most_visited_states <- c("California", "Florida", "Texas")
least_visited_states <- c("North Dakota", "Vermont", "Nebraska")

healthdata <- healthdata %>%
  mutate(
    Week_Ending_Date = as.Date(Week_Ending_Date, format = "%m/%d/%Y")
  )

most_visited_data <- healthdata %>%
  filter(Jurisdiction %in% most_visited_states) %>%
  group_by(Week_Ending_Date, Jurisdiction) %>%
  summarize(Pneu_Influenza_or_COVID_Deaths = sum(Pneu_Influenza_or_COVID_Deaths, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Group = "Most Visited States")

least_visited_data <- healthdata %>%
  filter(Jurisdiction %in% least_visited_states) %>%
  group_by(Week_Ending_Date, Jurisdiction) %>%
  summarize(Pneu_Influenza_or_COVID_Deaths = sum(Pneu_Influenza_or_COVID_Deaths, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Group = "Least Visited States")

# Combine the two datasets
combined_data <- bind_rows(most_visited_data, least_visited_data)

# Plot with faceting to ensure the same Y-axis
ggplot(combined_data, aes(x = Week_Ending_Date, y = Pneu_Influenza_or_COVID_Deaths, color = Jurisdiction, group = Jurisdiction)) +
  geom_line() +
  labs(
    title = "Respiratory Deaths Over Time",
    x = "Year",
    y = "Total Respiratory Deaths",
    color = "States"
  ) +
  facet_wrap(~ Group, scales = "free_x") +  # Facet by group to separate most and least visited
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.position = "bottom",  # Position the legend at the bottom of the plot
    legend.title = element_text(size = 10, face = "bold", hjust = 0.5),  # Center the legend title
    legend.text = element_text(size = 10, face = "bold"),
    panel.spacing = unit(1, "lines"), # Slightly reduce the space between panels
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_x_date(expand = expansion(mult = c(0.02, 0.02))) +  # Add a small margin to the x-axis range
  guides(color = guide_legend(ncol = 2))  # Split the legend into 2 columns

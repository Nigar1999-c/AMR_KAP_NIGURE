# Install the package if not already installed
install.packages("gt")
library(gt)
library(tidyverse)
library(gtsummary)

#load clean data
data <- readxl::read_excel("clean_data/AMR_Clean.xlsx")



# Define the data for the table
Characteristic <- c("Knowledge Level", "Knowledge Level", "Knowledge Level",
                    "Attitude Level", "Attitude Level", "Attitude Level",
                    "Practice Level", "Practice Level")
Level <- c("Good", "Moderate", "Poor",
           "Positive", "Negative", "Uncertain",
           "Good", "Misuse")
Count <- c(122, 314, 268, 209, 124, 371, 250, 454)
Percentage <- c(17, 45, 38, 30, 18, 53, 36, 64)

# Create a data frame
data_table <- data.frame(Characteristic, Level, Count, Percentage)

# Print the table
print(data_table)


# Create a styled table
data_table %>%
  gt() %>%
  tab_header(
    title = "Distribution of Knowledge, Attitude, and Practice Levels"
  ) %>%
  cols_label(
    Characteristic = "Characteristic",
    Level = "Level",
    Count = "Count",
    Percentage = "Percentage (%)"
  )



# Load required libraries
library(tidyr)
library(dplyr)

# Read the CSV file
data_long <- read.csv("~/Downloads/Health_Ins_long_format.csv")

# Convert from long to wide format
data_wide <- data_long %>%
  pivot_wider(
    id_cols = c("ResponseId", "Economy", "AGE", "GENDER", "RELIGION", "POL", 
                "INC", "HINS", "ALC", "MJ", "GB", "NIC"),
    names_from = "x",           # Price values will become column names
    values_from = "y"           # Purchase intention (1/0) will be the values
  )

# Optional: Clean up column names by adding a prefix
data_wide <- data_wide %>%
  rename_with(~paste0("price_", .), matches("^[0-9]"))  # Adds "price_" prefix to numerical column names

# Write to CSV if needed
write.csv(data_wide, "~/Downloads/Health_Ins_wide_format.csv", row.names = FALSE)
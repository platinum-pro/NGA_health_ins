# Load required libraries
library(tidyr)
library(dplyr)

# Read the wide format data
data_wide <- read.csv("~/Downloads/Health_Ins_wide_format.csv")

# First, let's reshape to long format temporarily to find the transition point
data_long <- data_wide %>%
  # Convert to long format, keeping only ResponseId and price columns
  pivot_longer(
    cols = starts_with("price_"),
    names_to = "price",
    values_to = "intention"
  ) %>%
  # Clean up price column - remove "price_" prefix and convert to numeric
  mutate(
    price = as.numeric(gsub("price_", "", price))
  ) %>%
  # Remove NA values
  filter(!is.na(intention)) %>%
  # Arrange by ResponseId and price to ensure correct order
  arrange(ResponseId, price)

# Modified function to handle NAs
find_transition_price <- function(prices, intentions) {
  # Remove any NA pairs
  valid_data <- !is.na(intentions)
  prices <- prices[valid_data]
  intentions <- intentions[valid_data]
  
  if(length(intentions) < 2) return(NA)  # Need at least 2 points to find transition
  
  # Find all positions where value changes from 1 to 0
  transitions <- which(diff(intentions) == -1)
  
  if(length(transitions) > 0) {
    # Get the first transition point
    first_transition <- transitions[1]
    # Calculate average of last 1 and first 0
    transition_price <- mean(c(
      prices[first_transition],    # last price with 1
      prices[first_transition + 1] # first price with 0
    ))
    return(transition_price)
  } else {
    return(NA)  # Return NA if no transition found
  }
}

# Calculate transition prices for each ResponseId
transition_prices <- data_long %>%
  group_by(ResponseId) %>%
  summarise(
    Pchange = find_transition_price(price, intention),
    # Add diagnostic counts
    n_responses = n(),
    n_valid = sum(!is.na(intention)),
    has_transition = !is.na(Pchange)
  )

# Join back to original wide data
final_data <- data_wide %>%
  left_join(transition_prices, by = "ResponseId")

# Write to CSV
write.csv(final_data, "~/Downloads/Health_Ins_wide_format_with_transition.csv", row.names = FALSE)

# Print summary statistics
print(summary(transition_prices))
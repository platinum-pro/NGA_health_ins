# Read the data
data <- read.csv("~/Downloads/Health_Insurance_recoded.csv")

# Create vector of original price points
price_points <- c(5, 25, 65, 125, 250, 500, 1500, 3000, 5500, 17500, 35000, 70000, 140000, 280000, 560000)

# Normalize price points by dividing by maximum price
normalized_prices <- price_points / max(price_points)

# Function to calculate AUC for one respondent
calculate_normalized_auc <- function(row) {
  # Extract responses for price points, maintaining order
  responses <- as.numeric(row[paste0("P", price_points)])
  
  # Remove NA pairs (both x and y values for that position)
  valid_indices <- !is.na(responses)
  if (sum(valid_indices) < 2) return(NA)  # Need at least 2 points for AUC
  
  x_values <- normalized_prices[valid_indices]
  y_values <- responses[valid_indices]
  
  # Calculate AUC using trapezoidal rule
  # diff(x) gives differences between consecutive x values
  # y[-1] gives all y values except first
  # y[-length(y)] gives all y values except last
  auc <- sum(diff(x_values) * (y_values[-1] + y_values[-length(y_values)]) / 2)
  
  return(auc)
}

# Apply the function to each row
results <- data.frame(
  ResponseId = data$ResponseId,
  normalized_auc = apply(data, 1, calculate_normalized_auc)
)

# View the first few results
head(results)

# Save results if needed
write.csv(results, "~/Downloads/ind_auc_scores.csv", row.names = FALSE)
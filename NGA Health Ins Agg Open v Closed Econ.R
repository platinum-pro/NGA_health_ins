# Read the CSV file
df <- read.csv("~/Downloads/NGA Health insurance cleaned data.csv", stringsAsFactors = TRUE)

# Specify the column names with 'X' prefix
numeric_cols <- c("X5", "X25", "X65", "X125", "X250", "X500", "X1500", "X3000", "X5500", 
                  "X17500", "X35000", "X70000", "X140000", "X280000", "X560000")

# Filter for 'Open' economy and calculate percentages
yes_percentages <- sapply(df[numeric_cols], function(x) {
  # Only consider rows where Economy is 'Open'
  open_economy_rows <- df$Economy == 'Open'
  x_open <- x[open_economy_rows]
  
  # Convert to character and calculate percentage
  x_open <- as.character(x_open)
  yes_count <- sum(x_open == 'Yes', na.rm = TRUE)
  total_count <- sum(!is.na(x_open))
  
  if(total_count > 0) {
    return((yes_count / total_count) * 100)
  } else {
    return(NA)
  }
})

# Create a data frame with the results
result <- data.frame(
  Amount = gsub("X", "", numeric_cols),  # Remove 'X' prefix for cleaner output
  Percentage_Yes = round(as.numeric(yes_percentages), 2)
)

# Save results to CSV
write.csv(result, "~/Downloads/yes_percentages_open_economy.csv", row.names = FALSE)

# Display results in console
print(result)
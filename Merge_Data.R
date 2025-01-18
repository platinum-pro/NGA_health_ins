# Read the CSV files
health_insurance <- read.csv("~/Downloads/NGA Health insurance cleaned data.csv")
health_behaviors <- read.csv("~/Downloads/NGA Health Ins Data II.csv")

# Check for duplicate ResponseIDs in each dataset
duplicates_insurance <- sum(duplicated(health_insurance$ResponseID))
duplicates_behaviors <- sum(duplicated(health_behaviors$ResponseID))

# Print number of duplicates if any
print(paste("Duplicates in health insurance dataset:", duplicates_insurance))
print(paste("Duplicates in health behaviors dataset:", duplicates_behaviors))

# If you want to proceed with merging after removing duplicates:
health_insurance_unique <- health_insurance[!duplicated(health_insurance$ResponseID), ]
health_behaviors_unique <- health_behaviors[!duplicated(health_behaviors$ResponseID), ]

# Now merge the deduplicated datasets
merged_data <- merge(health_insurance_unique, health_behaviors_unique, 
                     by = "ResponseID", 
                     all = TRUE)

# Save the merged dataset
write.csv(merged_data, "~/Downloads/merged_health_data.csv", row.names = FALSE)
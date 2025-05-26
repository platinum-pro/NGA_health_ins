# Load required libraries
library(ggplot2)
library(dplyr)

# Read the data (if not already loaded)
df <- read.csv("~/Downloads/Health_Ins_with_AUC_May_26_2025.csv")

# Convert normalized_auc to numeric (in case it was read as character)
df$normalized_auc <- as.numeric(as.character(df$normalized_auc))

# Check the structure of the data
str(df$normalized_auc)
summary(df$normalized_auc)

# Define variable specifications
var_specs <- list(
  list(var = "Economy", label1 = "Open economy", label0 = "Closed economy"),
  list(var = "GENDER", label1 = "Male", label0 = "Female"),
  list(var = "AGE2", label1 = "Above median age", label0 = "Below median age"),
  list(var = "RELIGION2", label1 = "High religiosity", label0 = "Low religiosity"),
  list(var = "POL2", label1 = "High conservatism", label0 = "Low conservatism"),
  list(var = "INC", label1 = "Higher income", label0 = "Lower income"),
  list(var = "HINS", label1 = "Insured", label0 = "Uninsured"),
  list(var = "ALC", label1 = "History of alcohol use", label0 = "No history of alcohol use"),
  list(var = "GB", label1 = "Gambling history", label0 = "No gambling history")
)

# Function to create summary statistics with standard error
create_summary <- function(data, var_name, spec) {
  data %>%
    filter(!is.na(!!sym(var_name)) & !is.na(normalized_auc)) %>%
    filter(!!sym(var_name) %in% c(0, 1)) %>%  # Only include 0 and 1 values
    mutate(group_var = !!sym(var_name)) %>%
    group_by(group_var) %>%
    summarise(
      mean_auc = mean(normalized_auc, na.rm = TRUE),
      se_auc = sd(normalized_auc, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      group_label = case_when(
        group_var == 1 ~ spec$label1,
        group_var == 0 ~ spec$label0,
        TRUE ~ as.character(group_var)
      )
    ) %>%
    filter(!is.na(group_var))  # Extra safety check
}

# Function to create individual barchart
create_barchart <- function(summary_data, var_name) {
  # Change specific variable names for x-axis labels
  x_label <- case_when(
    var_name == "AGE2" ~ "AGE",
    var_name == "RELIGION2" ~ "RELIGION", 
    var_name == "POL2" ~ "POL",
    var_name == "HINS" ~ "INSURANCE STATUS",
    TRUE ~ var_name
  )
  
  ggplot(summary_data, aes(x = group_label, y = mean_auc)) +
    geom_bar(stat = "identity", fill = "lightgray", color = "black", width = 0.6) +
    geom_errorbar(aes(ymin = mean_auc - se_auc, ymax = mean_auc + se_auc), 
                  width = 0.2, color = "black") +
    labs(
      title = paste(""),
      x = x_label,
      y = "Mean Normalized AUC"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black")
    ) +
    ylim(0, max(summary_data$mean_auc + summary_data$se_auc) * 1.1)
}

# Create all barcharts
plots <- list()

for (i in seq_along(var_specs)) {
  spec <- var_specs[[i]]
  var_name <- spec$var
  
  # Create summary data
  summary_data <- create_summary(df, var_name, spec)
  
  # Create plot
  plots[[var_name]] <- create_barchart(summary_data, var_name)
  
  # Print summary statistics
  cat("\n", var_name, "Summary:\n")
  print(summary_data)
}

# Display all plots
for (var_name in names(plots)) {
  print(plots[[var_name]])
}

# Optional: Save plots to files
# Uncomment the following section if you want to save plots as PNG files

# for (var_name in names(plots)) {
#   filename <- paste0("barchart_", var_name, ".png")
#   ggsave(filename, plots[[var_name]], width = 8, height = 6, dpi = 300)
#   cat("Saved:", filename, "\n")
# }

# Create a combined plot (optional - all plots in one figure)
library(gridExtra)
combined_plot <- do.call(grid.arrange, c(plots, ncol = 3))

# Save combined plot (optional)
# ggsave("all_barcharts_combined.png", combined_plot, width = 15, height = 20, dpi = 300)
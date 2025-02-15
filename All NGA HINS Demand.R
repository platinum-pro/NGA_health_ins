library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Read the data
data <- read.csv("~/Downloads/Agg_percentage_analysis.csv")

# Convert All column to numeric if needed
data$All <- as.numeric(data$All)

# Define the Koff function
Koff <- function(x, alpha, Qo, data) {
  k <- log10(max(data$x[!is.na(data$All)])) - log10(min(data$x[!is.na(data$All)]))
  Qo * 10^(k * (exp(-alpha * Qo * x) - 1))
}

# Function to calculate Pmax
calculate_pmax <- function(alpha, Qo, data) {
  k <- log10(max(data$x[!is.na(data$All)])) - log10(min(data$x[!is.na(data$All)]))
  # Add check for valid inputs
  if(alpha <= 0 || Qo <= 0 || k <= 0) {
    return(NA)
  }
  pmax <- (1/log(10^k))/(alpha * Qo)
  # Check if pmax is within the data range
  if(pmax < min(data$x, na.rm = TRUE) || pmax > max(data$x, na.rm = TRUE)) {
    return(NA)
  }
  return(pmax)
}

# Function to fit model and calculate R-squared
fit_and_calculate_rsq <- function(data) {
  # Remove NA values
  valid_data <- data[!is.na(data$All) & data$x > 0, ]
  
  fit <- tryCatch({
    nls(formula = All ~ Koff(x, alpha, Qo, data),
        data = valid_data,
        start = list(alpha = 0.0000001, Qo = 100),
        algorithm = "port",
        lower = c(alpha = 0, Qo = 0),
        upper = c(alpha = 0.1, Qo = 100),
        control = nls.control(maxiter = 50000))
  }, error = function(e) {
    message("Error fitting model: ", e$message)
    return(NULL)
  })
  
  if (!is.null(fit)) {
    residuals <- residuals(fit)
    tss <- sum((valid_data$All - mean(valid_data$All, na.rm = TRUE))^2, na.rm = TRUE)
    rss <- sum(residuals^2)
    r_squared <- 1 - (rss / tss)
    
    # Calculate Pmax
    params <- coef(fit)
    pmax <- calculate_pmax(params["alpha"], params["Qo"], valid_data)
    
    cat("R-squared =", round(r_squared, 4), "\n")
    if(!is.na(pmax)) {
      cat("Pmax =", round(pmax, 2), "\n")
    } else {
      cat("Pmax could not be calculated\n")
    }
    
    return(list(fit = fit, r_squared = r_squared, pmax = pmax))
  }
  
  return(NULL)
}

# Create plot function
create_plot <- function(data, fit_results) {
  # Remove zero or negative x values
  valid_data <- data[data$x > 0, ]
  x_range <- 10^seq(log10(min(valid_data$x)), log10(max(valid_data$x)), length.out = 100)
  plot_data <- data.frame(x = x_range)
  
  if (!is.null(fit_results$fit)) {
    plot_data$All_pred <- predict(fit_results$fit, newdata = data.frame(x = x_range))
  }
  
  p <- ggplot() +
    geom_point(data = valid_data, aes(x = x, y = All), shape = 16) +
    geom_line(data = plot_data, aes(x = x, y = All_pred))
  
  # Add vertical line for Pmax if it exists and is within the plot range
  if (!is.null(fit_results$pmax) && !is.na(fit_results$pmax)) {
    if(fit_results$pmax >= min(valid_data$x) && fit_results$pmax <= max(valid_data$x)) {
      p <- p + geom_vline(xintercept = fit_results$pmax, linetype = "dashed", color = "grey1", alpha = 0.5)
    }
  }
  
  p <- p +
    scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000, 1000000),
                  labels = scales::comma,
                  limits = range(valid_data$x)) +
    labs(title = "Aggregate Demand",
         x = "Price (₦)",
         y = "Proportion of respondents purchasing \nat each price (%)") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          plot.margin = margin(t = 10, r = 30, b = 10, l = 10, unit = "pt")) +
    annotation_logticks(sides = "b")
  
  return(p)
}

# Fit model and calculate R-squared
fit_results <- fit_and_calculate_rsq(data)

# Print parameter values
if (!is.null(fit_results$fit)) {
  cat("\nParameters:\n")
  print(summary(fit_results$fit)$parameters)
}

# Create and display plot
p <- create_plot(data, fit_results)
print(p)
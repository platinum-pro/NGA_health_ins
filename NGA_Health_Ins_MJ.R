library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Read the data
data <- read.csv("~/Downloads/Agg_percentage_analysis.csv")

# Convert percentage columns to numeric if needed
data$MJ_1 <- as.numeric(data$MJ_1)
data$MJ_0 <- as.numeric(data$MJ_0)

# Define the Koff function for MJ_0 only
Koff <- function(x, alpha, Qo, data) {
  k <- log10(max(data$x[!is.na(data$MJ_0)])) - log10(min(data$x[!is.na(data$MJ_0)]))
  Qo * 10^(k * (exp(-alpha * Qo * x) - 1))
}

# Function to calculate Pmax for MJ_0
calculate_pmax <- function(alpha, Qo, data) {
  k <- log10(max(data$x[!is.na(data$MJ_0)])) - log10(min(data$x[!is.na(data$MJ_0)]))
  if(alpha <= 0 || Qo <= 0 || k <= 0) {
    return(NA)
  }
  pmax <- (1/log(10^k))/(alpha * Qo)
  if(pmax < min(data$x, na.rm = TRUE) || pmax > max(data$x, na.rm = TRUE)) {
    return(NA)
  }
  return(pmax)
}

# Function to fit model and calculate R-squared for MJ_0
fit_mj0 <- function(data) {
  # Remove NA values
  valid_data <- data[!is.na(data$MJ_0) & data$x > 0, ]
  
  fit <- tryCatch({
    nls(formula = MJ_0 ~ Koff(x, alpha, Qo, data),
        data = valid_data,
        start = list(alpha = 0.0000001, Qo = 100),
        algorithm = "port",
        lower = c(alpha = 0, Qo = 0),
        upper = c(alpha = 0.1, Qo = 100),
        control = nls.control(maxiter = 50000))
  }, error = function(e) {
    message("Error fitting model for MJ_0: ", e$message)
    return(NULL)
  })
  
  if (!is.null(fit)) {
    residuals <- residuals(fit)
    tss <- sum((valid_data$MJ_0 - mean(valid_data$MJ_0, na.rm = TRUE))^2, na.rm = TRUE)
    rss <- sum(residuals^2)
    r_squared <- 1 - (rss / tss)
    
    params <- coef(fit)
    pmax <- calculate_pmax(params["alpha"], params["Qo"], valid_data)
    
    cat("R-squared for MJ_0 =", round(r_squared, 4), "\n")
    if(!is.na(pmax)) {
      cat("Pmax for MJ_0 =", round(pmax, 2), "\n")
    }
    
    return(list(fit = fit, r_squared = r_squared, pmax = pmax))
  }
  
  return(NULL)
}

# Create plot function
create_plot <- function(data, fit_mj0) {
  # Remove zero or negative x values
  valid_data <- data[data$x > 0, ]
  
  # Create prediction data for MJ_0
  x_range <- 10^seq(log10(min(valid_data$x)), log10(max(valid_data$x)), length.out = 100)
  plot_data <- data.frame(x = x_range)
  
  if (!is.null(fit_mj0$fit)) {
    plot_data$MJ_0_pred <- predict(fit_mj0$fit, newdata = data.frame(x = x_range))
  }
  
  p <- ggplot() +
    # MJ_1 points and smoothed line (non-parametric)
    geom_point(data = valid_data, aes(x = x, y = MJ_1, color = "Cannabis History", shape = "Cannabis History")) +
    geom_smooth(data = valid_data, aes(x = x, y = MJ_1, color = "Cannabis History"),
                method = "loess", se = TRUE, linetype = "solid", linewidth = 0.8) +
    
    # MJ_0 points and fitted curve
    geom_point(data = valid_data, aes(x = x, y = MJ_0, color = "No Cannabis History", shape = "No Cannabis History")) +
    geom_line(data = plot_data, aes(x = x, y = MJ_0_pred, color = "No Cannabis History"), linetype = "dashed")
  
  # Add vertical line for Pmax of MJ_0 if it exists
  if (!is.null(fit_mj0$pmax) && !is.na(fit_mj0$pmax)) {
    if(fit_mj0$pmax >= min(valid_data$x) && fit_mj0$pmax <= max(valid_data$x)) {
      p <- p + geom_vline(xintercept = fit_mj0$pmax, linetype = "dashed", color = "grey50", alpha = 0.5)
    }
  }
  
  p <- p +
    scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000, 1000000),
                  labels = scales::comma,
                  limits = range(valid_data$x)) +
    labs(title = "Aggregate Demand by Cannabis Use History",
         x = "Price (₦)",
         y = "Proportion of respondents purchasing \nat each price (%)",
         color = "Cannabis Use History",
         shape = "Cannabis Use History") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "right") +
    scale_color_manual(values = c("Cannabis History" = "black", "No Cannabis History" = "grey50")) +
    scale_shape_manual(values = c("Cannabis History" = 1, "No Cannabis History" = 16)) +
    annotation_logticks(sides = "b")
  
  return(p)
}

# Fit model for MJ_0 only
fit_mj0 <- fit_mj0(data)

# Print parameter values for MJ_0
if (!is.null(fit_mj0$fit)) {
  cat("\nParameters for No Cannabis History group:\n")
  print(summary(fit_mj0$fit)$parameters)
}

# Create and display plot
p <- create_plot(data, fit_mj0)
print(p)
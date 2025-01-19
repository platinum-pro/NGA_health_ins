library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Read the data
data <- read.csv("~/Downloads/NGA Agg Health Ins Yes percentages.csv")

# Convert percentage columns to numeric if needed
data$Open <- as.numeric(data$Open)
data$Closed <- as.numeric(data$Closed)

# Define the Koff function with condition-specific k
Koff <- function(x, alpha, Qo, condition, data) {
  if(condition == "Open") {
    k <- log10(max(data$x[!is.na(data$Open)])) - log10(min(data$x[!is.na(data$Open)]))
  } else {
    k <- log10(max(data$x[!is.na(data$Closed)])) - log10(min(data$x[!is.na(data$Closed)]))
  }
  Qo * 10^(k * (exp(-alpha * Qo * x) - 1))
}

# Function to calculate Pmax
calculate_pmax <- function(alpha, Qo, condition, data) {
  if(condition == "Open") {
    k <- log10(max(data$x[!is.na(data$Open)])) - log10(min(data$x[!is.na(data$Open)]))
  } else {
    k <- log10(max(data$x[!is.na(data$Closed)])) - log10(min(data$x[!is.na(data$Closed)]))
  }
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
fit_and_calculate_rsq <- function(condition, data) {
  # Remove NA values
  valid_data <- data[!is.na(data[[condition]]) & data$x > 0, ]
  
  # Create a modified formula that includes the condition parameter
  formula_str <- sprintf("%s ~ Koff(x, alpha, Qo, '%s', data)", condition, condition)
  formula <- as.formula(formula_str)
  
  fit <- tryCatch({
    nls(formula = formula,
        data = valid_data,
        start = list(alpha = 0.0000001, Qo = 100),
        algorithm = "port",
        lower = c(alpha = 0, Qo = 0),
        upper = c(alpha = 0.1, Qo = 100),
        control = nls.control(maxiter = 50000))
  }, error = function(e) {
    message("Error fitting model for ", condition, ": ", e$message)
    return(NULL)
  })
  
  if (!is.null(fit)) {
    residuals <- residuals(fit)
    tss <- sum((valid_data[[condition]] - mean(valid_data[[condition]], na.rm = TRUE))^2, na.rm = TRUE)
    rss <- sum(residuals^2)
    r_squared <- 1 - (rss / tss)
    
    # Calculate Pmax
    params <- coef(fit)
    pmax <- calculate_pmax(params["alpha"], params["Qo"], condition, valid_data)
    
    cat("R-squared for", condition, "=", round(r_squared, 4), "\n")
    if(!is.na(pmax)) {
      cat("Pmax for", condition, "=", round(pmax, 2), "\n")
    } else {
      cat("Pmax for", condition, "could not be calculated\n")
    }
    
    return(list(fit = fit, r_squared = r_squared, pmax = pmax))
  }
  
  return(NULL)
}

# Create plot function
create_plot <- function(data, fits) {
  # Remove zero or negative x values
  valid_data <- data[data$x > 0, ]
  x_range <- 10^seq(log10(min(valid_data$x)), log10(max(valid_data$x)), length.out = 100)
  plot_data <- data.frame(x = x_range)
  
  if (!is.null(fits$Open$fit)) {
    plot_data$Open_pred <- predict(fits$Open$fit, newdata = data.frame(x = x_range))
  }
  if (!is.null(fits$Closed$fit)) {
    plot_data$Closed_pred <- predict(fits$Closed$fit, newdata = data.frame(x = x_range))
  }
  
  p <- ggplot() +
    geom_point(data = valid_data, aes(x = x, y = Open, color = "Open", shape = "Open")) +
    geom_point(data = valid_data, aes(x = x, y = Closed, color = "Closed", shape = "Closed")) +
    geom_line(data = plot_data, aes(x = x, y = Open_pred, linetype = "Open"), color = "black") +
    geom_line(data = plot_data, aes(x = x, y = Closed_pred, linetype = "Closed"), color = "black")
  
  # Add vertical lines for Pmax only if they exist and are within the plot range
  if (!is.null(fits$Open$pmax) && !is.na(fits$Open$pmax)) {
    if(fits$Open$pmax >= min(valid_data$x) && fits$Open$pmax <= max(valid_data$x)) {
      p <- p + geom_vline(xintercept = fits$Open$pmax, linetype = "solid", color = "black", alpha = 0.5)
    }
  }
  if (!is.null(fits$Closed$pmax) && !is.na(fits$Closed$pmax)) {
    if(fits$Closed$pmax >= min(valid_data$x) && fits$Closed$pmax <= max(valid_data$x)) {
      p <- p + geom_vline(xintercept = fits$Closed$pmax, linetype = "dashed", color = "grey50", alpha = 0.5)
    }
  }
  
  p <- p +
    scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000, 1000000),
                  labels = scales::comma,
                  limits = range(valid_data$x)) +
    labs(title = "Aggregate Demand for Health Insurance among Nigerian Participants (N = 106)",
         x = "Price (₦)",
         y = "Proportion of respondents purchasing \nhealth insurance at each price (%)",
         color = "Economy",
         shape = "Economy",
         linetype = "Fitted curve") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "right") +
    scale_color_manual(values = c("Open" = "black", "Closed" = "grey50")) +
    scale_shape_manual(values = c("Open" = 1, "Closed" = 16)) +
    scale_linetype_manual(values = c("Open" = "solid", "Closed" = "dashed")) +
    annotation_logticks(sides = "b")
  
  return(p)
}

# Fit models and calculate R-squared for both conditions
fits <- list(
  Open = fit_and_calculate_rsq("Open", data),
  Closed = fit_and_calculate_rsq("Closed", data)
)

# Print parameter values
if (!is.null(fits$Open$fit)) {
  cat("\nParameters for Open economy:\n")
  print(summary(fits$Open$fit)$parameters)
}
if (!is.null(fits$Closed$fit)) {
  cat("\nParameters for Closed economy:\n")
  print(summary(fits$Closed$fit)$parameters)
}

# Create and display plot
p <- create_plot(data, fits)
print(p)
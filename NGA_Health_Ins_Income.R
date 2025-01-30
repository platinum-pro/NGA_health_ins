library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Read the data
data <- read.csv("~/Downloads/Agg_percentage_analysis.csv")

# Convert percentage columns to numeric if needed
data$INC_1 <- as.numeric(data$INC_1)
data$INC_0 <- as.numeric(data$INC_0)

# Define the Koff function with condition-specific k
Koff <- function(x, alpha, Qo, condition, data) {
  if(condition == "INC_1") {
    k <- log10(max(data$x[!is.na(data$INC_1)])) - log10(min(data$x[!is.na(data$INC_1)]))
  } else {
    k <- log10(max(data$x[!is.na(data$INC_0)])) - log10(min(data$x[!is.na(data$INC_0)]))
  }
  Qo * 10^(k * (exp(-alpha * Qo * x) - 1))
}

# Function to calculate Pmax
calculate_pmax <- function(alpha, Qo, condition, data) {
  if(condition == "INC_1") {
    k <- log10(max(data$x[!is.na(data$INC_1)])) - log10(min(data$x[!is.na(data$INC_1)]))
  } else {
    k <- log10(max(data$x[!is.na(data$INC_0)])) - log10(min(data$x[!is.na(data$INC_0)]))
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
  
  if (!is.null(fits$INC_1$fit)) {
    plot_data$INC_1_pred <- predict(fits$INC_1$fit, newdata = data.frame(x = x_range))
  }
  if (!is.null(fits$INC_0$fit)) {
    plot_data$INC_0_pred <- predict(fits$INC_0$fit, newdata = data.frame(x = x_range))
  }
  
  p <- ggplot() +
    geom_point(data = valid_data, aes(x = x, y = INC_1, color = "High Income", shape = "High Income")) +
    geom_point(data = valid_data, aes(x = x, y = INC_0, color = "Low Income", shape = "Low Income")) +
    geom_line(data = plot_data, aes(x = x, y = INC_1_pred, linetype = "High Income"), color = "black") +
    geom_line(data = plot_data, aes(x = x, y = INC_0_pred, linetype = "Low Income"), color = "black")
  
  # Add vertical lines for Pmax only if they exist and are within the plot range
  if (!is.null(fits$INC_1$pmax) && !is.na(fits$INC_1$pmax)) {
    if(fits$INC_1$pmax >= min(valid_data$x) && fits$INC_1$pmax <= max(valid_data$x)) {
      p <- p + geom_vline(xintercept = fits$INC_1$pmax, linetype = "solid", color = "black", alpha = 0.5)
    }
  }
  if (!is.null(fits$INC_0$pmax) && !is.na(fits$INC_0$pmax)) {
    if(fits$INC_0$pmax >= min(valid_data$x) && fits$INC_0$pmax <= max(valid_data$x)) {
      p <- p + geom_vline(xintercept = fits$INC_0$pmax, linetype = "dashed", color = "grey50", alpha = 0.5)
    }
  }
  
  p <- p +
    scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000, 1000000),
                  labels = scales::comma,
                  limits = range(valid_data$x)) +
    labs(title = "Aggregate Demand by Income Groups",
         x = "Price (₦)",
         y = "Proportion of respondents purchasing \nat each price (%)",
         color = "Income Group",
         shape = "Income Group",
         linetype = "Fitted curve") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "right") +
    scale_color_manual(values = c("High Income" = "black", "Low Income" = "grey50")) +
    scale_shape_manual(values = c("High Income" = 1, "Low Income" = 16)) +
    scale_linetype_manual(values = c("High Income" = "solid", "Low Income" = "dashed")) +
    annotation_logticks(sides = "b")
  
  return(p)
}

# Fit models and calculate R-squared for both conditions
fits <- list(
  INC_1 = fit_and_calculate_rsq("INC_1", data),
  INC_0 = fit_and_calculate_rsq("INC_0", data)
)

# Print parameter values
if (!is.null(fits$INC_1$fit)) {
  cat("\nParameters for High Income group:\n")
  print(summary(fits$INC_1$fit)$parameters)
}
if (!is.null(fits$INC_0$fit)) {
  cat("\nParameters for Low Income group:\n")
  print(summary(fits$INC_0$fit)$parameters)
}

# Create and display plot
p <- create_plot(data, fits)
print(p)
library(dplyr)

# Read the data
data <- read.csv("~/Downloads/Agg_percentage_analysis.csv")

# Convert percentage columns to numeric if needed
data$NIC_1 <- as.numeric(data$NIC_1)  # Above median nicotine use
data$NIC_0 <- as.numeric(data$NIC_0)  # Below median nicotine use

# Define the Koff function with condition-specific k
Koff <- function(x, alpha, Qo, condition, data) {
  if(condition == "NIC_1") {
    k <- log10(max(data$x[!is.na(data$NIC_1)])) - log10(min(data$x[!is.na(data$NIC_1)]))
  } else {
    k <- log10(max(data$x[!is.na(data$NIC_0)])) - log10(min(data$x[!is.na(data$NIC_0)]))
  }
  Qo * 10^(k * (exp(-alpha * Qo * x) - 1))
}

# Function to calculate Pmax
calculate_pmax <- function(alpha, Qo, condition, data) {
  if(condition == "NIC_1") {
    k <- log10(max(data$x[!is.na(data$NIC_1)])) - log10(min(data$x[!is.na(data$NIC_1)]))
  } else {
    k <- log10(max(data$x[!is.na(data$NIC_0)])) - log10(min(data$x[!is.na(data$NIC_0)]))
  }
  if(alpha <= 0 || Qo <= 0 || k <= 0) return(NA)
  pmax <- (1/log(10^k))/(alpha * Qo)
  if(pmax < min(data$x, na.rm = TRUE) || pmax > max(data$x, na.rm = TRUE)) return(NA)
  return(pmax)
}

# Function to perform parameter comparisons with F-tests
perform_parameter_F_tests <- function(data) {
  # Remove any NA or zero/negative values
  valid_data <- data[data$x > 0 & !is.na(data$NIC_1) & !is.na(data$NIC_0), ]
  
  # Prepare data in long format
  long_data <- data.frame(
    x = rep(valid_data$x, 2),
    y = c(valid_data$NIC_1, valid_data$NIC_0),
    condition = factor(rep(c("NIC_1", "NIC_0"), each = nrow(valid_data)))
  )
  
  # Calculate k for both conditions
  k_NIC_1 <- log10(max(data$x[!is.na(data$NIC_1)])) - log10(min(data$x[!is.na(data$NIC_1)]))
  k_NIC_0 <- log10(max(data$x[!is.na(data$NIC_0)])) - log10(min(data$x[!is.na(data$NIC_0)]))
  
  # Test for alpha (elasticity)
  alpha_test <- try({
    # Model with same alpha
    model_same_alpha <- nls(y ~ ifelse(condition == "NIC_1",
                                       Qo_1 * 10^(k_NIC_1 * (exp(-alpha * Qo_1 * x) - 1)),
                                       Qo_0 * 10^(k_NIC_0 * (exp(-alpha * Qo_0 * x) - 1))),
                            data = long_data,
                            start = list(alpha = 0.0000001, Qo_1 = 100, Qo_0 = 100),
                            algorithm = "port",
                            lower = c(alpha = 0, Qo_1 = 0, Qo_0 = 0),
                            upper = c(alpha = 0.1, Qo_1 = 100, Qo_0 = 100))
    
    # Model with different alphas
    model_diff_alpha <- nls(y ~ ifelse(condition == "NIC_1",
                                       Qo_1 * 10^(k_NIC_1 * (exp(-alpha_1 * Qo_1 * x) - 1)),
                                       Qo_0 * 10^(k_NIC_0 * (exp(-alpha_0 * Qo_0 * x) - 1))),
                            data = long_data,
                            start = list(alpha_1 = 0.0000001, alpha_0 = 0.0000001, 
                                         Qo_1 = 100, Qo_0 = 100),
                            algorithm = "port")
    
    RSS1 <- sum(residuals(model_same_alpha)^2)
    RSS2 <- sum(residuals(model_diff_alpha)^2)
    df1 <- df.residual(model_same_alpha)
    df2 <- df.residual(model_diff_alpha)
    F_stat <- ((RSS1 - RSS2)/(df1 - df2))/(RSS2/df2)
    p_value <- pf(F_stat, df1 - df2, df2, lower.tail = FALSE)
    
    list(F_stat = F_stat, p_value = p_value, df1 = df1 - df2, df2 = df2,
         model_same = model_same_alpha, model_diff = model_diff_alpha)
  }, silent = TRUE)
  
  # Test for Q0 (intensity)
  Q0_test <- try({
    # Model with same Q0
    model_same_Q0 <- nls(y ~ ifelse(condition == "NIC_1",
                                    Qo * 10^(k_NIC_1 * (exp(-alpha_1 * Qo * x) - 1)),
                                    Qo * 10^(k_NIC_0 * (exp(-alpha_0 * Qo * x) - 1))),
                         data = long_data,
                         start = list(Qo = 100, alpha_1 = 0.0000001, alpha_0 = 0.0000001),
                         algorithm = "port")
    
    # Use model_diff_alpha from above as it has different Q0s
    RSS1 <- sum(residuals(model_same_Q0)^2)
    RSS2 <- sum(residuals(alpha_test$model_diff)^2)
    df1 <- df.residual(model_same_Q0)
    df2 <- df.residual(alpha_test$model_diff)
    F_stat <- ((RSS1 - RSS2)/(df1 - df2))/(RSS2/df2)
    p_value <- pf(F_stat, df1 - df2, df2, lower.tail = FALSE)
    
    list(F_stat = F_stat, p_value = p_value, df1 = df1 - df2, df2 = df2,
         model_same = model_same_Q0)
  }, silent = TRUE)
  
  # Print results
  cat("\n=== Parameter Comparison Results ===\n")
  
  cat("\nAlpha (Elasticity) Comparison:\n")
  if (!inherits(alpha_test, "try-error")) {
    cat("F statistic =", round(alpha_test$F_stat, 3), "\n")
    cat("df =", alpha_test$df1, ",", alpha_test$df2, "\n")
    cat("p-value =", sprintf("%.3f", alpha_test$p_value), "\n")
    cat("\nParameters from model with same alpha:\n")
    print(coef(alpha_test$model_same))
    cat("\nParameters from model with different alphas:\n")
    print(coef(alpha_test$model_diff))
  } else {
    cat("Could not compute alpha comparison\n")
  }
  
  cat("\nQ0 (Intensity) Comparison:\n")
  if (!inherits(Q0_test, "try-error")) {
    cat("F statistic =", round(Q0_test$F_stat, 3), "\n")
    cat("df =", Q0_test$df1, ",", Q0_test$df2, "\n")
    cat("p-value =", sprintf("%.3f", Q0_test$p_value), "\n")
    cat("\nParameters from model with same Q0:\n")
    print(coef(Q0_test$model_same))
  } else {
    cat("Could not compute Q0 comparison\n")
  }
  
  cat("\nNote: Pmax is a derived parameter calculated from alpha and Q0.\n")
  cat("Its statistical significance is determined by the above comparisons.\n")
  
  return(list(
    alpha_test = if(!inherits(alpha_test, "try-error")) alpha_test else NULL,
    Q0_test = if(!inherits(Q0_test, "try-error")) Q0_test else NULL
  ))
}

# Main analysis
cat("\n=== Individual Fits ===\n")
# Fit models and calculate R-squared for both conditions
fits <- list(
  NIC_1 = fit_and_calculate_rsq("NIC_1", data),  # Above median nicotine use
  NIC_0 = fit_and_calculate_rsq("NIC_0", data)   # Below median nicotine use
)

# Perform parameter comparisons
cat("\n=== Parameter Comparison Tests ===\n")
parameter_tests <- perform_parameter_F_tests(data)

# Save detailed results to a text file
capture.output({
  cat("=== Demand Analysis Results ===\n\n")
  
  cat("Individual Fits:\n")
  if (!is.null(fits$NIC_1$fit)) {
    cat("\nAbove Median Nicotine Use Group Summary:\n")
    print(summary(fits$NIC_1$fit))
  }
  if (!is.null(fits$NIC_0$fit)) {
    cat("\nBelow Median Nicotine Use Group Summary:\n")
    print(summary(fits$NIC_0$fit))
  }
  
  cat("\nParameter Comparison Tests:\n")
  parameter_tests <- perform_parameter_F_tests(data)
  
}, file = "~/Downloads/Nicotine_use_demand_analysis_results.txt", append = FALSE)
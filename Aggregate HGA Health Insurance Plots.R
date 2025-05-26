library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(gridExtra)
library(grid)

# Load your data
data <- read.csv("~/Downloads/Agg_percentage_analysis.csv")


# Define the demographic pairs to analyze
demographic_pairs <- list(
  list(var = "Economy", label1 = "Open economy", label0 = "Closed economy"),
  list(var = "GENDER", label1 = "Male", label0 = "Female"),
  list(var = "AGE_binary", label1 = "Above median age", label0 = "Below median age"),
  list(var = "RELIGION_binary", label1 = "High religiosity", label0 = "Low religiosity"),
  list(var = "POL_binary", label1 = "High conservatism", label0 = "Low conservatism"),
  list(var = "INC", label1 = "Higher income", label0 = "Lower income"),
  list(var = "HINS", label1 = "Insured", label0 = "Uninsured"),
  list(var = "ALC", label1 = "History of alcohol use", label0 = "No history of alcohol use"),
  list(var = "GB", label1 = "Gambling history", label0 = "No gambling history")
)

# Modified Koff function to work with any demographic variable
Koff <- function(x, alpha, Qo, condition, data, var_prefix) {
  col_name <- paste0(var_prefix, "_", condition)
  if(condition == "1") {
    k <- log10(max(data$x[!is.na(data[[col_name]])])) - log10(min(data$x[!is.na(data[[col_name]])]))
  } else {
    k <- log10(max(data$x[!is.na(data[[col_name]])])) - log10(min(data$x[!is.na(data[[col_name]])]))
  }
  Qo * 10^(k * (exp(-alpha * Qo * x) - 1))
}

# Modified calculate_pmax function
calculate_pmax <- function(alpha, Qo, condition, data, var_prefix) {
  col_name <- paste0(var_prefix, "_", condition)
  if(condition == "1") {
    k <- log10(max(data$x[!is.na(data[[col_name]])])) - log10(min(data$x[!is.na(data[[col_name]])]))
  } else {
    k <- log10(max(data$x[!is.na(data[[col_name]])])) - log10(min(data$x[!is.na(data[[col_name]])]))
  }
  if(alpha <= 0 || Qo <= 0 || k <= 0) {
    return(NA)
  }
  pmax <- (1/log(10^k))/(alpha * Qo)
  if(pmax < min(data$x, na.rm = TRUE) || pmax > max(data$x, na.rm = TRUE)) {
    return(NA)
  }
  return(pmax)
}

# Modified fit_and_calculate_rsq function
fit_and_calculate_rsq <- function(condition, data, var_prefix) {
  col_name <- paste0(var_prefix, "_", condition)
  valid_data <- data[!is.na(data[[col_name]]) & data$x > 0, ]
  
  formula_str <- sprintf("%s ~ Koff(x, alpha, Qo, '%s', data, '%s')", 
                         col_name, condition, var_prefix)
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
    message("Error fitting model for ", col_name, ": ", e$message)
    return(NULL)
  })
  
  if (!is.null(fit)) {
    residuals <- residuals(fit)
    tss <- sum((valid_data[[col_name]] - mean(valid_data[[col_name]], na.rm = TRUE))^2, na.rm = TRUE)
    rss <- sum(residuals^2)
    r_squared <- 1 - (rss / tss)
    
    params <- coef(fit)
    pmax <- calculate_pmax(params["alpha"], params["Qo"], condition, valid_data, var_prefix)
    
    return(list(fit = fit, r_squared = r_squared, pmax = pmax))
  }
  
  return(NULL)
}

# Modified create_plot function with legend titles
create_demographic_plot <- function(data, var_prefix, label1, label0) {
  valid_data <- data[data$x > 0, ]
  
  # Fit models for both conditions
  fits <- list(
    "1" = fit_and_calculate_rsq("1", data, var_prefix),
    "0" = fit_and_calculate_rsq("0", data, var_prefix)
  )
  
  x_range <- 10^seq(log10(min(valid_data$x)), log10(max(valid_data$x)), length.out = 100)
  plot_data <- data.frame(x = x_range)
  
  # Generate predictions if fits exist
  if (!is.null(fits[["1"]]$fit)) {
    plot_data[[paste0(var_prefix, "_1_pred")]] <- predict(fits[["1"]]$fit, 
                                                          newdata = data.frame(x = x_range))
  }
  if (!is.null(fits[["0"]]$fit)) {
    plot_data[[paste0(var_prefix, "_0_pred")]] <- predict(fits[["0"]]$fit, 
                                                          newdata = data.frame(x = x_range))
  }
  
  # Create the plot
  p <- ggplot() +
    geom_point(data = valid_data, 
               aes(x = x, y = .data[[paste0(var_prefix, "_1")]], 
                   color = label1, shape = label1), size = 2) +
    geom_point(data = valid_data, 
               aes(x = x, y = .data[[paste0(var_prefix, "_0")]], 
                   color = label0, shape = label0), size = 2)
  
  # Add fitted lines if they exist
  if (!is.null(fits[["1"]]$fit)) {
    p <- p + geom_line(data = plot_data, 
                       aes(x = x, y = .data[[paste0(var_prefix, "_1_pred")]], 
                           linetype = label1), color = "black")
  }
  if (!is.null(fits[["0"]]$fit)) {
    p <- p + geom_line(data = plot_data, 
                       aes(x = x, y = .data[[paste0(var_prefix, "_0_pred")]], 
                           linetype = label0), color = "black")
  }
  
  # Add Pmax lines if they exist and are within range
  if (!is.null(fits[["1"]]$pmax) && !is.na(fits[["1"]]$pmax)) {
    if(fits[["1"]]$pmax >= min(valid_data$x) && fits[["1"]]$pmax <= max(valid_data$x)) {
      p <- p + geom_vline(xintercept = fits[["1"]]$pmax, 
                          linetype = "solid", color = "black", alpha = 0.5)
    }
  }
  if (!is.null(fits[["0"]]$pmax) && !is.na(fits[["0"]]$pmax)) {
    if(fits[["0"]]$pmax >= min(valid_data$x) && fits[["0"]]$pmax <= max(valid_data$x)) {
      p <- p + geom_vline(xintercept = fits[["0"]]$pmax, 
                          linetype = "dashed", color = "grey50", alpha = 0.5)
    }
  }
  
  # Customize the plot with legend titles
  p <- p +
    scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000, 1000000),
                  labels = scales::comma,
                  limits = range(valid_data$x)) +
    labs(
      color = "",  # Remove legend title
      shape = "",  # Remove legend title
      linetype = ""  # Remove legend title
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = c(0.25, 0.3),
      legend.background = element_rect(fill = "white", color = NA),
      legend.margin = margin(0.5, 0.5, 0.5, 0.5),
      legend.box.spacing = unit(0, "pt"),
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.size = unit(0.8, "lines"),  # Make legend symbols smaller
      axis.title = element_blank(),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 6, face = "plain"),  # Make title same size as text
      axis.text = element_text(size = 6),
      plot.margin = unit(c(0.9, 1.5, 0.2, 0.2), "cm")  # top, right, bottom, left
    ) +
    scale_color_manual(
      values = c("black", "grey50"),
      labels = c(label1, label0)
    ) +
    scale_shape_manual(
      values = c(1, 16),
      labels = c(label1, label0)
    ) +
    scale_linetype_manual(
      values = c("solid", "dashed"),
      labels = c(label1, label0)
    ) +
    annotation_logticks(sides = "b")
  
  return(p)
}

# Create all plots
plots <- list()
for(demo in demographic_pairs) {
  plots[[demo$var]] <- create_demographic_plot(data, demo$var, demo$label1, demo$label0)
}

# Create the shared x and y axis labels
title <- textGrob(" ",
                  gp = gpar(fontsize = 10, font = 2))
x_axis_label <- textGrob("Price (₦)", 
                         gp = gpar(fontsize = 12))
y_axis_label <- textGrob("Proportion of respondents purchasing \nhealth insurance at each price (%)", 
                         rot = 90, 
                         gp = gpar(fontsize = 12))

# Convert plots list to a list that grid.arrange can handle
plot_list <- plots[1:length(plots)]

# Arrange plots in a grid with shared axes and adjusted spacing
grid_plot <- grid.arrange(
  grobs = plot_list,
  ncol = 3,
  left = y_axis_label,
  bottom = x_axis_label,
  padding = unit(2, "cm")  # Add padding between plots
)

# Save the grid plot with adjusted dimensions
ggsave("demand_curves_grid.png", grid_plot, width = 22, height = 25, dpi = 300)

# Save detailed results
capture.output({
  cat("=== Demand Analysis Results ===\n\n")
  for(demo in demographic_pairs) {
    cat(sprintf("\n=== Analysis for %s ===\n", demo$var))
    fits <- list(
      "1" = fit_and_calculate_rsq("1", data, demo$var),
      "0" = fit_and_calculate_rsq("0", data, demo$var)
    )
    
    if (!is.null(fits[["1"]]$fit)) {
      cat(sprintf("\n%s Summary:\n", demo$label1))
      print(summary(fits[["1"]]$fit))
    }
    if (!is.null(fits[["0"]]$fit)) {
      cat(sprintf("\n%s Summary:\n", demo$label0))
      print(summary(fits[["0"]]$fit))
    }
  }
}, file = "demand_analysis_results.txt", append = FALSE)
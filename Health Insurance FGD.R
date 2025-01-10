library(ggplot2)
library(tidyr)
library(scales)
library(dplyr)

# Raw data
Price <- c(5, 25, 65, 125, 250, 500, 1500, 3000, 5500, 17500, 35000, 70000, 140000, 280000, 560000)
S4644 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
S5223 <- c(1, 1, 1, 1, 1, 1, 1, 1)
S0764 <- c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0)
S4587 <- c(0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
S2609 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

# Create separate data frames for each subject
df_4644 <- data.frame(x = Price, y = S4644, subject = "S4644")
df_5223 <- data.frame(x = Price[1:8], y = S5223[1:8], subject = "S5223")
df_0764 <- data.frame(x = Price, y = S0764, subject = "S0764")
df_4587 <- data.frame(x = Price, y = S4587, subject = "S4587")
df_2609 <- data.frame(x = Price, y = S2609, subject = "S2609")

# Combine all data frames
df_long <- rbind(df_4644, df_5223, df_0764, df_4587, df_2609)

# Plot function with line
plot_with_line <- function(subject_data, subject_name) {
  ggplot(subject_data, aes(x = x, y = y)) +
    geom_point(size = 2) +
    geom_line() +
    scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 100000),
                  labels = function(x) paste0(scales::comma(x))) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = paste("Participant", subject_name),
         x = "Price (₦)",
         y = "Willingness to purchase\nhealth insurance") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5)) +
    annotation_logticks(sides = "b")
}

# Create plots
plots <- lapply(split(df_long, df_long$subject), function(data) {
  plot_with_line(data, unique(data$subject))
})

# Display plots
gridExtra::grid.arrange(grobs = plots, ncol = 3)
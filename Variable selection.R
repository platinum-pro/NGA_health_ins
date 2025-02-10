# Load required libraries
library(car)
library(corrplot)
library(tidyverse)

# Read the data
data_wide <- read.csv("~/Downloads/Health_Ins_wide_format_with_transition.csv")

# Select predictors and DV
predictors <- data_wide %>%
  select(Economy, AGE, GENDER, RELIGION, POL, INC, HINS, risk_binary, Pchange)

# Convert variables to numeric if they're not already
# Remove any rows with NA values for this analysis
predictors_clean <- predictors %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

# 1. Correlation Matrix for predictors only (excluding DV)
predictor_vars <- predictors_clean %>%
  select(-Pchange)
cor_matrix <- cor(predictor_vars, use = "complete.obs")

# Print correlations
print("Correlation Matrix:")
print(round(cor_matrix, 3))

# 2. Visualize correlation matrix
png("predictor_correlations.png", width = 800, height = 600)
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE,
         number.cex = 0.7)
dev.off()

# 3. Identify highly correlated pairs (|r| > 0.7)
high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
if(nrow(high_cor) > 0) {
  print("\nHighly correlated pairs (|r| > 0.7):")
  for(i in 1:nrow(high_cor)) {
    if(high_cor[i, 1] < high_cor[i, 2]) {  # avoid printing duplicates
      cat(sprintf("%s and %s: r = %.3f\n",
                  colnames(cor_matrix)[high_cor[i, 1]],
                  colnames(cor_matrix)[high_cor[i, 2]],
                  cor_matrix[high_cor[i, 1], high_cor[i, 2]]))
    }
  }
} else {
  print("\nNo pairs with correlation > 0.7 found")
}

# 4. VIF Analysis
model <- lm(Pchange ~ Economy + AGE + GENDER + RELIGION + POL + INC + HINS + risk_binary, 
            data = predictors_clean)
vif_values <- vif(model)

print("\nVIF Values:")
print(round(vif_values, 3))

# 5. Eigenvalue analysis
eigen_values <- eigen(cor_matrix)$values
condition_indices <- sqrt(max(eigen_values) / eigen_values)

print("\nCondition Indices:")
print(round(condition_indices, 3))

# Summary
print("\nMulticollinearity Summary:")
print("Variables with VIF > 5:")
print(names(vif_values[vif_values > 5]))

print("\nCondition Index > 30 indicates severe multicollinearity")
if(max(condition_indices) > 30) {
  print("WARNING: High condition indices detected")
}

# Optional: Check correlations with DV
cor_with_dv <- cor(predictor_vars, predictors_clean$Pchange)
print("\nCorrelations with Dependent Variable (Pchange):")
print(round(cor_with_dv, 3))
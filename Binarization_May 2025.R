# Read the data
df <- read.csv("~/Downloads/Health_Ins_with_AUC_May_26_2025.csv")

# Create new binary variables with corrected coding
# AGE2: 0 when AGE <= 23.5, 1 when AGE > 23.5, NA remains NA
df$AGE2 <- ifelse(is.na(df$AGE), NA, ifelse(df$AGE <= 23.5, 0, 1))

# RELIGION2: 0 when RELIGION <= 81, 1 when RELIGION > 81, NA remains NA  
df$RELIGION2 <- ifelse(is.na(df$RELIGION), NA, ifelse(df$RELIGION <= 81, 0, 1))

# POL2: 0 when POL <= 60, 1 when POL > 60, NA remains NA
df$POL2 <- ifelse(is.na(df$POL), NA, ifelse(df$POL <= 60, 0, 1))

# Alternative approach using dplyr (more concise):
library(dplyr)

df <- df %>%
  mutate(
    AGE2 = case_when(
      is.na(AGE) ~ NA_real_,
      AGE <= 23.5 ~ 0,
      AGE > 23.5 ~ 1
    ),
    RELIGION2 = case_when(
      is.na(RELIGION) ~ NA_real_,
      RELIGION <= 81 ~ 0,
      RELIGION > 81 ~ 1
    ),
    POL2 = case_when(
      is.na(POL) ~ NA_real_,
      POL <= 60 ~ 0,
      POL > 60 ~ 1
    )
  )

# Save the updated dataset back to the same file
write.csv(df, "~/Downloads/Health_Ins_with_AUC_May_26_2025.csv", row.names = FALSE)

# Check the results
summary(df[c("AGE", "AGE2", "RELIGION", "RELIGION2", "POL", "POL2")])

# View first few rows to verify
head(df[c("AGE", "AGE2", "RELIGION", "RELIGION2", "POL", "POL2")])
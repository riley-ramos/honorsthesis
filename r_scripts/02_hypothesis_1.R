# =============================================================================
# Script:      02_hypothesis_1.R
# Author:      Riley Ramos
# Date:        2025
# Description: Examines the relationship between urban sprawl, race, and
#              income in the Las Vegas Valley using OLS regression models.
#              Corresponds to Hypothesis 1 in the thesis.
#
# Input:       data/processed/thesis_data.xlsx
#                - sheet: "sprawl_index"
#                - sheet: "demographics"
#
# Output:      Console — regression summaries, correlation, VIF
#              Console — stargazer LaTeX table (Table 4 in thesis)
# =============================================================================


# --- Dependencies ------------------------------------------------------------

library(here)        # portable file paths
library(readxl)      # read Excel workbook sheets
library(tidyverse)   # dplyr, ggplot2, etc.
library(car)         # vif()
library(stargazer)   # regression table output (LaTeX)


# --- Global options ----------------------------------------------------------

options(scipen = 999)  # disable scientific notation


# =============================================================================
# Section 1: Load Data
# =============================================================================
# Reads the sprawl index and demographics sheets from the processed workbook.
# The demographics sheet already contains pre-computed columns from
# 01_data_cleaning.R: total_population, white_population, median_income,
# and poc_population.

sprawl <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                     sheet = "sprawl_index")

demographics <- read_excel(here("data", "processed", "thesis_data.xlsx"),
                           sheet = "demographics")


# =============================================================================
# Section 2: Data Preparation
# =============================================================================
# Computes percent POC and scales median income to thousands of dollars for
# interpretability of regression coefficients. Joins each dataset with the
# sprawl index for use in models.

# --- 2a. Race ----------------------------------------------------------------
race <- demographics %>%
  select(census_tract, total_population, white_population, poc_population) %>%
  mutate(
    perc_poc   = (poc_population / total_population) * 100,
    perc_white = (white_population / total_population) * 100
  )

# Join race with sprawl index
indices_race <- inner_join(sprawl, race, by = "census_tract")

# --- 2b. Income --------------------------------------------------------------
income <- demographics %>%
  select(census_tract, median_income) %>%
  # Scale to thousands so coefficients are interpretable as "per $1,000 increase"
  mutate(median_income = median_income / 1000)

# Join income with sprawl index
income_sprawl <- inner_join(sprawl, income, by = "census_tract")

# --- 2c. Combined race + income ----------------------------------------------
race_income <- income_sprawl %>%
  left_join(indices_race, by = c("census_tract", "sprawl_index"))


# =============================================================================
# Section 3: Diagnostics — Correlation and Multicollinearity
# =============================================================================
# Checks the bivariate correlation between percent POC and median income,
# and tests for multicollinearity using Variance Inflation Factors (VIF).
# Both are discussed in the Hypothesis 1 results section of the thesis.

cat("\n--- Correlation: % POC vs Median Income ---\n")
print(cor(race_income$perc_poc, race_income$median_income, use = "complete.obs"))

cat("\n--- VIF: Sprawl ~ % POC + Median Income ---\n")
print(vif(lm(sprawl_index ~ perc_poc + median_income, data = race_income)))


# =============================================================================
# Section 4: OLS Regressions — Thesis Models (Table 4)
# =============================================================================
# Three models with sprawl index as the dependent variable.
# Results correspond to Table 4 in the thesis.

# Model 1: Effect of % POC on sprawl
m1 <- lm(sprawl_index ~ perc_poc, data = indices_race)
summary(m1)

# Model 2: Effect of median income on sprawl
m2 <- lm(sprawl_index ~ median_income, data = income_sprawl)
summary(m2)

# Model 3: Combined effect of % POC and median income on sprawl
m3 <- lm(sprawl_index ~ perc_poc + median_income, data = race_income)
summary(m3)


# =============================================================================
# Section 5: Regression Table Output (Table 4 in thesis)
# =============================================================================
# Produces LaTeX-formatted regression table for m1, m2, m3.
# Copy the LaTeX output directly into the thesis document.

stargazer(m1, m2, m3,
          type             = "latex",
          digits           = 1,
          header           = FALSE,
          title            = "Impact of Percent POC and Median Income on Sprawl",
          covariate.labels = c("% POC", "Median Income"),
          dep.var.labels   = "Sprawl Index")


# =============================================================================
# Section 6: Exploratory Models (not in thesis)
# =============================================================================
# Models below were used during analysis but did not appear in the final paper.
# Retained here for transparency and reproducibility.

# --- Interaction model: perc_poc * median_income -----------------------------
m4 <- lm(sprawl_index ~ perc_poc * median_income, data = race_income)
summary(m4)

# --- Diagnostic: income ~ POC ------------------------------------------------
m5 <- lm(median_income ~ perc_poc, data = race_income)
summary(m5)

# --- Reversed direction: sprawl as predictor ---------------------------------

# y = % POC
m6 <- lm(perc_poc ~ sprawl_index, data = indices_race)
summary(m6)

# y = median income
m7 <- lm(median_income ~ sprawl_index, data = income_sprawl)
summary(m7)

# y = POC * income (interaction)
m8 <- lm(perc_poc * median_income ~ sprawl_index, data = race_income)
summary(m8)

# y = POC, x = income (no sprawl)
m_x <- lm(perc_poc ~ median_income, data = race_income)
summary(m_x)

# Exploratory stargazer output (text format for quick review)
stargazer(m6, m7,
          type             = "text",
          digits           = 1,
          header           = FALSE,
          covariate.labels = "Sprawl Index",
          dep.var.labels   = c("% POC", "Median Income"))

# --- Exploratory scatter plots -----------------------------------------------

# % POC vs sprawl
ggplot(indices_race, aes(x = perc_poc, y = sprawl_index)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  labs(title = "Percent POC vs Sprawl", x = "Percent POC", y = "Sprawl Index")

# Median income vs sprawl
ggplot(income_sprawl, aes(x = median_income, y = sprawl_index)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  labs(title = "Median Income vs Sprawl",
       x = "Median Income ($1,000s)", y = "Sprawl Index")

# % POC vs median income
ggplot(race_income, aes(x = perc_poc, y = median_income)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  labs(title = "Percent POC vs Median Income",
       x = "Percent POC", y = "Median Income ($1,000s)")

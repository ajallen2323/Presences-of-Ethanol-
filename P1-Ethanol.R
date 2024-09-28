# Author:  Angelina
# Date:    2024-05-07
# Purpose: Portfolio 1 - Ethanol
#-------------------------------------------------------------------------------

# Load packages
suppressPackageStartupMessages(library("tidyverse")); theme_set(theme_bw())
library("knitr")
library("kableExtra")

#input data
data             <- read_csv("gas_mileage_data.csv")

gas_data         <- data |>
                     drop_na(c('mpg', 'ethanol'))


cleaned_gas_data <- gas_data %>%
  mutate(ethanol = if_else(ethanol == 10, "Yes", "No"))

# Calculate summary statistics for the data
s <- cleaned_gas_data |>
  summarize(
     n = n(),
     mean_mpg = mean(mpg),
     median_mpg = median(mpg),
     min_mpg = min(mpg),
     max_mpg = max(mpg),
     Ethanol_Yes = sum(ethanol == "Yes", na.rm = TRUE),
     Ethanol_No = sum(ethanol == "No", na.rm = TRUE)
  )


kable(s, 
      format = "html", 
      caption = "Summary Statistic of mpg and ethanol     variables",
      col.names = c("Total Obs",
                    "mean(mpg)",
                    "median(mpg)",
                    "min(mpg)",
                    "max(mpg)", 
                    "Yes(ethanol)",
                    "No(ethanol)")) %>%
  kable_styling()

# Create exploratory plot of the data
g<- ggplot(cleaned_gas_data, 
             aes(
               x = ethanol,
               y = mpg, 
               color = ethanol)) +
  geom_boxplot()+
  geom_jitter() +
  labs(
    x = "Presence of 10% Ethanol(Yes/No)",
    y = "Miles Per Gallon(mpg)",
    title = "Explanatory Plot of the Response vs the Explanatory"
  )
g

# Fit linear regression model 
m            <- lm(mpg ~ ethanol, 
                   data = cleaned_gas_data)

nd <- cleaned_gas_data |>
  select(ethanol) |>
  unique()

# Construct confidence intervals
ci <- bind_cols(
  nd,
  predict(m,
          newdata = nd,
          interval = "confidence") |>
    as.data.frame() |>
    rename(mpg = fit)
)

# Plot with ggplot
pg <- ggplot(cleaned_gas_data, 
             aes(
               x = ethanol,
               y = mpg, 
               color = ethanol)) +
  geom_jitter() +
  geom_pointrange(
    data = ci, 
    aes(
      ymin = lwr, 
      ymax = upr
    ), 
    col = "purple") +
  labs(
    x = "Presence of 10% Ethanol (Yes/No)",
    y = "Miles Per Gallon (mpg)",
    title = "Effect of 10% Ethanol Presence on Estimated Mean Gas Mileag with 95% Confidence Intervals",
    subtitle = "Simple Linear Regression Model"
  )

pg


#calculate uncertainty

ethanol_present <- cleaned_gas_data |>
  filter(ethanol == "Yes")
ethanol_absent <- cleaned_gas_data |>
  filter(ethanol == "No")

mean_mpg_ethanol_present <- mean(ethanol_present$mpg)
mean_mpg_ethanol_absent <- mean(ethanol_absent$mpg)

ci_diff_means <- t.test(ethanol_present$mpg, ethanol_absent$mpg)$conf.int

# Construct confidence interval to answer scientific question
ci <- bind_cols(
  nd,
  predict(m,
          newdata = nd,
          interval = "confidence") |>
    as.data.frame() |>
    rename(mpg = fit)
)


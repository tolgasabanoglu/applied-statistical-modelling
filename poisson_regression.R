# Load the required libraries
library(rethinking)
library(tidyverse)
library(brms)
library(ggplot2)
library(dplyr)
library(loo)

# Load Hurricanes data
data(Hurricanes)

# Acquaint yourself with the columns
help(Hurricanes)

# Standardize femininity
Hurricanes$fem_std <- (Hurricanes$femininity - mean(Hurricanes$femininity)) / sd(Hurricanes$femininity)

# Fit the Poisson model
m12h1 <- brm(
  deaths ~ fem_std,
  data = Hurricanes,
  family = poisson(),
  prior = c(prior(normal(0, 10), class = "Intercept"),
            prior(normal(0, 1), class = "b")),
  chains = 4
)

# Summary of the Poisson model
summary(m12h1)

# Fit the intercept-only Poisson model
m12h1int <- brm(
  deaths ~ 1,
  data = Hurricanes,
  family = poisson(),
  prior = prior(normal(0, 10), class = "Intercept"),
  chains = 4
)

# Add LOO criterion to the models
m12h1 <- add_criterion(m12h1, "loo")
m12h1int <- add_criterion(m12h1int, "loo")

# Compare models using LOO
loo_compare(m12h1, m12h1int, criterion = "loo") %>% print(simplify = F)

# Fit the gamma-Poisson (negative-binomial) model with femininity as a predictor
m12h2gam <- brm(
  deaths ~ fem_std,
  data = Hurricanes,
  family = negbinomial(),
  prior = c(prior(normal(0, 10), class = "Intercept"),
            prior(normal(0, 1), class = "b")),
  chains = 4
)

# Summary of the gamma-Poisson model
summary(m12h2gam)

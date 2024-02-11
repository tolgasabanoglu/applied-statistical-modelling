# Load necessary libraries
library(rethinking)
library(tidyverse)
library(brms)

# Load tulips dataset
data(tulips)
df <- tulips

# Create a tibble with centered and standardized variables
df_tulip <- tulips %>%
  as_tibble() %>%
  mutate(light = -1 * shade,
         blooms_std = blooms / max(blooms),
         water_cent = water - mean(water),
         shade_cent = shade - mean(shade),
         light_cent = light - mean(light))

# Fit the interaction model (including bed as a main effect)
b8h1 <- brm(blooms_std ~ 0 + water_cent + light_cent + bed + 
              water_cent:light_cent,
            data = df_tulip, family = gaussian,
            prior = c(prior(normal(0, 0.25), class = b),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = ("b8h1.rds"))

# Summary of the interaction model
summary(b8h1)

# Fit the model without bed
b8h2 <- brm(blooms_std ~ 1 + water_cent + light_cent + water_cent:light_cent,
            data = df_tulip, family = gaussian,
            prior = c(prior(normal(0.5, 0.25), class = Intercept),
                      prior(normal(0, 0.25), class = b, lb = 0),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = ("b8m4"))

# Add WAIC criterion to both models
b8h2 <- add_criterion(b8h2, criterion = "waic", "loo")
b8h1 <- add_criterion(b8h1, criterion = "waic", "loo")

# Compare the models using WAIC
loo_compare(b8h1, b8h2, criterion = "waic")

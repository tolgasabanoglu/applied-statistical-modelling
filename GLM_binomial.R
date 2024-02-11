# Load necessary libraries
library(rethinking)
library(tidyverse)
library(brms)
library(ggplot2)
library(dplyr)
library(modelr)
library(tidybayes)

# Load eagles dataset
data(eagles, package = "MASS")

# Transform eagles data to a tibble
eagle_df <- eagles %>% 
  as_tibble() %>% 
  mutate(pirateL = ifelse(P == "L", 1, 0),
         victimL = ifelse(V == "L", 1, 0),
         pirateA = ifelse(A == "A", 1, 0))

# Fit the initial model
m11h2 <- brm(y | trials(n) ~ 1 + pirateL + victimL + pirateA,
             data = eagle_df, family = binomial,
             prior = c(prior(normal(0, 1.5), class = Intercept),
                       prior(normal(0, 1), class = b)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234)

# Plot the posterior predictions
eagle_df %>% 
  add_epred_draws(m11h2) %>% 
  mutate(label = paste0(P, A, V)) %>% 
  ggplot(aes(x = label, y = .epred)) +
  stat_pointinterval(aes(color = "Posterior"), .width = 0.89, size = 5) +
  geom_point(data = eagle_df, size = 2,
             aes(x = paste0(P, A, V), y = y, color = "Observed")) +
  scale_color_manual(values = c("Posterior" = "red",
                                "Observed" = "blue"),
                     name = NULL) +
  labs(x = "Case", y = "Successes")

# Improve the model by adding interaction
m11h2c <- brm(y | trials(n) ~ 1 + pirateL * pirateA + victimL,
              data = eagle_df, family = binomial,
              prior = c(prior(normal(0, 1.5), class = Intercept),
                        prior(normal(0, 1), class = b)),
              iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234)

# Calculate WAIC and compare models
m11h2 <- add_criterion(m11h2, criterion = "waic")
m11h2c <- add_criterion(m11h2c, criterion = "waic")

loo_compare(m11h2, m11h2c, criterion = "waic")

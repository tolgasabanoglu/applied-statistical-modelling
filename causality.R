# Load required libraries
library(brms)
library(MASS)
library(rethinking)
library(tidyverse)
library(dplyr)
library(loo)
library(dagitty)
library(ggplot2)
library(ggdag)
library(kableExtra)
library(broom.mixed)
library(knitr)
library(ggraph)
library(igraph)
library(tidybayes)

# Load the data and standardize variables (except for "group")
data("foxes")

dat_foxes <- foxes %>%
  as_tibble() %>%
  mutate(across(-group, scale))

# Define the Bayesian model for area on weight using brm()
m_foxes1 <- brm(
  weight ~ 1 + area,  
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),
    prior(normal(0, 0.5), class = "b", coef = "area"),  
    prior(exponential(1), class = "sigma")
  ),
  chains = 4, cores = 4, seed = 1234
)

# Prior predictive simulation for area on weight
n <- 1000
prior_sim <- tibble(
  group = seq_len(n),
  alpha = rnorm(n, 0, 0.2),
  beta = rnorm(n, 0, 0.5)
) %>%
  expand(nesting(group, alpha, beta),
         area = seq(from = -2, to = 2, length.out = 100)) %>%
  mutate(weight = alpha + beta * area)

# Plot prior predictive simulation
ggplot(prior_sim, aes(x = area, y = weight, group = group)) +
  geom_line(alpha = 1 / 10) +
  geom_hline(yintercept = c((0 - mean(foxes$weight)) / sd(foxes$weight),
                            (max(foxes$weight) - mean(foxes$weight)) /
                              sd(foxes$weight)),
             linetype = c("dashed", "solid"), color = "red") +
  annotate(geom = "text", x = -2, y = -3.83, hjust = 0, vjust = 1,
           label = "No weight") +
  annotate(geom = "text", x = -2, y = 2.55, hjust = 0, vjust = 0,
           label = "Maximum weight") +
  expand_limits(y = c(-4, 4)) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x = "Standardized Area", y = "Standardized Weight")

# Summary of the model
summary(m_foxes1)

# Define the Bayesian model for avgfood on weight using brm()
m_foxes2 <- brm(
  weight ~ 1 + avgfood,  
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),
    prior(normal(0, 0.5), class = "b", coef = "avgfood"),  
    prior(exponential(1), class = "sigma")
  ),
  chains = 4, cores = 4, seed = 1234
)

# Summary of the model
summary(m_foxes2)

# Define the Bayesian model for groupsize on weight using brm()
m_foxes3 <- brm(
  weight ~ 1 + avgfood + groupsize,  
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),
    prior(normal(0, 0.5), class = "b", coef = "avgfood"),  
    prior(normal(0, 0.5), class = "b", coef = "groupsize"),  
    prior(exponential(1), class = "sigma")
  ),
  chains = 4, cores = 4, seed = 1234
)

# Summary of the model
summary(m_foxes3)

# Model comparison using WAIC
# Define models
m1 <- brm(weight ~ 1 + avgfood + groupsize + area, data = dat_foxes,
          prior = c(
            prior(normal(0, 0.2), class = "Intercept"),  
            prior(exponential(1), class = "sigma"),
            prior(normal(0, 0.5), class = "b", coef = "avgfood"),
            prior(normal(0, 0.5), class = "b", coef = "groupsize"),
            prior(normal(0, 0.5), class = "b", coef = "area")
          ), chains = 4, cores = 4, seed = 1234)

m2 <- brm(weight ~ 1 + avgfood + groupsize, data = dat_foxes,
          prior = c(
            prior(normal(0, 0.2), class = "Intercept"),  
            prior(exponential(1), class = "sigma"),
            prior(normal(0, 0.5), class = "b", coef = "avgfood"),
            prior(normal(0, 0.5), class = "b", coef = "groupsize")
          ), chains = 4, cores = 4, seed = 1234)

m3 <- brm(weight ~ 1 + groupsize + area, data = dat_foxes,
          prior = c(
            prior(normal(0, 0.2), class = "Intercept"),  
            prior(exponential(1), class = "sigma"),
            prior(normal(0, 0.5), class = "b", coef = "groupsize"),
            prior(normal(0, 0.5), class = "b", coef = "area")
          ), chains = 4, cores = 4, seed = 1234)

m4 <- brm(weight ~ 1 + avgfood, data = dat_foxes,
          prior = c(
            prior(normal(0, 0.2), class = "Intercept"),  
            prior(exponential(1), class = "sigma"),
            prior(normal(0, 0.5), class = "b", coef = "avgfood")
          ), chains = 4, cores = 4, seed = 1234)

m5 <- brm(weight ~ 1 + area, data = dat_foxes,
          prior = c(
            prior(normal(0, 0.2), class = "Intercept"),  
            prior(exponential(1), class = "sigma"),
            prior(normal(0, 0.5), class = "b", coef = "area")
          ), chains = 4, cores = 4, seed = 1234)

# Compute WAIC for each model
waic_m1 <- loo(m1)
waic_m2 <- loo(m2)
waic_m3 <- loo(m3)
waic_m4 <- loo(m4)
waic_m5 <- loo(m5)

# Compare models using WAIC
model_comparison <- loo::compare(waic_m1, waic_m2, waic_m3, waic_m4, waic_m5)

# Print the model comparison results
print(model_comparison)

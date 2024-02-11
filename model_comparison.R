# Load the required libraries
library(brms)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(ggdag)
library(dagitty)
library(dplyr)
library(rethinking)

# Define DAG structure
dag <-
  tibble(name = c("S", "A", "M", "D"),
         x = c(1, 1, 2, 3),
         y = c(3, 1, 2, 1))

# Plot the DAG
dagify(D ~ A + M,
       M ~ A + S,
       A ~ S,
       coords = dag) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_text(color = "black", size = 10) +
  geom_dag_edges(edge_color = "black", edge_width = 2,
                 arrow_directed = grid::arrow(length = grid::unit(15, "pt"),
                                              type = "closed")) +
  theme_void()

# Load WaffleDivorce data
data("WaffleDivorce")

# Prepare data for the model
south_divorce <- WaffleDivorce %>%
  as_tibble() %>%
  select(D = Divorce,
         A = MedianAgeMarriage,
         M = Marriage,
         S = South) %>%
  drop_na(everything()) %>%
  mutate(across(where(is.double), standardize))

# Fit the model
model <- brm(D ~ A + M + S, data = south_divorce, family = gaussian,
            prior = c(prior(normal(0, 0.2), class = Intercept),
                      prior(normal(0, 0.5), class = b),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234)

# Display model summary
summary(model)

# Plot model diagnostics
plot(model)

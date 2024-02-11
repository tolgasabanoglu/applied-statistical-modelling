# Load the required libraries
library(brms)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)
library(rethinking)
library(entropy)

# Create the data frame
df <- tibble(
  island = paste("Island", 1:3),
  a = c(0.2, 0.8, 0.05),
  b = c(0.2, 0.1, 0.15),
  c = c(0.2, 0.05, 0.7),
  d = c(0.2, 0.025, 0.05),
  e = c(0.2, 0.025, 0.05)
) %>%
  pivot_longer(-island, names_to = "species", values_to = "prop")

# Calculating entropy
p_logp <- function(p) {
  if (p == 0) return(0)
  p * log(p)
}

calc_entropy <- function(x) {
  avg_logprob <- sum(map_dbl(x, p_logp))
  -1 * avg_logprob
}

df %>%
  group_by(island) %>%
  summarize(prop = list(prop), .groups = "drop") %>%
  mutate(entropy = map_dbl(prop, calc_entropy))

# Calculating KL divergence
div <- function(p, q) {
  sum(p * (log(p) - log(q)))
}

crossing(model = paste("Island", 1:3), predicts = paste("Island", 1:3)) %>%
  filter(model != predicts) %>%
  left_join(df, by = c("model" = "island")) %>%
  rename(model_prop = prop) %>%
  left_join(df, by = c("predicts" = "island", "species")) %>%
  rename(predict_prop = prop) %>%
  group_by(model, predicts) %>%
  summarize(q = list(model_prop),
            p = list(predict_prop),
            .groups = "drop") %>%
  mutate(div = map2_dbl(p, q, div))

# Divorce rate exercise
# McElreath page 125
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

# standardize
d$d <- (d$Divorce - mean(d$Divorce)) / sd(d$Divorce)
d$m <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)
d$a <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)
# except S, which is an indicator variable
# turn this into an index variable (factor)
d$s <- as.factor(d$South)

library(brms)

m1.5H4 <- brm(
  data = d,
  family = gaussian(link = "identity"),
  d ~ 1 + a,
  prior = c(prior(normal(0, 2.5), class = b),
            prior(exponential(1), class = sigma)),
  iter = 5e3
)

m3.5H4 <- brm(
  data = d,
  family = gaussian(link = "identity"),
  d ~ 0 + s,
  prior = c(prior(normal(0, 2.5), class = b),
            prior(exponential(1), class = sigma)),
  iter = 5e3
)

m4.5H4 <- brm(
  data = d,
  family = gaussian(link = "identity"),
  d ~ 0 + s + a,
  prior = c(prior(normal(0, 2.5), class = b),
            prior(exponential(1), class = sigma)),
  iter = 5e3
)

# adding criterion
m1 <- add_criterion(m1.5H4, criterion = c("waic", "loo"))
m3 <- add_criterion(m3.5H4, criterion = c("waic", "loo"))
m4 <- add_criterion(m4.5H4, criterion = c("waic", "loo"))

# comparing the models
loo_compare(m1, m3, m4, criterion = "waic")
loo_compare(m1, m3, m4, criterion = "loo")

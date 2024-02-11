---
title: "Assignment 9"
author: "Tolga Şabanoğlu"
date: "2023-07-13"
output:
  html_document: default
  pdf_document: default
---

__13E2. Rewrite the following model as a multilevel model.__
yi ∼ Binomial(1, pi)
logit(pi) = αgroup[i] + βxi 
αgroup ∼ Normal(0, 1.5) 
β ∼ Normal(0, 0.5)

__multilevel model:__

yi ∼ Binomial(1, pi)
logit(pi) = αgroup[i] + βxi
αgroup ∼ Normal(¯α, σα)
β ∼ Normal(0, 1)
α¯ ∼ Normal(0, 10)
σα ∼ HalfCauchy(0, 1)


__13E3. Rewrite the following model as a multilevel model.__
yi ∼ Normal(μi, σ)
μi = αgroup[i] + βxi αgroup ∼ Normal(0, 5) β ∼ Normal(0, 1)
σ ∼ Exponential(1)


__multilevel model:__
yi ∼ Normal(µi, σ)
µi = αgroup[i] + βxi
αgroup ∼ Normal(¯α, σα)
β ∼ Normal(0, 1)
σ ∼ HalfCauchy(0, 1)
α¯ ∼ Normal(0, 10)
σα ∼ HalfCauchy(0, 1)



__13M1. Revisit the Reed frog survival data, data(reedfrogs), and add the predation and size treatment variables to the varying intercepts model. Consider models with either main effect alone, both main effects, as well as a model including both and their interaction. Instead of focusing on inferences about these two predictor variables, focus on the inferred variation across tanks. Explain why it changes as it does across models.__

```{r, warning=FALSE, message=FALSE, error=FALSE}

library(brms)
library(rethinking)
library(broom.mixed)

data(reedfrogs)
d <- reedfrogs
str(d)


d$pred <- ifelse( d$pred=="no" , 0 , 1 )
d$big <- ifelse( d$size=="big" , 1 , 0)

d$tank <- 1:nrow(d)

# no predictors
m0 <- brm(
  surv | trials(density) ~ 1 + (1 | tank),
  data = d,
  family = binomial(),
  prior = c(
    prior(normal(0, 10), class = Intercept),
    prior(cauchy(0, 1), class = sd)
  ),
  chains = 4
)


# presence of predators
m_p <- brm(
  surv | trials(density) ~ 1 + pred + (1 | tank),
  data = d,
  family = binomial(),
  prior = c(prior(normal(0, 1.5), class = Intercept),  # alpha bar
                prior(exponential(1), class = sd)),
  chains = 4
)


#only the dummy variable indicating big tadpoles
m_b <- brm(
  surv | trials(density) ~ 1 + big + (1 | tank),
  data = d,
  family = binomial(),
  prior = c(prior(normal(0, 1.5), class = Intercept),  # alpha bar
                prior(exponential(1), class = sd)),
  chains = 4
)


# containing both predictors and a model that contains their interaction
m_p_b <- brm(
  surv | trials(density) ~ 1 + pred + big + (1 | tank),
  data = d,
  family = binomial(),
  prior = c(prior(normal(0, 1.5), class = Intercept),  # alpha bar
                prior(exponential(1), class = sd)),
  chains = 4
)


# Model formulation (bp, bb, and bpb)
m_p_b_pb <- brm(
  surv | trials(density) ~ 1 + pred * big + pred + big + (1 | tank),
  data = d,
  family = binomial(),
  prior = c(prior(normal(0, 1.5), class = Intercept),  # alpha bar
                prior(exponential(1), class = sd)),
  chains = 4
)


# Create a list of the models
models <- list(m0, m_p, m_b, m_p_b, m_p_b_pb)

# Extract the coefficients for all models
coefficients <- lapply(models, tidy)

# Combine the coefficients into a single data frame
coefficients_df <- do.call(rbind, coefficients)

# View the coefficients
coefficients_df




```

One observation is that the variable "big" has a smaller impact on reducing the variation compared to the variable "pred." In these models, the predictor "big" does not contribute significantly to the prediction, and accounting for it has minimal effect on the estimated variation across tanks.

__13M2. Compare the models you fit just above, using WAIC. Can you reconcile the differences in WAIC with the posterior distributions of the models?__

```{r, warning=FALSE, message=FALSE, error=FALSE}


m0 <- add_criterion(m0, "waic")
m_p <- add_criterion(m_p, "waic")
m_b <- add_criterion(m_b, "waic")
m_p_b <- add_criterion(m_p_b, "waic")
m_p_b_pb <- add_criterion(m_p_b_pb, "waic")


com <- loo_compare(m0, m_b, m_p_b, m_p_b_pb, criterion = "waic")

print(com, simplify = F)


```
In the table, m0 and m_p_b generally perform better than m_b based on these metrics.

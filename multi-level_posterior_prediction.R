# Load the required libraries
library(brms)
library(rethinking)
library(broom.mixed)

# 13E2: Multilevel model for binomial outcome
model_13E2 <- brm(
  yi | trials(1) ~ 1 + xi + (1 | group),
  data = your_data,
  family = binomial(),
  prior = c(
    prior(normal(0, 1.5), class = Intercept),  # alpha bar
    prior(normal(0, 0.5), class = b),
    prior(normal(0, 10), class = Intercept_group),  # alpha bar
    prior(halfcauchy(1), class = sigma_group)
  )
)

# 13E3: Multilevel model for normal outcome
model_13E3 <- brm(
  yi ~ 1 + xi + (1 | group),
  data = your_data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 5), class = Intercept_group),  # alpha bar
    prior(normal(0, 1), class = b),
    prior(halfcauchy(1), class = sigma)
  )
)

# 13M1: Multilevel model for Reed frog survival data
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

m_p <- brm(
  surv | trials(density) ~ 1 + pred + (1 | tank),
  data = d,
  family = binomial(),
  prior = c(prior(normal(0, 1.5), class = Intercept),  # alpha bar
            prior(exponential(1), class = sd)),
  chains = 4
)

m_b <- brm(
  surv | trials(density) ~ 1 + big + (1 | tank),
  data = d,
  family = binomial(),
  prior = c(prior(normal(0, 1.5), class = Intercept),  # alpha bar
            prior(exponential(1), class = sd)),
  chains = 4
)

m_p_b <- brm(
  surv | trials(density) ~ 1 + pred + big + (1 | tank),
  data = d,
  family = binomial(),
  prior = c(prior(normal(0, 1.5), class = Intercept),  # alpha bar
            prior(exponential(1), class = sd)),
  chains = 4
)

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

# 13M2: Compare models using WAIC
m0 <- add_criterion(m0, "waic")
m_p <- add_criterion(m_p, "waic")
m_b <- add_criterion(m_b, "waic")
m_p_b <- add_criterion(m_p_b, "waic")
m_p_b_pb <- add_criterion(m_p_b_pb, "waic")

com <- loo_compare(m0, m_b, m_p_b, m_p_b_pb, criterion = "waic")

print(com, simplify = F)

# Part 14E1

# Load the required libraries
library(brms)

# Specify the model
model_14E1 <- brm(
  yi ~ 1 + x + (1 + x | group),
  data = your_data,  # Replace 'your_data' with your actual dataset
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(exponential(1), class = "sigma"),
    prior(normal(0, 10), class = "Intercept", group = "group"),
    prior(normal(0, 1), class = "b", group = "group"),
    prior(exponential(1), class = "sigma", group = "group")
  ),
  chains = 4
)

# Print the summary of the model
summary(model_14E1)


# Part 14M1

# Simulation setup
a <- 3.5
b <- -1
sigma_a <- 1
sigma_b <- 0.5
rho <- 0

mu <- c(a, b)
cov_ab <- sigma_a * sigma_b * rho
sigma <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol = 2)

# Simulate data
set.seed(5)
vary_effects <- MASS::mvrnorm(20, mu, sigma)
head(vary_effects)

# Plot correlation
ggplot(vary_effects, aes(x = a_cafe, y = b_cafe)) +
  geom_point(color = "#80A0C7") +
  geom_rug(color = "#8B9DAF", linewidth = 1/7) +
  labs(title = "Correlation between intercepts and slopes")

# Fit the multilevel model
b14.2 <- brm(
  wait ~ 1 + afternoon + (1 + afternoon | cafe),
  data = d,
  family = gaussian,
  prior = c(
    prior(normal(5, 2), class = "Intercept"),
    prior(normal(-1, 0.5), class = "b"),
    prior(exponential(1), class = "sd"),
    prior(exponential(1), class = "sigma"),
    prior(lkj(2), class = "cor")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 867530
)

# Calculate WAIC
b14.1_waic <- loo(b14.1)
b14.2_waic <- loo(b14.2)

# Compare WAIC
loo_compare(b14.1_waic, b14.2_waic)

# Load the required libraries
library(rethinking)
library(brms)

# Given data
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,
            0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,
            0,1,1,1,1,1,0,0,1,0,1,1,0,1,0,1,1,1,0,1,1,1,1)

birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,
            1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,
            1,0,0,1,0,0,1,1,0,0,0,1,1,1,0,0,0,0)

# 3H1. Using grid approximation, compute the posterior distribution for the probability of a birth being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior probability?

# Define the grid
p_grid <- seq(0, 1, length.out = 1000)

# Define the prior probability
prior <- rep(1, length(p_grid))

# Compute the likelihood
likelihood <- dbinom(sum(birth1), size = length(birth1), prob = p_grid) *
              dbinom(sum(birth2), size = length(birth2), prob = p_grid)

# Unstandardized posterior
unstd_posterior <- prior * likelihood

posterior <- unstd_posterior / sum(unstd_posterior)

# Plot
plot(posterior ~ p_grid, type = "l")

# Find the parameter value that maximizes the posterior probability
max_posterior_idx <- which.max(posterior)
max_posterior_value <- p_grid[max_posterior_idx]

max_posterior_value

# 3H2. Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above. Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals.

# Drawing 10,000 random parameter values
samples <- sample(p_grid, size = 10000, replace = TRUE, prob = posterior)

HPDI(samples, prob = 0.50)
HPDI(samples, prob = 0.89)
HPDI(samples, prob = 0.97)

# 3H3. Use rbinom to simulate 10,000 replicates of 200 births. You should end up with 10,000 numbers, each one a count of boys out of 200 births. Compare the distribution of predicted numbers of boys to the actual count in the data (111 boys out of 200 births). There are many good ways to visualize the simulations, but the dens command (part of the rethinking package) is probably the easiest way in this case. Does it look like the model fits the data well? That is, does the distribution of predictions include the actual observation as a central, likely outcome?

# Simulate 10,000 replicates of 200 births using the parameter values sampled from the posterior distribution
n_replicates <- 10000
n_births <- 200

simulated_counts <- rbinom(n_replicates, size = n_births, prob = samples)

# Plot
dens(simulated_counts, adj = 0.1)

# 4E3
# Using the model definition above, write down the appropriate form of Bayes’ theorem that includes the proper likelihood and priors

likelihood <- yi ~ dnorm(mu, sigma)
prior_mu <- mu ~ dnorm(0, 10)
prior_sigma <- sigma ~ dexp(1)

bayes_theorem <- expression(likelihood * prior_mu * prior_sigma)

# 4M1
# For the model definition below, simulate observed y values from the prior (not the posterior)

num_obs <- 10000

# Step 1: Simulate values for μ and σ from the priors
μ_0 <- rnorm(1, 0, 10)
σ_0 <- rexp(1, 1)

# Step 2: Simulate observed y values from the Normal distribution
y_values <- rnorm(num_obs, μ_0, σ_0)

# Plotting the density of simulated observed y values
plot(density(y_values), main = "Density of Simulated Observed y Values", xlab = "y", ylab = "Density")

# 4H2
# Select out all the rows in the Howell1 data with ages below 18 years of age.
# If you do it right, you should end up with a new data frame with 192 rows in it

# (a) Fit a linear regression to these data, using quap.
# Present and interpret the estimates. For every 10 units of increase in weight,
# how much taller does the model predict a child gets?

library(rethinking)
library(brms)

data(Howell1)

# Select rows with ages below 18
below_18_df <- Howell1[Howell1$age < 18, ]

# Check the number of rows in the new data frame
nrow(below_18_df)  

# Fit a linear regression model
model <- brm(height ~ weight, data = below_18_df)

# Summarize the model estimates
summary(model)

# (b) Plot the raw data, with height on the vertical axis and weight on the horizontal axis.
# Superimpose the MAP regression line and 89% interval for the mean.
# Also superimpose the 89% interval for predicted heights

library(ggplot2)
library(brms)
library(rethinking)

data(Howell1)

# Fit a Bayesian linear regression model
fit <- brm(height ~ weight, data = Howell1)

# Extract the predicted heights and calculate the 89% interval for predicted heights
pred <- posterior_predict(fit, newdata = Howell1)
pred_mean <- apply(pred, 2, mean)
pred_interval <- apply(pred, 2, function(x) quantile(x, c(0.055, 0.945)))

# Extract the population-level effects
intercept <- fixef(fit)["Intercept"]
slope <- fixef(fit)["weight"]

# Plot the data, regression line, and prediction interval
ggplot(Howell1, aes(x = weight, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", 
              linetype = "dashed", aes(x = weight, y = pred_mean)) +
  geom_ribbon(aes(ymin = pred_interval[1,], ymax = pred_interval[2,]), 
              fill = "lightblue", alpha = 0.5) +
  labs(x = "Weight", y = "Height") +
  ggtitle("Howell1 Data with MAP Regression Line and 89% Prediction Interval") +
  theme_minimal()


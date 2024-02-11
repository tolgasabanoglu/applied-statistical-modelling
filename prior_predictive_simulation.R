---
title: "Assignment 2"
name: "Tolga Şabanoğlu"
date: "2023-05-11"
output: html_document

---

The Hard problems here all use the data below. These data indicate the gender (male=1, female=0) of officially reported first and second born children in 100 two-child families.

```{r}

birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
1,0,1,1,1,0,1,1,1,1)

birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
0,0,0,1,1,1,0,0,0,0)

```

So for example, the first family in the data reported a boy (1) and then a girl (0). The second family reported a girl (0) and then a boy (1). The third family reported two girls. You can load these two vectors into R’s memory by typing:

```{r}

library(rethinking)
data(homeworkch3)

```

Use these vectors as data. So for example to compute the total number of boys born across all of these births, you could use:

```{r}

sum(birth1) + sum(birth2)

```

3H1. Using grid approximation, compute the posterior distribution for the probability of a birth being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior probability?

```{r}

# Define the grid
p_grid <- seq(0, 1, length.out = 1000)

# Define the prior probability
prior <- rep(1, length(p_grid))

# Compute the likelihood
likelihood <- dbinom(sum(birth1), size = length(birth1), prob = p_grid) *
              dbinom(sum(birth2), size = length(birth2), prob = p_grid)

# Undstandardized posterior
unstd_posterior <- prior * likelihood

posterior <- unstd_posterior / sum(unstd_posterior)

#Plot 
plot(posterior ~ p_grid, type ="l")

#Find the parameter value that maximizes the posterior probability
max_posterior_idx <- which.max(posterior)
max_posterior_value <- p_grid[max_posterior_idx]

max_posterior_value

```


3H2. Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above. Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals.


```{r}

# Drawing 10,000 random parameter values 
samples <- sample(p_grid, size=10000, replace= TRUE, prob=posterior)

HPDI(samples,prob=0.50)
HPDI(samples,prob=0.89)
HPDI(samples,prob=0.97)

```


3H3. Use rbinom to simulate 10,000 replicates of 200 births. You should end up with 10,000 numbers, each one a count of boys out of 200 births. Compare the distribution of predicted numbers of boys to the actual count in the data (111 boys out of 200 births). There are many good ways to visualize the simulations, but the dens command (part of the rethinking package) is probably the easiest way in this case. Does it look like the model fits the data well? That is, does the distribution of predictions include the actual observation as a central, likely outcome?


```{r}

# Simulate 10,000 replicates of 200 births using the parameter values sampled from the posterior distribution
n_replicates <- 10000
n_births <- 200

simulated_counts <- rbinom(n_replicates, size = n_births, prob = samples)

# Plot
dens(simulated_counts, adj=0.1)


```


## Working with brms:

3H1. Using grid approximation, compute the posterior distribution for the probability of a birth being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior probability?



```{r}

library(brms)

# Combine the birth data into a single vector
birth_data <- c(birth1, birth2)

# Create a data frame with the combined data
data <- data.frame(birth = birth_data)

# Create a formula for the model
formula <- bf(birth ~ 1)

# Specify the uniform prior for the probability of a birth being a boy
prior <- prior("uniform(0,1)", class = "Intercept")

# Run the Bayesian logistic regression model using brm
model <- brm(formula, data = data, family = binomial(), prior = prior, chains = 4)

# Summarize the model to see the posterior distribution
summary(model)



```


3H2. Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above. Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals.



```{r}

# Drawing 10,000 random parameter values 
samples <- sample(p_grid, size=10000, replace= TRUE, prob=posterior)

HPDI(samples,prob=0.50)
HPDI(samples,prob=0.89)
HPDI(samples,prob=0.97)


```


3H3. Use rbinom to simulate 10,000 replicates of 200 births. You should end up with 10,000 num- bers, each one a count of boys out of 200 births. Compare the distribution of predicted numbers of boys to the actual count in the data (111 boys out of 200 births). There are many good ways to visualize the simulations, but the dens command (part of the rethinking package) is probably the easiest way in this case. Does it look like the model fits the data well? That is, does the distribution of predictions include the actual observation as a central, likely outcome?


```{r}

library(brms)

# Create a data frame with the birth data
data <- data.frame(birth1)

# Model specification
model <- brm(birth1 | trials(200) ~ 1, data = data, family = binomial())

# Check the number of draws generated by the model
max_draws <- nrow(model$draws)

# Set the number of draws for simulation
ndraws <- min(10000, max_draws)  # Choose a value between 1 and the maximum number of draws

# Simulate counts of boys
simulations <- posterior_predict(model, draws = ndraws)

# Extract the count of boys from each replicate
simulated_counts <- apply(simulations, 1, sum)

# Actual count of boys
actual_count <- sum(birth1)

# Plotting the distribution of predicted numbers of boys
hist(simulated_counts, breaks = seq(0, 200, by = 5), col = "lightblue", xlab = "Number of Boys",
     ylab = "Frequency", main = "Distribution of Predicted Boys")

# Add a vertical line for the actual count of boys
abline(v = actual_count, lwd = 2, col = "red")


```




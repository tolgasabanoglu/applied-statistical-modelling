---
title: "Assignment 11"
author: "Tolga Şabanoğlu"
date: "2023-07-20"
output:
  html_document: default
  pdf_document: default
---


__14E1. Add to the following model varying slopes on the predictor x.__

yi ∼ Normal(μi, σ)
μi = αgroup[i] + βxi 
αgroup ∼ Normal(α, σα)
α ∼ Normal(0, 10) 
β ∼ Normal(0, 1) 
σ ∼ Exponential(1)
σα ∼ Exponential(1)

```{r, warning=FALSE, echo=FALSE, message=FALSE, error=FALSE}

knitr::include_graphics("/Users/tolgasabanoglu/Library/CloudStorage/GoogleDrive-tolgasabanoglu@gmail.com/My Drive/summer semester 23/Applied statistical modeling/varyingslope.jpg")

```


__14M1. Repeat the café robot simulation from the beginning of the chapter. This time, set rho to zero, so that there is no correlation between intercepts and slopes. How does the posterior distribution of the correlation reflect this change in the underlying simulation?__

```{r, warning=FALSE, message=FALSE, error=FALSE}

library(brms)
library(MASS)
library(rethinking)
library(tidyverse)
library(dplyr)
library(loo)

a <-  3.5  # average morning wait time
b <- -1    # average difference afternoon wait time
sigma_a <-  1    # std dev in intercepts
sigma_b <-  0.5  # std dev in slopes
rho <- 0   # correlation between intercepts and slopes

# the next three lines of code simply combine the terms, above
mu <- c(a, b)

cov_ab <- sigma_a * sigma_b * rho
sigma  <- matrix(c(sigma_a^2, cov_ab, 
                   cov_ab, sigma_b^2), ncol = 2)


matrix(1:4, nrow = 2, ncol = 2)


sigmas <- c(sigma_a, sigma_b)          # standard deviations
rho <- matrix(c(1, rho,             # correlation matrix
                   rho, 1), nrow = 2)

# now matrix multiply to get covariance matrix
sigma <- diag(sigmas) %*% rho %*% diag(sigmas)

# how many cafes would you like?
n_cafes <- 20

set.seed(5)  # used to replicate example

vary_effects <- 
  MASS::mvrnorm(n_cafes, mu, sigma) %>% 
  data.frame() %>% 
  set_names("a_cafe", "b_cafe")

head(vary_effects)


vary_effects %>% 
  ggplot(aes(x = a_cafe, y = b_cafe)) +
  geom_point(color = "#80A0C7") +
  geom_rug(color = "#8B9DAF", linewidth = 1/7)


cor(vary_effects$a_cafe, vary_effects$b_cafe)


n_visits <- 10
sigma    <-  0.5  # std dev within cafes

set.seed(22)  # used to replicate example

select_dplyr <- dplyr::select

d <-
  vary_effects %>% 
  mutate(cafe = 1:n_cafes) %>% 
  expand_grid(visit = 1:n_visits) %>% 
  mutate(afternoon = rep(0:1, times = n() / 2)) %>% 
  mutate(mu = a_cafe + b_cafe * afternoon) %>% 
  mutate(wait = rnorm(n = n(), mean = mu, sd = sigma)) %>% 
  select_dplyr(cafe, everything())

# fit the model
b14.1 <- 
  brm(data = d, 
      family = gaussian,
      wait ~ 1 + afternoon + (1 + afternoon | cafe),
      prior = c(prior(normal(5, 2), class = Intercept),
                prior(normal(-1, 0.5), class = b),
                prior(exponential(1), class = sd),
                prior(exponential(1), class = sigma),
                prior(lkj(2), class = cor)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 867530,
      )

# The varying slopes model

n_sim <- 1e4

set.seed(14)
r_1 <- 
  rlkjcorr(n_sim, K = 2, eta = 1) %>%
  data.frame()

set.seed(14)
r_2 <- 
  rlkjcorr(n_sim, K = 2, eta = 2) %>%
  data.frame()

set.seed(14)
r_4 <- 
  rlkjcorr(n_sim, K = 2, eta = 4) %>%
  data.frame()


# for annotation
text <-
  tibble(x     = c(.83, .625, .45),
         y     = c(.56, .75, 1.07),
         label = c("eta = 1", "eta = 2", "eta = 4"))

# plot
r_1 %>% 
  ggplot(aes(x = X2)) +
  geom_density(color = "transparent", fill = "#394165", alpha = 2/3, adjust = 1/2) +
  geom_density(data = r_2,
               color = "transparent", fill = "#DCA258", alpha = 2/3, adjust = 1/2) +
  geom_density(data = r_4,
               color = "transparent", fill = "#FCF9F0", alpha = 2/3, adjust = 1/2) +
  geom_text(data = text,
            aes(x = x, y = y, label = label),
            color = "#A65141", family = "Courier") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(LKJcorr(eta)),
       x = "correlation")


post <- as_draws_df(b14.1)

post %>%
  ggplot() +
  geom_density(data = r_2, aes(x = X2),
               color = "transparent", fill = "#EEDA9D", alpha = 3/4) +
  geom_density(aes(x = cor_cafe__Intercept__afternoon),
               color = "transparent", fill = "#A65141", alpha = 9/10) +
  annotate(geom = "text", 
           x = c(-0.15, 0), y = c(2.21, 0.85), 
           label = c("posterior", "prior"), 
           color = c("#A65141", "#EEDA9D"), family = "Courier") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "Correlation between intercepts\nand slopes, prior and posterior",
  x = "correlation")

```


__14M2. Fit this multilevel model to the simulated café data:__

Wi ∼ Normal(μi, σ)
μi = αcafé[i] + βcafé[i]Ai 
αcafé ∼ Normal(α, σα) 
βcafé ∼Normal(β,σβ)
α ∼ Normal(0, 10)
β ∼ Normal(0, 10) σ, σα, 
σβ ∼ Exponential(1)

Use WAIC to compare this model to the model from the chapter, the one that uses a multi-variate Gaussian prior. Explain the result.

```{r, warning=FALSE, message=FALSE, error=FALSE}

# set up parameters of population
a <- 3.5 # average morning wait time
b <- (-1) # average difference afternoon wait time
sigma_a <- 1 # std dev in intercepts
sigma_b <- 0.5 # std dev in slopes
rho <- (-0.7) # correlation between intercepts and slopes
Mu <- c( a , b )
cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix( c(sigma_a^2,cov_ab,cov_ab,sigma_b^2) , ncol=2 )

# simulate observations
N_cafes <- 20
set.seed(5) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]
N_visits <- 10
afternoon <- rep(0:1,N_visits*N_cafes/2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5 # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )
# package into data frame
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )


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

```



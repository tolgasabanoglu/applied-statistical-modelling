---
title: "Assignment 8"
author: "Tolga Şabanoğlu"
date: "2023-06-28"
output:
  html_document: default
  pdf_document: default
---


__11H2. The data contained in library(MASS);data(eagles) are records of salmon pirating attempts by Bald Eagles in Washington State. See ?eagles for details. While one eagle feeds, sometimes another will swoop in and try to steal the salmon from it. Call the feeding eagle the “victim” and the thief the “pirate.” Use the available data to build a binomial GLM of successful pirating attempts.__
(a) Consider the following model:

yi ∼ Binomial(ni, pi)
logit(pi) = α + βPPi + βVVi + βAAi
α ∼ Normal(0, 1.5) βP, βV, βA ∼ Normal(0, 0.5)

where y is the number of successful attempts, n is the total number of attempts, P is a dummy variable indicating whether or not the pirate had large body size, V is a dummy variable indicating whether or not the victim had large body size, and finally A is a dummy variable indicating whether or not the pirate was an adult. Fit the model above to the eagles data, using brms. Is the quadratic approximation okay?

```{r,  warning=FALSE}

library(rethinking)
library(tidyverse)
library(brms)
library(ggplot2)
library(dplyr)
library(modelr)
library(tidybayes)



data(eagles, package = "MASS")

eagle_df <- eagles %>% 
  as_tibble() %>% 
  mutate(pirateL = ifelse(P == "L", 1, 0),
         victimL = ifelse(V == "L", 1, 0),
         pirateA = ifelse(A == "A", 1, 0))


m11h2 <- brm(y | trials(n) ~ 1 + pirateL + victimL + pirateA,
                  data = eagle_df, family = binomial,
                  prior = c(prior(normal(0, 1.5), class = Intercept),
                            prior(normal(0, 1), class = b)),
                  iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234)



```


__(b) Plot the posterior predictions. Compute and display both (1) the predicted probability of success and its 89% interval for each row (i) in the data, as well as (2) the predicted success count and its 89% interval. What different information does each type of posterior prediction provide?__


```{r, warning=FALSE}

eagle_df %>% 
  add_epred_draws(m11h2) %>% 
  mutate(label = paste0(P, A, V)) %>% 
  ggplot(aes(x = label, y = .epred)) +
  stat_pointinterval(aes(color = "Posterior"), .width = 0.89, size = 5) +
  geom_point(data = eagle_df, size = 2,
             aes(x = paste0(P, A, V), y = y, color = "Observed")) +
  scale_color_manual(values = c("Posterior" = "red",
                                "Observed" = "blue"),
                     name = NULL) +
  labs(x = "Case", y = "Successes")



```

__(c) Now try to improve the model. Consider an interaction between the pirate’s size and age (immature or adult). Compare this model to the previous one, using WAIC. Interpret.__


```{r, warning=FALSE}

m11h2c <- brm(y | trials(n) ~ 1 + pirateL * pirateA + victimL,
                   data = eagle_df, family = binomial,
                   prior = c(prior(normal(0, 1.5), class = Intercept),
                             prior(normal(0, 1), class = b)),
                   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234)

m11h2 <- add_criterion(m11h2, criterion = "waic")
m11h2c <- add_criterion(m11h2c, criterion = "waic")

loo_compare(m11h2, m11h2c, criterion = "waic")
          

```

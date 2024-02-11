---
title: "Assignment 7"
author: "Tolga Şabanoğlu"
date: "2023-06-22"
output:
  html_document: default
  pdf_document: default
---


__8H1. Return to the data(tulips) example in the chapter. Now include the bed variable as a predictor in the interaction model. Don’t interact bed with the other predictors; just include it as a main effect. Note that bed is categorical. So to use it properly, you will need to either construct dummy variables or rather an index variable, as explained in Chapter 5__

```{r}

library(rethinking)
library(tidyverse)
library(brms)

data(tulips)
df <- tulips
str(df)


df_tulip <- tulips %>%
  as_tibble() %>%
  mutate(light = -1 * shade,
         blooms_std = blooms / max(blooms),
         water_cent = water - mean(water),
         shade_cent = shade - mean(shade),
         light_cent = light - mean(light))


b8h1 <- brm(blooms_std ~ 0 + water_cent + light_cent + bed + 
              water_cent:light_cent,
            data = df_tulip, family = gaussian,
            prior = c(prior(normal(0, 0.25), class = b),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = ("b8h1.rds"))

summary(b8h1)

```



__8H2. Use WAIC to compare the model from 8H1 to a model that omits bed. What do you infer from this comparison? Can you reconcile the WAIC results with the posterior distribution of the bed coefficients?__


```{rh2, warning=FALSE}

b8h2 <- brm(blooms_std ~ 1 + water_cent + light_cent + water_cent:light_cent,
            data = df_tulip, family = gaussian,
            prior = c(prior(normal(0.5, 0.25), class = Intercept),
                      prior(normal(0, 0.25), class = b, lb = 0),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = ("b8m4"))


b8h2 <- add_criterion(b8h2, criterion = "waic", "loo")
b8h1 <- add_criterion(b8h1, criterion = "waic", "loo")

# comparing the models 
loo_compare(b8h1, b8h2, criterion = "waic")


```




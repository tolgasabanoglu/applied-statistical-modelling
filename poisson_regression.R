---
title: "Assignment 9"
author: "Tolga Şabanoğlu"
date: "2023-07-03"
output:
  html_document: default
  pdf_document: default
---

__12H1. In 2014, a paper was published that was entitled "Female hurricanes are deadlier than male hurricanes." As the title suggests,the paper claimed that hurricanes with female names have caused greater loss of life, and the explanation given is that people unconsciously rate female hurricanes as less dangerous and so are less likely to evacuate. Statisticians severely criticized the paper after publication. Here, you’ll explore the complete data used in the paper and consider the hypothesis that hurricanes with female names are deadlier.__

Acquaint yourself with the columns by inspecting the help? Hurricanes. In this problem, you’ll focus on predicting deaths using femininity of each hurricane’s name. Fit and interpret the simplest possible model, a Poisson model of deaths using femininity as a predictor, using brms instead of quap() or ulam(). Compare the model to an intercept-only Poisson model of deaths. How strong is the association between femininity of name and deaths? Which storms does the model fit (retrodict) well? Which storms does it fit poorly?


```{r,  warning=FALSE, message = FALSE, error=FALSE, echo=FALSE}

library(rethinking)
library(tidyverse)
library(brms)
library(ggplot2)
library(dplyr)
library(loo)

data(Hurricanes)

# load data on object called d
d <- Hurricanes 

#standardised femininity
d$fem_std <- (d$femininity - mean(d$femininity)) / sd(d$femininity) # 

# fit the Poisson model
m12h1 <- brm(
  deaths ~ fem_std,
  data = d,
  family = poisson(),
  prior = c(prior(normal(0, 10), class = "Intercept"),
            prior(normal(0, 1), class = "b")),
  chains = 4
)

summary(m12h1)


# fit the intercept-only Poisson model 
m12h1int <- brm(
  deaths ~ 1,
  data = d,
  family = poisson(),
  prior = prior(normal(0, 10), class = "Intercept"),
  chains = 4
)

m12h1 <- add_criterion(m12h1, "loo")
m12h1int <- add_criterion(m12h1int, "loo")

loo_compare(m12h1, m12h1int, criterion = "loo") %>% print(simplify = F)


```
Model outcomes tell that there is a positive relationship between hurricane name femininity and death numbers. When comparing two models, a strong support for the model that includes femininity of names.


__12H2. Counts are nearly always over-dispersed relative to Poisson. So fit a gamma-Poisson (aka negative-binomial) model to predict deaths using femininity. Show that the over-dispersed model no longer shows as precise a positive association between femininity and deaths, with an 89% interval that overlaps zero. Can you explain why the association diminished in strength?__


```{r, warning=FALSE, message = FALSE, echo=FALSE, error=FALSE}

# fit the gamma-Poisson model with femininity as a predictor
m12h2gam <- brm(
  deaths ~ fem_std,
  data = d,
  family = negbinomial(),
  prior = c(prior(normal(0, 10), class = "Intercept"),
            prior(normal(0, 1), class = "b")),
  chains = 4
)


summary(m12h2gam)


```



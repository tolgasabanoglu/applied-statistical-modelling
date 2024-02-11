---
title: "Assignment 5"
author: "Tolga Şabanoğlu"
date: "2023-05-22"
output:
  html_document: default
  pdf_document: default
---


__5H4. Here is an open practice problem to engage your imagination. In the divorce date, States in the southern United States have many of the highest divorce rates. Add the South indicator variable to the analysis. First, draw one or more DAGs that represent your ideas for how Southern American culture might influence any of the other three variables (D, M or A). Then list the testable implications of your DAGs, if there are any, and fit one or more models to evaluate the implications. What do you think the influence of “Southerness” is?__


```{r, Warning= FALSE, message = FALSE}
# Load the required libraries
library(brms)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(ggdag)
library(dagitty)
library(dplyr)
library(rethinking)


dag <-
  tibble(name = c("S", "A", "M", "D"),
         x = c(1, 1, 2, 3),
         y = c(3, 1, 2, 1))

dagify(D ~ A + M,
       M ~ A + S,
       A ~ S,
       coords = dag) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_text(color = "black", size = 10) +
  geom_dag_edges(edge_color = "black", edge_width = 2,
                 arrow_directed = grid::arrow(length = grid::unit(15, "pt"),
                                              type = "closed")) +
  theme_void()

data("WaffleDivorce")

south_divorce <- WaffleDivorce %>%
  as_tibble() %>%
  select(D = Divorce,
         A = MedianAgeMarriage,
         M = Marriage,
         S = South) %>%
  drop_na(everything()) %>%
  mutate(across(where(is.double), standardize))

model <- brm(D ~ A + M + S, data = south_divorce, family = gaussian,
            prior = c(prior(normal(0, 0.2), class = Intercept),
                      prior(normal(0, 0.5), class = b),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234)
            

summary(model)

plot(model)

```

The effect of S is approximately 0.37, indicating that southerness is not very strong in relation to the other variables.

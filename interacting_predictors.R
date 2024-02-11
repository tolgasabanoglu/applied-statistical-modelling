---
title: "Assignment 6"
author: "Tolga Şabanoğlu"
date: "2023-06-22"
output:
  html_document: default
  pdf_document: default
---


__7H3. Consider three fictional Polynesian islands. On each there is a Royal Ornithologist charged by the king with surveying the bird population. They have each found the following proportions of 5 important bird species:__

```{r echo=FALSE, out.width = "30%", fig.align = "center"}
knitr::include_graphics("/Users/tolgasabanoglu/Desktop/7h3.png")
```

__Notice that each row sums to 1, all the birds. This problem has two parts. It is not computationally complicated. But it is conceptually tricky. First, compute the entropy of each island’s bird distribution. Interpret these entropy values.__


```{r, Warning= FALSE, message = FALSE}
# Load the required libraries
library(brms)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)
library(rethinking)
library(entropy)


# Create the data frame
df <- tibble(island = paste("Island", 1:3),
                  a = c(0.2, 0.8, 0.05),
                  b = c(0.2, 0.1, 0.15),
                  c = c(0.2, 0.05, 0.7),
                  d = c(0.2, 0.025, 0.05),
                  e = c(0.2, 0.025, 0.05)) %>%
  pivot_longer(-island, names_to = "species", values_to = "prop")


# Calculating entropy

p_logp <- function(p) {
  if (p == 0) return(0)
  p * log(p)
}

calc_entropy <- function(x) {
  avg_logprob <- sum(map_dbl(x, p_logp))
  -1 * avg_logprob
}


df %>%
  group_by(island) %>%
  summarize(prop = list(prop), .groups = "drop") %>%
  mutate(entropy = map_dbl(prop, calc_entropy))


```

__Second, use each island’s bird distribution to predict the other two. This means to compute the KL divergence of each island from the others, treating each island as if it were a statistical model of the other islands. You should end up with 6 different KL divergence values. Which island predicts the others best? Why?__

```{r, Warning= FALSE, message = FALSE}

# Calculating KL divergence
div <- function(p, q) {
  sum(p * (log(p) - log(q)))
}


crossing(model = paste("Island", 1:3),
         predicts = paste("Island", 1:3)) %>%
  filter(model != predicts) %>%
  left_join(df, by = c("model" = "island")) %>%
  rename(model_prop = prop) %>%
  left_join(df, by = c("predicts" = "island", "species")) %>%
  rename(predict_prop = prop) %>%
  group_by(model, predicts) %>%
  summarize(q = list(model_prop),
            p = list(predict_prop),
            .groups = "drop") %>%
  mutate(div = map2_dbl(p, q, div))



```
Overall, KL divergence between island 1 and island 2 is ~0.87, following by island 1 to island 3 with ~0.63. 



__Recall the divorce rate exercise from last week. Refit some of these models using brms, and compare them using the WAIC and PSIS-LOO estimates of ELPD. In particular, compare model m5.1 from the book with some of your models from last week including southernness as a predictor. Explain the model comparison results.__

```{r, Warning= FALSE, message = FALSE}

# McElreath page 125
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

# standardize
d$d <- (d$Divorce - mean(d$Divorce))/sd(d$Divorce)
d$m <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)
d$a <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)
# except S, which is an indicator variable
# turn this into an index variable (factor)
d$s <- as.factor(d$South)

library(brms)

m1.5H4 <-
  brm(data = d, 
      family = gaussian(link = "identity"),
      d ~ 1 + a,
      prior = c(prior(normal(0, 2.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 5e3
  )


m3.5H4 <-
  brm(data = d, 
      family = gaussian(link = "identity"),
      d ~ 0 + s,
      prior = c(prior(normal(0, 2.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 5e3
  )


m4.5H4 <-
  brm(data = d, 
      family = gaussian(link = "identity"),
      d ~ 0 + s + a,
      prior = c(prior(normal(0, 2.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 5e3
  )

# adding criterion
m1 <- add_criterion(m1.5H4, criterion = c("waic", "loo"))
m3 <- add_criterion(m3.5H4, criterion = c("waic", "loo"))
m4 <- add_criterion(m4.5H4, criterion = c("waic", "loo"))


# comparing the models 
loo_compare(m1, m3, m4, criterion = "waic")
loo_compare(m1, m3, m4, criterion = "loo")

 

```

The elpd_diff value for model m1 is -0.8. A negative elpd_diff indicates that model m1 has a slightly lower expected log pointwise predictive density compared to the reference model m4. However, the se_diff value of 1.8 indicates that the difference is not statistically significant, as the standard error includes zero. Therefore, we cannot conclude that model m1 is significantly worse or better than model m4 based on these results.

For model m3, the ELPD difference is -7.6. Similarly to m1, a negative ELPD difference suggests that model m3 has lower expected log predictive density compared to the reference model (m4). The SE difference is 5.7, which indicates a relatively higher uncertainty compared to m1. The large SE difference suggests a considerable level of uncertainty and indicates that the difference may not be statistically significant or practically meaningful.


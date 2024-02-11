---
title: "Assignment 1"
name: "Tolga Şabanoğlu"
date: "2023-04-26"
output: html_document

---

libraries 
```{r}

library(patchwork)
library(tidyverse)
library(ggplot2)

```

2M1. Recall the globe tossing model from the chapter. Compute and plot the grid approximate posterior distribution for each of the following sets of observations. In each case, assume a uniform prior for p.

(1) W,W,W
(2) W,W,W,L
(3) L,W,W,L,W,W,W

```{r}

# Define grid of parameter values
p_grid <- seq(0, 1, length.out = 1000)

# Define uniform prior
prior <- rep(1, 1000)

# Likelihood for each case
# Case 1: W, W, W
likelihood_1 <- dbinom(3, size =3, p_grid)

# Case 2: W, W, W, L
likelihood_2 <- dbinom(3, size= 4, p_grid)

# Case 3: L, W, W, L, W, W, W
likelihood_3 <- dbinom(5, size =7, p_grid)

# Compute posterior using grid approximation
posterior_1 <- likelihood_1 * prior
posterior_1 <- posterior_1 / sum(posterior_1)

posterior_2 <- likelihood_2 * prior
posterior_2 <- posterior_2 / sum(posterior_2)

posterior_3 <- likelihood_3 * prior
posterior_3 <- posterior_3 / sum(posterior_3)

# Plot posterior distributions
plot(p_grid, posterior_1, type = "l", xlab = "Proportion Water", ylab = "Posterior Probability", 
     main = "Posterior Distribution - Case 1")

plot(p_grid, posterior_2, type = "l", xlab = "Proportion Water", ylab = "Posterior Probability", 
     main = "Posterior Distribution - Case 2")

plot(p_grid, posterior_3, type = "l", xlab = "Proportion Water", ylab = "Posterior Probability", 
     main = "Posterior Distribution - Case 3")




```

2M2. Now assume a prior for p that is equal to zero when p < 0.5 and is a positive constant when p ≥ 0.5. Again compute and plot the grid approximate posterior distribution for each of the sets of observations in the problem just above.

```{r}

# Calculate prior probability of each value of p
prior <- ifelse(p_grid < 0.5, 0, 1)

#put the other condition 
likelihood <- dbinom(3, 3, p_grid)
posterior_1 <- likelihood * prior
posterior_1 <- posterior_1 / sum(posterior_1)
plot(p_grid, posterior_1, type="l", xlab="Probability of water - Case 1", ylab="Posterior probability")


likelihood <- dbinom(3, 4, p_grid)
posterior_2 <- likelihood * prior
posterior_2 <- posterior_2 / sum(posterior_2)
plot(p_grid, posterior_2, type="l", xlab="Probability of water Case 2", ylab="Posterior probability")

likelihood <- dbinom(5, 7, p_grid)
posterior_3 <- likelihood * prior
posterior_3 <- posterior_3 / sum(posterior_3)
plot(p_grid, posterior_3, type="l", xlab="Probability of water Case 3", ylab="Posterior probability")

```

2M3. Suppose there are two globes, one for Earth and one for Mars. The Earth globe is 70% covered in water. The Mars globe is 100% land. Further suppose that one of these globes—you don’t know which—was tossed in the air and produced a “land” observation. Assume that each globe was equally likely to be tossed. Show that the posterior probability that the globe was the Earth, conditional on seeing “land” (Pr(Earth|land)), is 0.23.


```{r}

# Prior probability
Pr_Earth <- 0.5

# Probability of observing "land" given that the globe is the Earth 
Pr_land_Earth <- 0.3

# Probability of observing "land" given that the globe is Mars 
Pr_land_Mars <- 1

# Calculate posterior probability using Bayes' theorem
Pr_Earth_land <- Pr_land_Earth * Pr_Earth / (Pr_land_Earth * Pr_Earth + Pr_land_Mars * (1 - Pr_Earth))
Pr_Earth_land


```


2M4. Suppose you have a deck with only three cards. Each card has two sides, and each side is either black or white. One card has two black sides. The second card has one black and one white side. The third card has two white sides. Now suppose all three cards are placed in a bag and shuffled. Someone reaches into the bag and pulls out a card and places it flat on a table. A black side is shown facing up, but you don’t know the color of the side facing down. Show that the probability that the other side is also black is 2/3. Use the counting method (Section 2 of the chapter) to approach this problem. This means counting up the ways that each card could produce the observed data (a black side facing up on the table).



```{r}

# Define the probabilities of drawing each card
p_A1 <- 1/3
p_A2 <- 1/3
p_A3 <- 1/3

# Define the probabilities of seeing a black side given each card
p_B_given_A1 <- 1
p_B_given_A2 <- 1/2
p_B_given_A3 <- 0

# Compute the probability of seeing a black side
p_B <- p_B_given_A1 * p_A1 + p_B_given_A2 * p_A2 + p_B_given_A3 * p_A3

# Compute the probability that the card with two black sides was drawn
p_A1_given_B <- p_B_given_A1 * p_A1 / p_B

p_A1_given_B

```


2M5. Now suppose there are four cards: B/B, B/W, W/W, and another B/B. Again suppose a card is drawn from the bag and a black side appears face up. Again calculate the probability that the other side is black.



```{r}

# Prior probability of other side being black
p_b <- 2/3 

# Probability of a black side appearing face up
p_a <- 1/4 + 1/2 

# Probability of a black side appearing face up given that the other side is black
p_a_b <- 1 

# Applying Bayes' theorem
p_b_a <- p_a_b * p_b / p_a 

# Probability that the other side is black given a black side appears face up
p_b_a


```


2M6. Imagine that black ink is heavy,and so cards with black sides are heavier than cards with white sides. As a result, it’s less likely that a card with black sides is pulled from the bag. So again assume there are three cards: B/B, B/W, and W/W. After experimenting a number of times, you conclude that for every way to pull the B/B card from the bag, there are 2 ways to pull the B/W card and 3 ways to pull the W/W card. Again suppose that a card is pulled and a black side appears face up. Show that the probability the other side is black is now 0.5. Use the counting method, as before.




```{r}

# Prior probabilities of drawing each card
p1 <- 1/6 # B/B
p2 <- 1/3 # B/W
p3 <- 1/2 # W/W

# Total probability of getting a black side
p_a <- p1 * 1 + p2 * 1/2 + p3 * 0

# Probability of drawing the B/B card and getting a black side
p_b1_a <- p1 * 1 / p_a

# Probability of drawing the B/W card and getting a black side
p_b2_a <- p2 * 1/2 / p_a

# Probability that the other side is black given a black side appears face up
p_b_a <- p_b1_a / (p_b1_a + p_b2_a)

# Probability that the other side is black given a black side appears face up
p_b_a


```


2M7. Assume again the original card problem, with a single card showing a black side faceup.Before looking at the other side, we draw another card from the bag and lay it face up on the table. The face that is shown on the new card is white. Show that the probability that the first card, the one showing a black side, has black on its other side is now 0.75. Use the counting method, if you can. Hint: Treat this like the sequence of globe tosses, counting all the ways to see each observation, for each possible first card.


```{r}


#


```



2H1. Suppose there are two species of panda bear. Both are equally common in the wild and live in the same places. They look exactly alike and eat the same food, and there is yet no genetic assay capable of telling them apart. They differ however in their family sizes. Species A gives birth to twins 10% of the time, otherwise birthing a single infant. Species B births twins 20% of the time, otherwise birthing singleton infants. Assume these numbers are known with certainty, from many years of field research.
Now suppose you are managing a captive panda breeding program. You have a new female panda of unknown species, and she has just given birth to twins. What is the probability that her next birth will also be twins?


```{r}

# Probability of giving birth to twins for species A and B
P_T_A <- 0.1
P_T_B <- 0.2

# Probability of giving birth to a single offspring for species A and B
P_S_A <- 0.9
P_S_B <- 0.8

# Probability of being species A or B (assumed to be equal)
P_A <- 0.5
P_B <- 0.5

# Probability of giving birth to twins (overall)
P_T <- P_A * P_T_A + P_B * P_T_B

# Probability of giving birth to twins again, regardless of previous birth
P_T_T <- P_A * P_T_A^2 + P_B * P_T_B^2

# Probability of giving birth to twins again, given that she has just given birth to twins
P_T_given_T <- P_T_T / P_T

P_T_given_T


```


2H2. Recall all the facts from the problem above. Now compute the probability that the panda we have is from species A, assuming we have observed only the first birth and that it was twins.

```{r}

# Prior probability of species A and B
prior_A <- 0.5
prior_B <- 0.5

# Probability of twins given species A and B
prob_T_A <- 0.1
prob_T_B <- 0.2

# Probability of observing twins
prob_T <- prob_T_A * prior_A + prob_T_B * prior_B

# Probability of species A given twins
prob_A_T <- prob_T_A * prior_A / prob_T

prob_A_T  # output the result


```


2H3. Continuing on from the previous problem,suppose the same panda mother has a second birth and that it is not twins, but a singleton infant. Compute the posterior probability that this panda is species A.


```{r}
# Prior probabilities
prior_A <- 0.5
prior_B <- 0.5

# Likelihoods
likelihood_TT_S_A <- 0.1 * 0.9
likelihood_TT_S_B <- 0.2 * 0.8

# Evidence
evidence <- likelihood_TT_S_A * prior_A + likelihood_TT_S_B * prior_B

# Posterior probability of species A
posterior_A <- likelihood_TT_S_A * prior_A / evidence

posterior_A


```

2H4. A common boast of Bayesian statisticians is that Bayesian inference makes it easy to use all of the data, even if the data are of different types.
So suppose now that a veterinarian comes along who has a new genetic test that she claims can identify the species of our mother panda. But the test, like all tests, is imperfect. This is the information you have about the test:

The probability it correctly identifies a species A panda is: 0.8.  
The probability it correctly identifies a species B panda is: 0.65.
The vet administers the test to your panda and tells you that the test is positive for species A. First ignore your previous information from the births and compute the posterior probability that your panda is species A. Then redo your calculation, now using the birth data as well.

```{r}

# Prior probabilities
prior_A <- 0.5
prior_B <- 0.5

# Probabilities of test results given the true species
P_D_given_A <- 0.8
P_D_given_B <- 0.35

# Probability of positive test result
P_D <- P_D_given_A * prior_A + P_D_given_B * prior_B

# Posterior probability of species A without birth data
P_A_given_D_no_birth <- P_D_given_A * prior_A / P_D

# Prior probability of species A with birth data
prior_A_with_birth <- 0.3

# Posterior probability of species A with birth data
P_A_given_D_with_birth <- P_D_given_A * prior_A_with_birth / P_D

# Print results
P_A_given_D_no_birth
P_A_given_D_with_birth

```

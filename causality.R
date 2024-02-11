# Humboldt Universität zu Berlin
# Applied Statistical Methods - Summer Term 2023
### Topic: causality, foxes data
#### Tolga Şabanoğlu 

__6H3. Use a model to infer the total causal influence of area on weight. Would increasing the area available to each fox make it heavier (healthier)? You might want to standardize the variables. Regardless, use prior predictive simulation to show that your model’s prior predictions stay within the possible outcome range__

```{r, warning=FALSE, message=FALSE, error=FALSE}

# Loading required libraries 
library(brms)
library(MASS)
library(rethinking)
library(tidyverse)
library(dplyr)
library(loo)
library(dagitty)
library(ggplot2)
library(ggdag)
library(kableExtra)
library(broom.mixed)
library(knitr)
library(ggraph)
library(igraph)
library(tidybayes)


```

![Figure 1: DAG analysis of fox data](/Users/tolgasabanoglu/Desktop/foxes.png)


```{r, warning=FALSE, message=FALSE, error=FALSE}

# Load the data and standardize variables (except for "group")
data("foxes")

dat_foxes <- foxes %>%
  as_tibble() %>%
  mutate(across(-group, scale))

# Define the Bayesian model using brm()
m_foxes1 <- brm(
  weight ~ 1 + area,  
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),
    prior(normal(0, 0.5), class = "b", coef = "area"),  
    prior(exponential(1), class = "sigma")
  ),
  chains = 4, cores = 4, seed = 1234
)

# Using prior predictive simulation 
n <- 1000
tibble(group = seq_len(n),
       alpha = rnorm(n, 0, 0.2),
       beta = rnorm(n, 0, 0.5)) %>%
  expand(nesting(group, alpha, beta),
         area = seq(from = -2, to = 2, length.out = 100)) %>%
  mutate(weight = alpha + beta * area) %>%
  ggplot(aes(x = area, y = weight, group = group)) +
  geom_line(alpha = 1 / 10) +
  geom_hline(yintercept = c((0 - mean(foxes$weight)) / sd(foxes$weight),
                            (max(foxes$weight) - mean(foxes$weight)) /
                              sd(foxes$weight)),
             linetype = c("dashed", "solid"), color = "red") +
  annotate(geom = "text", x = -2, y = -3.83, hjust = 0, vjust = 1,
           label = "No weight") +
  annotate(geom = "text", x = -2, y = 2.55, hjust = 0, vjust = 0,
           label = "Maximum weight") +
  expand_limits(y = c(-4, 4)) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x = "Standardized Area", y = "Standardized Weight")
       

```

Figure 2: Prior predictive simulation for area on weight

```{r, warning=FALSE, message=FALSE, error=FALSE}

# Checking effect of area on weight 
summary(m_foxes1)

```

Based on Figure 1 provided by McElreath (2016), there are two main paths:

(1) area -> avgfood -> weight,
(2) area -> avgfood -> groupsize -> weight

To scale variables, the dataset was standardized. Prior predictive simulation was used to assess the appropriateness of chosen prior distributions and ensure that prior assumptions align with the data and scientific principles. This feature is routinely used to check models and prior influence after fitting the data using the posterior predictive distribution (Lemoine, 2019; Gelman et al., 2020), but it can also be used before seeing the data using the prior predictive distribution (Gabry et al., 2019, as cited in Wesner & Pomeranz, 2021). When inspecting posterior predictive distribution, Figure 2 shows that there are some outliers beyond the values of 2 and -2.

The estimated coefficient for "area" is 0.02 with a credible interval including both positive and negative values (-0.16 to 0.20). This suggests that there is not a substantial causal influence of "area" on "weight." Increasing the area available to each fox is not strongly associated with making them heavier or healthier. The coefficient is close to zero, and the credible interval includes zero, indicating that changes in "area" do not have a statistically significant effect on "weight" based on this model.

Additionally, the standard deviation (sigma) of approximately 1.01 represents the variability in "weight" not explained by the model, suggesting other unaccounted factors influencing fox weight.

In summary, based on this Bayesian regression model, there is no strong evidence to suggest that increasing the area available to each fox would significantly impact their weight or health. The model's prior predictions also stay within a realistic range, suggesting reasonable model assumptions.


__6H4. Now infer the causal impact of adding food to a territory. Would this make foxes heavier? Which covariates do you need to adjust for to estimate the total causal influence of food?__


```{r, warning=FALSE, message=FALSE, error=FALSE}

# Define the Bayesian regression model using brm()
m_foxes2 <- brm(
  weight ~ 1 + avgfood,  
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),
    prior(normal(0, 0.5), class = "b", coef = "avgfood"),  
    prior(exponential(1), class = "sigma")
  ),
  chains = 4, cores = 4, seed = 1234
)

# Print the test results
summary(m_foxes2)

```

Paths:

(1) avgfood -> groupsize -> weight
(2) avgfood -> weight

When examining the model parameters, prior specifications include setting a normal prior distribution with a mean of 0 and a standard deviation of 0.2 for the intercept (constant term) in the regression. Another prior sets a normal distribution with a mean of 0 and a standard deviation of 0.5 for the coefficient associated with 'avgfood.' This prior defines the expected effect of 'avgfood' on 'weight.' The line 'chains = 4' indicates that four Markov chains will run in parallel. 'Cores = 4' specifies the number of CPU cores allocated for parallel processing. The 'seed = 1234' setting initializes a seed for ensuring reproducibility. The 'sigma' class represents the estimate of the residual standard deviation, which quantifies the unexplained variability in the data.

Based on both the DAG analysis and the model results, we can deduce the causal impact of introducing additional food ('avgfood') to a territory on fox weight. In summary, the model outcomes suggest a slight, statistically non-significant negative association between 'avgfood' and 'weight,' with a relatively weak effect. The model's predictions are centered around 0 for the intercept, and the data points exhibit a standard deviation of approximately 1.01, indicating moderate variability.


__6H5. Now infer the causal impact of group size. Which covariates do you need to adjust for? Looking at the posterior distribution of the resulting model, what do you think explains these data? That is, can you explain the estimates for all three problems? How do they go together?__


```{r, warning=FALSE, message=FALSE, error=FALSE}

# Define the Bayesian regression model using brm()
m_foxes3 <- brm(
  weight ~ 1 + avgfood + groupsize,  # Include both avgfood and groupsize effects
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),
    prior(normal(0, 0.5), class = "b", coef = "avgfood"),  
    prior(normal(0, 0.5), class = "b", coef = "groupsize"),  
    prior(exponential(1), class = "sigma")
  ),
  chains = 4, cores = 4, seed = 1234
)

# Print the test results
summary(m_foxes3)

```

Two paths:

(1) groupsize -> weight
(2) groupsize -> avgfood -> weight

I applied Bayesian regression modeling to investigate the relationship between fox weight and two predictor variables: average food availability (avgfood) and group size (groupsize). The model aimed to uncover how these factors influence the weight of foxes in a given territory.

The coefficient estimate for groupsize is approximately -0.58, indicating that an increase in group size is associated with a decrease in fox weight. Larger group sizes tend to have lighter foxes. These models help explain factors influencing fox weight. Models collectively show different factors (area, avgfood, groupsize) and their varying effects on weight, even after adjusting for potential confounding. Among them, Model m_foxes3 (group size) has the most significant and negative impact on fox weight, suggesting that group size is a substantial influence, even after considering food availability (avgfood). Model m_foxes1 (Area) suggests a weaker positive association between area and fox weight, indicating that increased area may contribute to slightly heavier foxes. Model m_foxes2 (avgfood) suggests a weak negative association between food availability and fox weight, implying that more available food might lead to slightly lighter foxes, though this effect is relatively minor.

As a result, these findings offer valuable insights into the ecological factors that influence fox weight, providing guidance for wildlife management and conservation efforts.


__7H5. Revisit the urban fox data, data(foxes), from the previous chapter’s practice problems. Use WAIC or PSIS based model comparison on five different models, each using weight as the outcome, and containing these sets of predictor variables:__

(1) avgfood + groupsize + area 
(2) avgfood + groupsize
(3) groupsize + area
(4) avgfood
(5) area


__Can you explain the relative differences in WAIC scores, using the fox DAG from the previous chapter? Be sure to pay attention to the standard error of the score differences (dSE)__


```{r, warning=FALSE, message=FALSE, error=FALSE}

# Recall the standardized foxes data
str(dat_foxes)

# Fit a model for sets of predictor variables, respectively:

# Model 1
m1 <- brm(
  weight ~ 1 + avgfood + groupsize + area,  # Include all predictors
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),  
    prior(exponential(1), class = "sigma"),
    prior(normal(0, 0.5), class = "b", coef = "avgfood"),  # Prior for avgf
    prior(normal(0, 0.5), class = "b", coef = "groupsize"),  # Prior for grps
    prior(normal(0, 0.5), class = "b", coef = "area")  # Prior for area
  ),
  chains = 4, cores = 4, seed = 1234
)

# Model 2
m2 <- brm(
  weight ~ 1 + avgfood + groupsize,  # Include avgfood and groupsize
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),  
    prior(exponential(1), class = "sigma"),
    prior(normal(0, 0.5), class = "b", coef = "avgfood"),  # Prior for avgfood
    prior(normal(0, 0.5), class = "b", coef = "groupsize")  # Prior for groupsize
  ),
  chains = 4, cores = 4, seed = 1234
)

# Model 3
m3 <- brm(
  weight ~ 1 + groupsize + area,  # Include groupsize and area
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),  
    prior(exponential(1), class = "sigma"),
    prior(normal(0, 0.5), class = "b", coef = "groupsize"),  # Prior for groupsize
    prior(normal(0, 0.5), class = "b", coef = "area")  # Prior for area
  ),
  chains = 4, cores = 4, seed = 1234
)

# Model 4
m4 <- brm(
  weight ~ 1 + avgfood,  # Include only avgfood
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),  
    prior(exponential(1), class = "sigma"),
    prior(normal(0, 0.5), class = "b", coef = "avgfood")  # Separate prior for avgfood
  ),
  chains = 4, cores = 4, seed = 1234
)

# Model 5
m5 <- brm(
  weight ~ 1 + area,  # Include only area
  data = dat_foxes,
  prior = c(
    prior(normal(0, 0.2), class = "Intercept"),  
    prior(exponential(1), class = "sigma"),
    prior(normal(0, 0.5), class = "b", coef = "area")  # Separate prior for area
  ),
  chains = 4, cores = 4, seed = 1234
)

# Compute WAIC for each model
waic_m1 <- loo(m1)
waic_m2 <- loo(m2)
waic_m3 <- loo(m3)
waic_m4 <- loo(m4)
waic_m5 <- loo(m5)

# Compare models using WAIC
model_comparison <- loo::compare(waic_m1, waic_m2, waic_m3, waic_m4, waic_m5)

# Print the model comparison results
print(model_comparison)


```

Leave-one-out cross-validation (LOO) and the widely applicable information criterion (WAIC) were used for model comparison. Lower WAIC scores indicate better model fit and predictive performance. The models were compared using their WAIC scores, yielding the following results:

- Model 1 (avgfood + groupsize + area) had the lowest WAIC score (322.7), indicating the best overall fit among the five models.

- Model 2 (avgfood + groupsize) and Model 3 (groupsize + area) had slightly higher WAIC scores, suggesting slightly worse fit compared to Model 1.

- Model 4 (avgfood) and Model 5 (area) had the highest WAIC scores (333.2 and 333.5, respectively), indicating less favorable predictive performance.

When comparing Model 1, Model 2, and Model 3, including all three predictors (Model 1) provided a better fit than including only avgfood and groupsize (Model 2) or groupsize and area (Model 3).

Model comparison based on WAIC scores confirmed that Model 1 (avgfood + groupsize + area) is the most suitable model for explaining fox weight, outperforming the other models. This aligns with the individual model results and emphasizes the importance of including all three predictors for the best fit and predictive performance.

Considering both WAIC differences and standard error of the score differences (se_diff), Models 1, 2, and 3 appear to be the most favorable choices, with Model 1 having a slight advantage in predictive accuracy. 

When looking at standard error of the score differences comparison:

- Model 1 (waic_m1) serves as the reference with a se_diff of 0.0.
- Model 2 (waic_m2) has a se_diff of 1.7, indicating that its performance is within the uncertainty range of Model 1 but slightly worse.
- Model 3 (waic_m3) has a se_diff of 1.4, similar to Model 2, indicating similar performance but still slightly worse than Model 1.
- Model 4 (waic_m4) has a se_diff of 3.4, indicating a more significant difference and lower performance compared to Model 1.
- Model 5 (waic_m5) also has a se_diff of 3.4, which is similar to Model 4 but worse than Models 1, 2, and 3.

In summary, based on se_diff, Models 2 and 3 have similar performance, slightly worse than Model 1 but better than Models 4 and 5. Based on waic, Model 1 is the best-performing model, followed by Models 2 and 3, which have similar but slightly worse fit. Models 4 and 5 are significantly worse in terms of model fit.
Model 1 (waic_m1) appears to be the best choice among the five models, as it has the lowest waic and small se_diff compared to the other models. Models 2 and 3 are also reasonable alternatives with slightly worse but comparable performance. Models 4 and 5 are less suitable due to their higher waic values and larger se_diff.

__References__

[Efficient Leave-One-Out Cross-Validation and WAIC for Bayesian Models -R package loo version 2.6.0-](https://cran.r-project.org/web/packages/loo/index.html) (2023). Retrieved from https://cran.r-project.org/web/packages/loo/index.html

[Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B.](https://books.google.de/books?hl=en&lr=&id=ZXL6AQAAQBAJ&oi=fnd&pg=PP1&dq=Bayesian+data+analysis&ots=uQYkx-9H-Z&sig=tUVTMtENpyKzGdyAg4iYe-LBfAc&redir_esc=y#v=onepage&q=Bayesian%20data%20analysis&f=false) (2020). Bayesian data analysis (3rd ed.)

[McElreath, R.](https://www.taylorfrancis.com/books/mono/10.1201/9781315372495/statistical-rethinking-richard-mcelreath) (2016). Statistical Rethinking: A Bayesian Course with Examples in R and Stan

[Vehtari, A., Gabry, J., Yao, Y., Di Cook, D., & Williams, D.](https://avehtari.github.io/rundoc/BCIntro.pdf) (2023). Introduction to Bayesian computation in R. Retrieved from https://avehtari.github.io/rundoc/BCIntro.pdf

[Wesner, J. S., & Pomeranz, J. P.](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.3739) (2021). Choosing priors in Bayesian ecological models by simulating from the prior predictive distribution. Ecosphere, 12(9), e03739


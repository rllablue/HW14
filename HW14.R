#####################
### ZOO800: HW 14 ###
#####################

# Author: Rebekkah LaBlue
# Concept: Model Selection
# Due: December 8, 2025



### --- PACKAGES --- ###

library(tidyverse)
library(dplyr)
library(purrr)
library(tibble)
library(magrittr)
library(car)
library(units)
library(stats)
library(AICcmodavg)
library(ggplot2)
library(viridis)
library(ggfortify)
library(lmtest)
library(here)


### --- PROBLEM --- ###

### In Week 11, we used a model selection process (backward selection) based on p-values described in
# Faraway pp 270 – 272. This week, we are going to expand on this to evaluate models using likelihood.
# This is a more wholistic approach to model selection that asks whether additional parameters are
# justified by improvements to their predictions. We’ll try a simple approach – the likelihood ratio test –
# first. This method only works for nested models where a candidate model can be compared to a simpler
# model that is a special case of the candidate model – i.e., the models are “nested.” We’ll then use AIC to
# compare multiple models at once.
# In both cases, you should use the simulated data set that you analyzed in Week 11 (you would have received this from a partner).


###################
### OBJECTIVE 1 ###
###################

# A # 

seed_data <- read.csv("SeedData.csv")
View(seed_data)

# B # 

seed_mod_full <- lm(seed.delivery ~ distance.to.seed.source.m * species, data = seed_data) # full mod, interactions
seed_mod_red <- lm(seed.delivery ~ distance.to.seed.source.m + species, data = seed_data) # reduced mod, no interactions

summary(seed_mod_full)
summary(seed_mod_red)

autoplot(seed_mod_full, which = 1:6)
autoplot(seed_mod_red, which = 1:6)
# Data not quite normal


# C # 

nll_full <- -as.numeric(logLik(seed_mod_full)) # (-) to make NLL, transform for LRT, model selection; as.numeric to strip characters, df from logLik value in output
nll_red <- -as.numeric(logLik(seed_mod_red))

nll_full
nll_red
### Full model has lower NLL than the reduced model.


# D # 

lrtest(seed_mod_red, seed_mod_full)
### The LRT suggests that the NLL of the full model is significantly lower than
# that of the reduced model (p < 0.0001), justifying the inclusion of additional 
# parameters (and degrees of freedom). Ecologically, this suggests that seed delivery 
# differs by combinations of distance to seed source (m) and tree species--declines
# or increases in seed delivery depend both on tree species and distance to tree seed source.

### These conclusions agree with those from backwards selection methods, which found the 
# interaction term to be significant (thus imparting the same ecological relevance) 
# and therefore did not reduce the model to fewer terms.



###################
### OBJECTIVE 2 ###
###################

# A #

# Construct models
mod_full <- lm(seed.delivery ~ distance.to.seed.source.m * species, data = seed_data)
mod_add <- lm(seed.delivery ~ distance.to.seed.source.m + species, data = seed_data)
mod_dist <- lm(seed.delivery ~ distance.to.seed.source.m, data = seed_data)
mod_spp <- lm(seed.delivery ~ species, data = seed_data)
mod_null <- lm(seed.delivery ~ 1, data = seed_data)

# Build list with models to allow/automate AICc calc
mod_candidates <- list(
  "Full (~ distance * species)" = mod_full,
  "Main effects (~ distance + species)" = mod_add,
  "Distance (~ distance)" = mod_dist,
  "Species (~ species)" = mod_spp,
  "Intercept (~ 1)" = mod_null
)

# Build df with AICc results
aic_table <- aictab(cand.set = mod_candidates, second.ord = TRUE) # second.ord = AICc ('second order Akaike Information Criterion')

View(aic_table)

### Lowest AICc = 'best' model
### Delta AICc < 2 = plausible model w/ substantial support
### AICc weight = probability model is best among candidate set; can convert to relative likelihood for ease of interp., 
#ie. (top mod weight / comparison model weight) = comp. model is 'roughly x times less likely' than top model 


# B # 

### Once again, the full model has the highest highest support; the interaction between species 
# and distance to seed source is therefore instrumental in understanding and evaluating the
# efficacy of tree seed delivery.


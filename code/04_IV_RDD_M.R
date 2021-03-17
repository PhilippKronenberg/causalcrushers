###############################################################################################################################################
###############################################################################################################################################
## Load required packages
library(tidyverse)
library(AER)
###############################################################################################################################################
###############################################################################################################################################

## Load Data
load("~/Dropbox/PhD/Courses/Identification_Causal_Inference/Assignment/causalcrushers/data/data.rda")
df <- data_table

## 4. Focusing on the self-selection case, assess whether the following methods overcome the selection bias. 
##    In each case, provide a short discussion of why the method works or why it does not work:

## ...Thus, the estimate of β suffers from selection bias if there is a difference in the 
## no-treatment potential outcomes between the treated and the untreated.

## Part a - Instrumental Variables
#Define variable 
#(Y1 = Dependent Variable, Y2 = endogenous variable, X1 = exogenous variable, X2 = Instrument)
## We want a variable that is correlated with an endogenous explanatory variable and uncorrelated with the error term. 
attach(df)
Y1 <- harv
Y2 <- treat
X1 <- cbind(area, sun, rain, exp, dist, child)
X2 <- vouch

# Ordinary Least Squares 
ols_reg <- lm(Y1 ~ Y2 + X1)
summary(ols_reg)

# Two-stage Least Squares
iv_reg <- ivreg(Y1 ~ Y2 + X1 | X1 + X2)
summary(iv_reg)



## Part b - Regression Discontinuity Design
## https://www.rdocumentation.org/packages/rddtools/versions/1.4.0
## https://www.econometrics-with-r.org/13-4-quasi-experiments.html
## https://github.com/bquast/rddtools-article

ggplot(aes(area, harv, data = df)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 50, colour = "grey", linetype = 2) +
  stat_smooth(method = "lm", se = F) +
  labs(x = "Test score (X)", y = "Potential Outcome (Y)")

## Part c - Matching approach (e.g., exact covariate matching or nearest neighbor matching based on the 
##          propensity score or the post-double-selection LASSO method2 

## Nearest neigbor/propensity-score matching: https://stats.stackexchange.com/questions/386399/11-nearest-neighbor-propensity-score-matching-in-r-matchit-package
##    https://www.r-bloggers.com/2016/06/how-to-use-r-for-matching-samples-propensity-score/
##    https://www.rdocumentation.org/packages/MatchIt/versions/4.1.0/topics/matchit
##    https://cran.r-project.org/web/packages/Matching/Matching.pdf (downloaded)
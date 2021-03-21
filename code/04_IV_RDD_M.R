###############################################################################################################################################
###############################################################################################################################################
## Load required packages
library(tidyverse)
library(AER)
library(rddtools)
library(MatchIt)
###############################################################################################################################################
###############################################################################################################################################

## Load Data
load("~/Dropbox/PhD/Courses/Identification_Causal_Inference/Assignment/causalcrushers/data/data.rda")
df <- data_table

## 4. Focusing on the self-selection case, assess whether the following methods overcome the selection bias. 
##    In each case, provide a short discussion of why the method works or why it does not work:

## ...Thus, the estimate of β suffers from selection bias if there is a difference in the 
## no-treatment potential outcomes between the treated and the untreated.

###############################################################################################################################################
###############################################################################################################################################

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

###############################################################################################################################################
###############################################################################################################################################

## Part b - Regression Discontinuity Design
## https://www.rdocumentation.org/packages/rddtools/versions/1.4.0
## https://www.econometrics-with-r.org/13-4-quasi-experiments.html
## https://github.com/bquast/rddtools-article
## https://rdpackages.github.io/ 

# Redefine treatment variable to restrict farmer access based on area. Only farmers with area <= 1000m2 can participate.
df$treat <- ifelse(area<=1000,1,0)

#Save old harvest numbers
old.harv <- df$harv

# Redefine harvest based on new treatment restriction
df <- df %>%
  mutate(new.harv = 400 + 0.05*area + 0.04*sun + 0.03*rain + 5*exp - 5*child + 10*educ + 100*treat + rnorm(1, mean=0, sd=1), .before = area)

#Save new harvest numbers
new.harv <- df$harv

#Compare old random harvest values to new ones with farm size restriction
par(mfrow=c(1,2))
plot(area,old.harv)
plot(area,new.harv)

## Make RDD data object;  RDD cutpoint is at 1000 sq. m. for farm area
attach(df)
rd.dat <- rdd_data(x=area, y=new.harv, cutpoint=1000)

## Plot
rd.dat$color <- cut(rd.dat$x, breaks = c(0,1000,+Inf))
plot(rd.dat$x,rd.dat$y,
     col=rd.dat$color)

## Estimate sharp RDD model 
rdd_mod <- rdd_reg_lm(rdd_object=rd.dat,
                      slope="same")
summary(rdd_mod)
plot(rdd_mod,
     cex=0.35,
     col=treat,
     xlab="Area",
     ylab="Harvest")

###############################################################################################################################################
###############################################################################################################################################
## Part c - Matching approach (e.g., exact covariate matching or nearest neighbor matching based on the 
##          propensity score or the post-double-selection LASSO method2 

## Nearest neighbor/propensity-score matching: https://stats.stackexchange.com/questions/386399/11-nearest-neighbor-propensity-score-matching-in-r-matchit-package
##    https://www.r-bloggers.com/2016/06/how-to-use-r-for-matching-samples-propensity-score/
##    https://www.rdocumentation.org/packages/MatchIt/versions/4.1.0/topics/matchit
##    https://cran.r-project.org/web/packages/Matching/Matching.pdf (downloaded)

## Perform matching
m.out <- matchit(treat ~ area + sun + rain + exp + dist + child + educ, data = data_table, method="nearest", ratio=1)
summary(m.out)

## Plot
#plot(m.out, type="jitter")
plot(m.out, type="hist") # plots very similar between raw and matched; probably because data randomly distributed in data generation

## Save matched data as new dataframe
m.dat <- match.data(m.out)

## Run model with new data
m.mod <- lm(new.harv ~  area + sun + rain + exp + dist + child + educ) 
summary(m.mod)

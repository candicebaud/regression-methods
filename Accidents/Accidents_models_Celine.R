# In this file we proceed to all computations related to explorations on the 
# Accidents data set.

# Libraries and files -----------------------------------------------------
library(ggplot2)
library("lme4")
library(MASS)
library(readr)
library(GGally)
library(DHARMa)
source(file="Accidents/Accidents_functions.R")
# Loading data set --------------------------------------------------------
load("Accidents/Accidents.RData")
# Models ------------------------------------------------------------------
model <- glm(Acc ~ log(Traffic) + SlopeType + log(Length) + Limit + Type +
               Width + Direction + Slope + HGV + Year , family=poisson,
             data=Accidents)
summary(model)
# Residual deviance: 3556.3  on 1146  degrees of freedom --> Maybe a problem of
# over dispersion
# AIC: 6057.6


plot(density(resid(model, type='pearson')))
a= sum(resid(model, type='pearson'))
1-pchisq(a, model$df.residual)

model$df.residual
pchisq(28.6, 1146)
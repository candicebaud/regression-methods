# In this file we proceed to all computations related to explorations on the 
# Accidents data set.

# Libraries and files -----------------------------------------------------
library(ggplot2)
library("lme4")
library(MASS)
library(readr)
library(GGally)
library(DHARMa)
library('glmtoolbox')
source(file="Accidents/Accidents_functions.R")
# Loading data set --------------------------------------------------------
load("Accidents/Accidents.RData")
# Models ------------------------------------------------------------------
# Model 1 #####
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

# Model 2 ######
fullmodel <- glm((Acc+1) ~ Lanes + Direction + log(Traffic) + HGV + Slope +
                   Urban + Type + log(Length) + Limit + SlopeType + Width + Lanes,
                 family=poisson, data=Accidents)
model2 <- stepCriterion(fullmodel, criterion = 'aic',
                       direction = c('forward', 'backward'))
summary(model2)
# Residual deviance: 3566  on 1147  degrees of freedom
# AIC: 6065.3


# Model 3 #####
model3 <- glmer((Acc+1) ~ log(Traffic) + HGV + Urban + Type + log(Length) + 
                Limit + SlopeType + Width + (1 | Tunnel ), family=poisson ,
              data = Accidents)
summary(model3)

# Residual deviance
# AIC : 5468.9

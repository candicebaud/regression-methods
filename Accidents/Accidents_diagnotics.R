#In this file, we make diagnostics on the models we designed for Accidents
# Libraries and files -----------------------------------------------------
library(ggplot2)
library('lme4')
library(MASS)
library(readr)
library(GGally)
library(boot)
library(glmmTMB)
library(performance)
source(file = "Accidents/Accidents_functions.R")
source(file = "explo_diag_functions.R")
# Loading data set --------------------------------------------------------
load("Accidents/Accidents.RData")

# BEST MODELS -------------------------------------------------------------
# Model1 ------------------------------------------------------------------
model <- glm(Acc ~ log(Traffic) + SlopeType + log(Length) + Limit + Type +
               Width + Direction + Slope + HGV + Year , family=poisson,
             data=Accidents)
# Test of dispersion
check_overdispersion(model)

# Test of outliers
check_outliers(model)

# Diagnostics
png(file="Accidents/plots/accidents_diagnosticsglm.png", width=600, height=600)
diagnosticsglm(model)
dev.off()
# Some other diagnostics, not to add in the final report.
# Checking linearity
checklinearity(model$fitted.values,model$residuals)

# Checking the variance
checkvariance(model$fitted.values,model$residuals)

# QQ-plot
qqplot(model)

# Model glmr --------------------------------------------------------------
modelglmr <- glmer(Acc~ log(Traffic) + log(Length) + Slope + Type + Width + 
                     (1 | Tunnel) + (1 | Company), 
                   family=poisson, data = Accidents)
# Test of dispersion --> there is over dispersion
check_overdispersion(modelglmr)

# Test of outliers --> no outliers detected
check_outliers(modelglmr)

# Diagnostics --> bad qq-plot and residual vs fitted
png(file="Accidents/plots/accidents_diagnosticsglmr.png", width=600, height=600)
diagnosticsglmer(modelglmr)
dev.off()

# Some other diagnostics, not to add in the final report.
# # Checking linearity
# checklinearity(modelglmr$fitted.values,modelglmr$residuals)
# 
# # Checking the variance
# checkvariance(modelglmr$fitted.values,modelglmr$residuals)

# QQ-plot
qqplot(modelglmr)

# OLD MODELS --------------------------------------------------------------
# # Model 2 -----------------------------------------------------------------
# model2 <- glm(Acc~ log(Traffic) + SlopeType + log(Length) + Limit + Type +
#                 Width + Direction + Slope + HGV, data = Accidents, family = poisson)
# summary(model2)
# # Test of dispersion --> there is over dispersion
# check_overdispersion(model2)
# 
# # Test of outliers --> no outliers detected
# check_outliers(model2)
# 
# # Diagnostics --> bad qq-plot and residual vs fitted
# png(file="Accidents/plots/accidents_diagnostics2.png", width=600, height=600)
# diagnosticsglm(model2)
# dev.off()
# 
# # Some other diagnostics, not to add in the final report.
# # Checking linearity
# checklinearity(model2$fitted.values,model2$residuals)
# 
# # Checking the variance
# checkvariance(model2$fitted.values,model2$residuals)
# 
# # QQ-plot
# qqplot(model2)
# 
# # Model 3 -----------------------------------------------------------------
# model3 <- glmer((Acc+1) ~ log(Traffic) + HGV + Urban + Type + log(Length) + 
#                   Limit + SlopeType + Width + (1 | Tunnel ), family=poisson ,
#                 data = Accidents)
# summary(model3)
# # Test of dispersion --> there is over dispersion
# check_overdispersion(model3)
# 
# # Test of outliers --> no outliers detected
# check_outliers(model3)
# 
# # Diagnostics --> bad qq-plot and residual vs fitted
# png(file="Accidents/plots/accidents_diagnostics3.png", width=600, height=600)
# diagnosticsglmer(model3)
# dev.off()
# # Some other diagnostics, not to add in the final report.
# # Checking linearity
# checklinearity(model3$fitted.values,model3$residuals)
# 
# # Checking the variance
# checkvariance(model3$fitted.values,model3$residuals)
# 
# # QQ-plot
# qqplot(model2)
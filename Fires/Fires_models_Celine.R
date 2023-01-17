# In this file is my own exploration of Fire and some tests. Codes are originally 
# from Candice

# Libraries and files -----------------------------------------------------
library(ggplot2)
library('lme4')
library(MASS)
library(readr)
library(GGally)
library(boot)
library(glmmTMB)
library(performance)
source(file="Fires/Fires_functions.R")
# Loading data ------------------------------------------------------------
load("Fires/Fires.RData")
# Models ------------------------------------------------------------------
df <- mFires()
dff <- mFires2()
# Poisson 1 ----
# Initial ------
m0 <- glm(Fires ~ log(Traffic) + HGV + log(Length), family="poisson", data=Fires)
summary(m0)
# Residual deviance: 191.59  on 166  degrees of freedom --> over dispersion !
# AIC: 368.67

# Test of dispersion
check_overdispersion(m0)
# Over dispersion test

# dispersion ratio =   1.222
# Pearson's Chi-Squared = 202.871
#                 p-value =   0.027
# 
# Over dispersion detected. --> and this is an issue !

simResids <- simulateResiduals(m0)
plot(simResids)
glm.diag.plots(m0)

# Test of outliers
check_outliers(m0)
# 1 outlier detected: case 149.
# - Based on the following method and threshold: cook (0.84).
# - For variable: (Whole model).

# # Change in scale "Fires"
# m00 <- glm(Fires ~ Traffic + HGV + Length, family="poisson", data=df)
# summary(m00)
# # Residual deviance: 257.70  on 166  degrees of freedom --> over dispersion !
# # AIC: 434.77
# simResids <- simulateResiduals(m00)
# plot(simResids)
# # This model is very poor ! Abandon it

# # Change scales "Fires2" -----
# m1 <- glm(Fires ~ Traffic + HGV + Length, family="poisson", data=dff)
# summary(m1)
# # Residual deviance: 78.182  on 166  degrees of freedom no more over dispersion but...
# # AIC: Inf --> This is also a problem...
# simResids <- simulateResiduals(m1)
# plot(simResids)
# glm.diag.plots(m0)
# This model is also very poor --> abandon it.

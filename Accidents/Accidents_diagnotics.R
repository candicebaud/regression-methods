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
# Model1 ------------------------------------------------------------------
model <- glm(Acc ~ log(Traffic) + SlopeType + log(Length) + Limit + Type +
               Width + Direction + Slope + HGV + Year , family=poisson,
             data=Accidents)
# Test of dispersion
check_overdispersion(model)

# Test of outliers
check_outliers(model)

# Diagnostics
png(file="Accidents/plots/accidents_diagnostics1.png", width=600, height=600)
glm.diag.plots(model)
dev.off()
# Some other diagnostics, not to add in the final report.
# Checking linearity
checklinearity(model$fitted.values,model$residuals)

# Checking the variance
checkvariance(model$fitted.values,model$residuals)

# QQ-plot
qqplot(model)

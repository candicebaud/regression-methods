#In this file, we make diagnostics on the models we designed for Fires

# Libraries and files -----------------------------------------------------
library(ggplot2)
library('lme4')
library(MASS)
library(readr)
library(GGally)
library(boot)
library(glmmTMB)
library(performance)
source(file = "Fires/Fires_functions.R")
source(file = "explo_diag_functions.R")
# Loading data set --------------------------------------------------------
load("Fires/Fires.RData")

# Negative binomial model -------------------------------------------------
m0 <- glm(Fires ~ log(Traffic) + HGV + log(Length), family="poisson", data=Fires)

# Test of dispersion
check_overdispersion(m0)

# Test of outliers
check_outliers(m0)

# Diagnostics
png(file="Fires/plots/fires_diagnostics.png", width=600, height=600)
glm.diag.plots(m0)
dev.off()
# Some other diagnostics, not to add in the final report.
# Checking linearity
checklinearity(m0$fitted.values,m0$residuals)

# Checking the variance
checkvariance(m0$fitted.values,m0$residuals)

# QQ-plot
qqplot(m0)
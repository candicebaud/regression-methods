#In this file, we make diagnostics on the models we designed in "Breakdowns_models.R"

# Libraries and files -----------------------------------------------------
library(ggplot2)
library('lme4')
library(MASS)
library(readr)
library(GGally)
library(boot)
library(glmmTMB)
library(performance)
source(file = "Breakdowns/Breakdowns_functions.R")
# Loading data set --------------------------------------------------------
load("Breakdowns/Breakdowns.RData")
# Modify the data set so that it make sense --> explanations in "Breakdowns_functions.R"
dfnb <- mBreakdowns2nb()

# Negative binomial model -------------------------------------------------
nbmodel <- glm.nb(Breakdowns ~ Tunnel + Slope + Direction + Year + HGV, data = dfnb) 
# Test of dispersion
check_overdispersion(nbmodel)

# Test of outliers
check_outliers(nbmodel)

# Diagnostics
glm.diag.plots(nbmodel)

# Checking linearity
checklinearity(nbmodel$fitted.values,nbmodel$residuals)

# Checking the variance
checkvariance(nbmodel$fitted.values,nbmodel$residuals)

# QQ-plot
qqplot(nbmodel)

# Old models --------------------------------------------------------------
# -------------------------------------------------------------------------
# Poisson regression ------------------------------------------------------
# dff <- mBreakdowns2()
# poissonmodel <- glm(Breakdowns ~ Year + HGV + Slope + Limit + Traffic + Length +
#                     Direction + Urban + Type + SlopeType + Tunnel,
#                     family="poisson", data=dff)
# # Test of dispersion
# check_overdispersion(poissonmodel)
# # Diagnostics
# glm.diag.plots(poissonmodel)
# # Checking linearity
# checklinearity(poissonmodel$fitted.values,poissonmodel$residuals)
# # Checking the variance
# checkvariance(poissonmodel$fitted.values,abs(poissonmodel$residuals))
# # QQ-plot -->to change !
# qqplot(poissonmodel)

# -------------------------------------------------------------------------
# Linear model ------------------------------------------------------------
# df <- mBreakdowns()
# linearmodel <- lm(formula = Breakdowns~., data = df)
# # Checking linearity
# # plot(x = df$Traffics,y=linearmodel$residuals)
# plot(linearmodel$fitted.values,linearmodel$residuals)
# # Checking the variance
# plot(linearmodel$fitted.values,abs(linearmodel$residuals))# 
# 
# # QQ-plot
# qqnorm(linearmodel$residuals, pch = 1, frame = FALSE)
# qqline(linearmodel$residuals, col = "red", lwd = 2)
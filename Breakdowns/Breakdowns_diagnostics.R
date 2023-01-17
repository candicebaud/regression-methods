#In this file, we make diagnostics on the models we designed in "Breakdowns_models.R"

# Libraries and files -----------------------------------------------------
library(ggplot2)
library('lme4')
library(MASS)
library(readr)
library(GGally)
library(boot)
source(file = "Breakdowns/Breakdowns_functions.R")
# Loading data set --------------------------------------------------------
load("Breakdowns/Breakdowns.RData")
# Modify the data set so that it make sense --> explanations in "Breakdowns_functions.R"
df <- mBreakdowns()
dff <- mBreakdowns2()
dfnb <- mBreakdowns2nb()

# Negative binomial model -------------------------------------------------
nbmodel <- glm.nb(Breakdowns ~ Tunnel + Slope + Direction + Year + HGV, data = dfnb) 

plot(nbmodel$fitted.values,nbmodel$residuals)
glm.diag.plots(nbmodel)
# Checking linearity
checklinearity(nbmodel$fitted.values,nbmodel$residuals)

# Checking the variance
checkvariance(nbmodel$fitted.values,nbmodel$residuals)

# QQ-plot
qqplot(nbmodel)

# Poisson regression ------------------------------------------------------
poissonmodel <- glm(Breakdowns ~ Year + HGV + Slope + Limit + Traffic + Length + 
                    Direction + Urban + Type + SlopeType + Tunnel, 
                    family="poisson", data=dff)
# Diagnostics
plot(poissonmodel$fitted.values,poissonmodel$residuals)
glm.diag.plots(poissonmodel)
# Checking linearity
plot(poissonmodel$fitted.values,poissonmodel$residuals)
# Checking the variance
plot(poissonmodel$fitted.values,abs(poissonmodel$residuals))

# QQ-plot -->to change !
qqnorm(poissonmodel$residuals, pch = 1, frame = FALSE, qtype = )
qqline(poissonmodel$residuals, col = "red", lwd = 2)

# Linear model ------------------------------------------------------------
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


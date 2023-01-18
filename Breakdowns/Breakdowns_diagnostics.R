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
library(xtable)
source(file = "Breakdowns/Breakdowns_functions.R")
source(file = "explo_diag_functions.R")
# Loading data set --------------------------------------------------------
load("Breakdowns/Breakdowns.RData")
# Modify the data set so that it make sense --> explanations in "Breakdowns_functions.R"
dfnb <- mBreakdowns2nb()
# Negative binomial model -------------------------------------------------
# Best model given in "Breakdowns_models.R"
nbmodel <- glm.nb(Breakdowns ~ Slope + Direction + Year + HGV+Tunnel , data = dfnb)

# Export summary
modexport <- data.frame(Coefficients = nbmodel$coefficients[1:13], 
                        Std.Error = coef(summary(nbmodel))[1:13,2],
                        z.value = coef(summary(nbmodel))[1:13,3],
                        Pr_z = coef(summary(nbmodel))[1:13,4])
colnames(modexport) <- c("Coefficients", "Std.Error", "z value", "Pr(>|z|)")
rownames(modexport) <- c("(Intercept)", "Slope", "Direction: Direction 2",
                         "Year: 2003", "Year: 2004", "Year: 2005", "Year: 2006",
                         "Year: 2007", "Year: 2008", "Year: 2009", "Year: 2010", 
                         "Year: 2011", "HGV")

print.xtable(xtable(modexport,digits = 4,display = c(rep("f", 4),"e")), 
             file = "Breakdowns/Breakdowns_model.tex")

# Test of dispersion
check_overdispersion(nbmodel)

# Test of outliers
check_outliers()

# Diagnostics
png(file="Breakdowns/plots/breakdowns_diagnostics.png", width=600, height=600)
glm.diag.plots(nbmodel)
dev.off()
# Some other diagnostics, not to add in the final report.
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
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
# BESTS MODELS ------------------------------------------------------------
# Poisson -----------------------------------------------------------------
print("Poisson model")
opt1 <- glm(Fires ~ log(Traffic) + HGV + log(Length) + Traffic, 
            family="poisson", data=Fires)
# # Export model
# modexport <- data.frame(Coefficients = opt1$coefficients, 
#                         Std.Error = coef(summary(opt1))[,2],
#                         z.value = coef(summary(opt1))[,3],
#                         Pr_z = coef(summary(opt1))[,4])
# colnames(modexport) <- c("Coefficients", "Std.Error", "z value", "Pr(>|z|)")
# rownames(modexport) <- c("(Intercept)", "log(Traffic)","HGV","log(Length)",
#                          "Traffic")
# 
# print.xtable(xtable(modexport,digits = 4, display = c(rep("f", 4),"e")), 
#              file = "Fires/Fires_modelglm.tex")
# # Test of dispersion
# check_overdispersion(opt1)
# 
# # Test of outliers
# check_outliers(opt1)
# 
# # Diagnostics
# png(file="Fires/plots/fires_diagnosticsglm.png", width=600, height=600)
diagnosticsglm(opt1)
# dev.off()
# # Some other diagnostics, not to add in the final report.
# # Checking linearity
# checklinearity(opt1$fitted.values,opt1$residuals)
# 
# # Checking the variance
# checkvariance(opt1$fitted.values,opt1$residuals)
# 
# # QQ-plot
# qqplot(opt1)

# Poisson random ----------------------------------------------------------
print("Poisson mixed model")
opt2 <- glmer(Fires ~ log(Traffic) + HGV + log(Length) + (1 | Tunnel ),
              data = Fires, family=poisson)
# # Export model
# # Fixed effects
# modexport <- as.data.frame(summary(opt2)$coefficients)
# colnames(modexport) <- c("Coefficients", "Std.Error", "z value", "Pr(>|z|)")
# rownames(modexport) <- c("(Intercept)", "log(Traffic)","HGV", "log(Length)")
# 
# print.xtable(xtable(modexport,digits = 4, display = c(rep("f", 4),"e")), 
#              file = "Fires/Fires_modelglmr_fixed.tex")
# 
# # Test of dispersion
# check_overdispersion(opt2)
# 
# # Test of outliers
# check_outliers(opt2)
# 
# # Diagnostics
# png(file="Fires/plots/fires_diagnosticsglmr.png", width=600, height=600)
diagnosticsglmer(opt2)
# dev.off()
# # Some other diagnostics, not to add in the final report.
# # Checking linearity
# checklinearity(opt2$fitted.values,opt2$residuals)
# 
# # Checking the variance
# checkvariance(opt2$fitted.values,opt2$residuals)
# 
# # QQ-plot
# qqplot(opt2)

# OLD MODELS --------------------------------------------------------------
# # Poisson model -----------------------------------------------------------
# m0 <- glm(Fires ~ log(Traffic) + HGV + log(Length), family="poisson", data=Fires)
# 
# # Test of dispersion
# check_overdispersion(m0)
# 
# # Test of outliers
# check_outliers(m0)
# 
# # Diagnostics
# png(file="Fires/plots/fires_diagnostics0.png", width=600, height=600)
# diagnosticsglm(m0)
# dev.off()
# # Some other diagnostics, not to add in the final report.
# # Checking linearity
# checklinearity(m0$fitted.values,m0$residuals)
# 
# # Checking the variance
# checkvariance(m0$fitted.values,m0$residuals)
# 
# # QQ-plot
# qqplot(m0)
# # Poisson model with sqrt link --------------------------------------------
# # Less accurate :(
# m1 <- glm(Fires ~ Traffic +Length+HGV+Slope+Urban,
#               family= poisson(link = make.link("sqrt")), data=Fires)
# 
# # Test of dispersion
# check_overdispersion(m1)
# 
# # Test of outliers
# check_outliers(m1)
# 
# # Diagnostics
# png(file="Fires/plots/fires_diagnostics1.png", width=600, height=600)
# diagnosticsglm(m1)
# dev.off()
# # Some other diagnostics, not to add in the final report.
# # Checking linearity
# checklinearity(m1$fitted.values,m1$residuals)
# 
# # Checking the variance
# checkvariance(m1$fitted.values,m1$residuals)
# 
# # QQ-plot
# qqplot(m1)
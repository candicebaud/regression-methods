# In this file is my own exploration of Fire and some tests. Codes are originally 
# from Candice

# Libraries and files -----------------------------------------------------
library(ggplot2)
library("lme4")
library(MASS)
library(readr)
library(GGally)
library(DHARMa)
# Loading data ------------------------------------------------------------
load("Fires/Fires.RData")
# Functions ---------------------------------------------------------------
mFires <- function(){
  df <- Fires
  # Change to factor
  df$Limit <- as.factor(df$Limit/10)
  # Change scale
  df$Traffic <- df$Traffic/10^7
  df$HGV     <- df$HGV*100
  df$Slope   <- df$Slope*1000
  df$Length  <- df$Length/100
  return(df)
}

mFires2 <- function(){
  df <- Fires
  # Change to factor
  df$Limit <- as.factor(df$Limit/10)
  # Change scale
  df$Fires <- 0.5*sqrt(df$Fires)
  df$Traffic <- df$Traffic/10^7
  df$HGV     <- df$HGV*100
  df$Slope   <- df$Slope*1000
  df$Length  <- df$Length/100
  return(df)
}
# Models ------------------------------------------------------------------
df <- mFires()
dff <- mFires2()
# Poisson 1 ----
# Initial ------
m0 <- glm(Fires ~ log(Traffic) + HGV + log(Length), family="poisson", data=Fires)
summary(m0)
# Residual deviance: 191.59  on 166  degrees of freedom --> over dispersion !
# AIC: 368.67
simResids <- simulateResiduals(m0)
plot(simResids)

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
# # This model is also very poor --> abandon it.

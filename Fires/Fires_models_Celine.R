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
  df$Fires <- 0.5*sqrt(df$Fires)
  df$Traffic <- df$Traffic/10^7
  df$HGV     <- df$HGV*100
  df$Slope   <- df$Slope*1000
  df$Length  <- df$Length/100
  return(df)
}
# Models ------------------------------------------------------------------
df <- mFires()
m2 <- glm(Fires ~ Traffic + HGV + Length, family="poisson", data=Fires)
summary(m2)


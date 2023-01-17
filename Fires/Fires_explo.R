# In this file we proceed to all computations related to explorations on the 
# Fires data set.

# Libraries and files -----------------------------------------------------
library(ggplot2)
library('lme4')
library(MASS)
library(readr)
library(GGally)
source(file = "Fires/Fires_functions.R")
source(file = "explo_diag_functions.R")
# Loading data set --------------------------------------------------------
load("Fires/Fires.RData")
# Data visualisation ------------------------------------------------------
# Change the scale of the parameters that needs to
mFires <- mFires()

# Repartition of the response variable y
png(file="Fires/plots/fires_ydensity.png", width=600, height=600)
ggplot(Fires, aes(x=Fires))+
  geom_histogram(aes(y=..density..), alpha=0.4, position="identity", lwd=0.1)+
  geom_density(alpha=0.3, lwd=1)+
  xlab("Number of Fires")+
  ggtitle("Repartition of Fires")+
  theme(text = element_text(size = 20)) 
dev.off()

png(file="Fires/plots/fires_ycount.png", width=600, height=600)
ggplot(Fires, aes(x=Fires))+
  geom_histogram(alpha=0.4, position="identity", lwd=0.1)+
  xlab("Number of Fires")+
  ggtitle("Repartition of Fires")+
  theme(text = element_text(size = 20)) 
dev.off()

# Correlation of parameters
# Best try
png(file="Fires/plots/fires_corr.png", pointsize = 14, 
    width=800, height=800)
suppressWarnings(pairs(~ Fires + Traffic + HGV + Slope + Limit + Length, 
                       data=Fires, upper.panel = panel.cor, diag.panel = panel.hist))
dev.off()
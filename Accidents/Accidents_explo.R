# In this file we proceed to all computations related to explorations on the 
# Accidents data set.

# Libraries and files -----------------------------------------------------
library(ggplot2)
library('lme4')
library(MASS)
library(readr)
library(GGally)
source(file = "explo_diag_functions.R")
# Loading data set --------------------------------------------------------
load("Accidents/Accidents.RData")
# Data visualisation ------------------------------------------------------
# Repartition of the response variable y
# png(file="Accidents/plots/accidents_ydensityc.png", width=600, height=600)
# p<-data.frame(rexp(100,1/mean(Accidents$Acc)))
# ggplot(p,aes(x=p[,1]))+
#   geom_histogram(aes(y=..density..), alpha=0.4, position="identity", lwd=0.1)+
#   geom_density(alpha=0.5, lwd=1, bw=0.5)+
#   xlab("Number of Accidents")+
#   ggtitle("Repartition of Accidents")+
#   theme(text = element_text(size = 20)) 
# dev.off()

sim<- rpois(length(Accidents$Acc), mean(Accidents$Acc))
df<- data.frame(obs = c(Accidents$Acc, sim),
                group= c(rep("Accidents", length(Accidents$Acc)),
                             rep("Simulation", length(Accidents$Acc))))
# png(file="Accidents/plots/accidents_ycomp.png", width=600, height=600)
ggplot(df, aes(x=obs, fill = group))+
  geom_bar(alpha=0.5, position="identity", lwd=0.1)+
  xlim(0,30)+
  xlab("Value of observation")+
  ggtitle("Repartition of Accidents")+
  theme(text = element_text(size = 20), legend.title=element_blank())
# dev.off()
                        

# png(file="Accidents/plots/accidents_ydensity.png", width=600, height=600)
# ggplot(Accidents,aes(x=Acc))+
#   geom_histogram(aes(y=..density..), alpha=0.4, position="identity", lwd=0.1)+
#   geom_density(alpha=0.5, lwd=1, bw=0.5)+
#   xlab("Number of Accidents")+
#   ggtitle("Repartition of Accidents")+
#   theme(text = element_text(size = 20)) 
# dev.off()


# png(file="Accidents/plots/accidents_ycount.png", width=600, height=600)
# ggplot(Accidents, aes(x=Acc))+
#   geom_histogram(alpha=0.4, position="identity", lwd=0.1)+
#   xlab("Number of Accidents")+
#   ggtitle("Repartition of Accidents")+
#   theme(text = element_text(size = 20)) 
# dev.off()
# 
# # Correlation of parameters
# # Best try
# png(file="Accidents/plots/accidents_corr.png", pointsize = 14, 
#     width=800, height=800)
suppressWarnings(pairs(~ Acc + Traffic + Limit + Lanes + Urban + SlopeType ,
                       data=Accidents, upper.panel = panel.cor, diag.panel = panel.hist))
# dev.off()
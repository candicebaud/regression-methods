# In this file we proceed to all computations related to explorations on the 
# Breakdows data set.


# Libraries and files -----------------------------------------------------
library(ggplot2)
library('lme4')
library(MASS)
library(readr)
library(GGally)
source(file = "Breakdowns/Breakdowns_functions.R")
# Loading data set --------------------------------------------------------
load("Breakdowns/Breakdowns.RData")
# Data visualisation ------------------------------------------------------
# Change the scale of the parameters that needs to
mBreakdowns <- mBreakdowns()

# Repartition of the response variable y
png(file="Breakdowns/plots/breakdowns_ydensity.png", width=600, height=600)
ggplot(Breakdowns, aes(x=Breakdowns))+
  geom_histogram(aes(y=..density..), alpha=0.4, position="identity", lwd=0.1)+
  geom_density(alpha=0.3, lwd=1)+
  xlab("Number of breakdowns")+
  ggtitle("Repartition of Breakdowns")+
  theme(text = element_text(size = 20)) 
dev.off()

png(file="Breakdowns/plots/breakdowns_ycount.png", width=600, height=600)
ggplot(Breakdowns, aes(x=Breakdowns))+
  geom_histogram(alpha=0.4, position="identity", lwd=0.1)+
  xlab("Number of breakdowns")+
  ggtitle("Repartition of Breakdowns")+
  theme(text = element_text(size = 20)) 
dev.off()

# Correlation of parameters
# Bringing Urban into the correlogram does not bring that much information.
# Surely in urban area there is much more traffic and HGV % than in non urban one.
# After some try, we can observe that number of Breakdows pro year seems stable

# Best try
png(file="Breakdowns/plots/breakdowns_corr.png", pointsize = 14, 
    width=800, height=800)
suppressWarnings(pairs(~ Breakdowns + Traffic + HGV + Length + SlopeType + Limit,
                       data=mBreakdowns, upper.panel = panel.cor, diag.panel = panel.hist))
dev.off()


# Old tries - Draft -------------------------------------------------------
# Correlation - old tries
# suppressWarnings(pairs(~ Breakdowns + Traffic + HGV + Slope + Length + Limit + Company,
#                        data=mBreakdowns, upper.panel = panel.cor, diag.panel = panel.hist))

# suppressWarnings(pairs(~ Breakdowns + Traffic + HGV + Urban + Type + Length + Tunnel,
#                        data=mBreakdowns, upper.panel = panel.cor, diag.panel = panel.hist))
# suppressWarnings(pairs(~ Breakdowns + Traffic + HGV + Year +Limit + Length + Tunnel,
#                        data=mBreakdowns, upper.panel = panel.cor, diag.panel = panel.hist))
# suppressWarnings(pairs(~ Breakdowns + Traffic + Slope + Length + SlopeType + Limit + Company,
#                        data=mBreakdowns, upper.panel = panel.cor, diag.panel = panel.hist))


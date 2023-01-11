# In this file we proceed to all computations related to explorations on the 
# Breakdows data set.


# Libraries and files -----------------------------------------------------
library(ggplot2)
library('lme4')
library(MASS)
library(readr)
library(GGally)
# Loading data set --------------------------------------------------------
load("Breakdowns/Breakdowns.RData")
# Functions ---------------------------------------------------------------
# Modify scales of the data frame
mBreakdowns <- function(){
  df <- Breakdowns
  # Change to factor
  df$Year  <- as.factor(df$Year)
  df$Limit <- as.factor(df$Limit)
  # Change scale
  df$Traffic <- log10(df$Traffic)
  df$HGV     <- df$HGV*100
  df$Slope   <- df$Slope*1000
  df$Length  <- log10(df$Length)
  return(df)
}

# Functions of Candice for correlograms
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Data visualisation ------------------------------------------------------
# Change the scale of the parameters that needs to
mBreakdowns <- mBreakdowns()

# Repartition of the response variable y
png(file="Breakdowns/plots/breakdowns_ydensity.png", width=600, height=600)
ggplot(Breakdowns, aes(x=Breakdowns))+
  geom_histogram(aes(y=..density..), alpha=0.4, position="identity", lwd=0.1)+
  geom_density(alpha=0.3, lwd=1)+
  xlab("Number of breakdowns")+
  ggtitle("Repartition of Breakdowns")
dev.off()

png(file="Breakdowns/plots/breakdowns_ycount.png", width=600, height=600)
ggplot(Breakdowns, aes(x=Breakdowns))+
  geom_histogram(alpha=0.4, position="identity", lwd=0.1)+
  xlab("Number of breakdowns")+
  ggtitle("Repartition of Breakdowns")
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


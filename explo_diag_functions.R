# In this files are some functions for data exploration and data analysis.
# Libraries and files -----------------------------------------------------
library(DHARMa)
# Data exploration --------------------------------------------------------
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
# Diagnostics -------------------------------------------------------------
#For all models
checklinearity<- function(fitted, residual){
  ggplot(data.frame(x = fitted, y =residual), aes(x=x, y=y))+
    geom_point(color = "black", alpha = 0.4, size =0.8)+
    xlim(0,30)+
    xlab("Fitted values")+
    ylab("Residuals")+
    ggtitle("Residuals vs fitted values")
}

checkvariance <- function(fitted, residual){
  checklinearity(fitted, abs(residual))
}
# From the package DHARMa
qqplot  <- function(modelx){
  simResids <- simulateResiduals(modelx)
  plotQQunif(simResids)
}
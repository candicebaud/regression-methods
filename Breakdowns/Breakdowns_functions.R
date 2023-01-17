# In this file, there are all the functions used that are linked to Breakdowns
# In this case, y = Breakdowns is the response variable of interest.
# A first observation we can make is that E[y]!= Var[y]. This is quite annoying
# since we have a count data and we would wish to apply a Poisson regression.
# But if X~Poisson, then E[X]=Var[X]. A simple transformation allows us to solve
# this problem.


# Libraries and files -----------------------------------------------------
library(DHARMa)
# Modify scales ---------------------------------------------------------------
# Modify scales of the data frame
mBreakdowns <- function(){
  df <- Breakdowns
  # Change to factor
  df$Year  <- as.factor(df$Year/10^2) #We also need to rescale this one
  df$Limit <- as.factor(df$Limit/10)
  # Change scale
  df$Breakdowns <- 0.5*sqrt(df$Breakdowns)
  df$Traffic <- log10(df$Traffic)
  df$HGV     <- df$HGV*100
  df$Slope   <- df$Slope*1000
  df$Length  <- df$Length/100
  return(df)
}

# Modify scales of the data frame
mBreakdowns2 <- function(){
  df <- Breakdowns
  # Change to factor
  df$Year  <- as.factor(df$Year/10^2) #We also need to rescale this one
  df$Limit <- as.factor(df$Limit/10)
  # Change scale
  df$Breakdowns <- 0.5*sqrt(df$Breakdowns)
  df$Traffic <- df$Traffic/10^7 #Only change with the previous adjustments
  df$HGV     <- df$HGV*100
  df$Slope   <- df$Slope*1000
  df$Length  <- df$Length/100
  return(df)
}

# Modify scales of the data frame for negative binomial
# We proceed the following : we consider a different manner of scaling the 
# parameters in the data base. In particular, we won't scale y=Breakdowns any
# more, as we do not require E[y]=Var[y] in order to use a negative binomial model.
mBreakdowns2nb <- function(){
  df <- Breakdowns
  # Change to factor
  df$Year  <- as.factor(df$Year/10^2) #We also need to rescale this one
  df$Limit <- as.factor(df$Limit/10)
  # Change scale
  df$Traffic <- df$Traffic/10^7 #Only change with the previous adjustments
  df$HGV     <- df$HGV*100
  df$Slope   <- df$Slope*1000
  df$Length  <- df$Length/100
  return(df)
}
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
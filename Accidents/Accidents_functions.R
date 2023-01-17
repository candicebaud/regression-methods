# In this file, there are all the functions used that are linked to Accidents
# In this case, y = Accidents is the response variable of interest.
# A first observation we can make is that E[y]!= Var[y]. This is quite annoying
# since we have a count data and we would wish to apply a Poisson regression.
# But if X~Poisson, then E[X]=Var[X]. A simple transformation allows us to solve
# this problem.

# Modify scales ---------------------------------------------------------------
# Modify scales of the data frame
# NEED TO BE MODIFIED
mAccidents <- function(){
  df <- Accidents
  # Change to factor
  df$Year  <- as.factor(df$Year/10^2) #We also need to rescale this one
  df$Limit <- as.factor(df$Limit/10)
  # Change scale
  df$Acc <- 0.5*sqrt(df$Acc)
  df$Traffic <- log10(df$Traffic)
  df$HGV     <- df$HGV*100
  df$Slope   <- df$Slope*1000
  df$Length  <- df$Length/100
  return(df)
}

# Modify scales of the data frame
mAccidents2 <- function(){
  df <- Accidents
  # Change to factor
  df$Year  <- as.factor(df$Year/10^2) #We also need to rescale this one
  df$Limit <- as.factor(df$Limit/10)
  # Change scale
  df$Acc <- 0.5*sqrt(df$Acc)
  df$Traffic <- df$Traffic/10^7 #Only change with the previous adjustments
  df$HGV     <- df$HGV*100
  df$Slope   <- df$Slope*1000
  df$Length  <- df$Length/100
  return(df)
}

# Modify scales of the data frame for negative binomial
# We proceed the following : we consider a different manner of scaling the 
# parameters in the data base. In particular, we won't scale y=Accidents any
# more, as we do not require E[y]=Var[y] in order to use a negative binomial model.
mAccidents2nb <- function(){
  df <- Accidents
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
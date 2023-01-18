# In this file are some functions for data base Fires
# Functions ---------------------------------------------------------------
# Modify scales of the data frame
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

mFires3 <- function(){
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
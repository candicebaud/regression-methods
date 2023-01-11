# In this file, there are all the functions used that are linked to Breakdowns

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

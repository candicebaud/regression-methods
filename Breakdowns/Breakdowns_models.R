# In this file, there are all the models fitted for the data set Breakdowns

# Libraries and files -----------------------------------------------------
library(ggplot2)
library('lme4')
library(MASS)
library(readr)
library(GGally)
source(file = "Breakdowns/Breakdowns_functions.R")
# Loading data set --------------------------------------------------------
load("Breakdowns/Breakdowns.RData")
# Modify the data set so that it make sense --> explanations in "Breakdowns_functions.R"
df <- mBreakdowns()
dff <- mBreakdowns2()
# Linear model ------------------------------------------------------------
# The very first try is to fit a linear model. It is expected not to work well
# since y=Breakdowns does not follow a Gaussian law.
 linearmodel <- lm(formula = Breakdowns~., data = df)
 summary(linearmodel)
# Most significant variables : Year, Direction,  HGV, Slope, Limit, some tunnels...
# Multiple R-squared:  0.7842,	Adjusted R-squared:  0.7603

# Poisson regression ------------------------------------------------------
# Based on the first choice of changes in the scales of variables
### Backward selection of variables ######
Poissonfull <- glm(Breakdowns ~ Year + HGV + Slope + Limit + Traffic+ Length + Direction+
                Urban + Type  + SlopeType + Tunnel + Company, 
                family="poisson", data=df)
model <- step(Poissonfull, direction = 'backward')

# Output: Step:  AIC=Inf
# Breakdowns ~ Year + HGV + Slope + Limit + Traffic + Length + 
  # Direction + Urban + Type + SlopeType + Tunnel
# Residual deviance:  298.49  on  937  degrees of freedom
# Slope seems to be significant, Limit maybe also

# What about problem of AIC = Inf  ???? -->next part on glm.nb
# summary(model)

### Forward selection of variables ######

 # Poissonempty0 <- glm(Breakdowns ~ Traffic, family="poisson",data=df)
 Poissonempty <- glm(Breakdowns ~1,        family="poisson",data=df)
 
 model_e <- step(Poissonempty, scope = ~Year + Direction +Traffic + HGV + Slope +
                 Urban + Type + Length + Limit + SlopeType + Tunnel + Company,
               direction = 'forward')
 
 # Output: Step:  AIC=Inf
 # Breakdowns ~ 1
 # Residual deviance: 1106.2  on 1041  degrees of freedom. 1106.2/1041 = 1.062632
 # Maybe I could have a model that suffers from over dispersion !!!
 
 # What about the large AIC ? --> See next part
 # summary(model_e)


# Poisson regression (2) ----------------------------------------------------
 # Based on the second choice of changes in the scales of variables
 ### Backward selection of variables ######
 Poissonfull2 <- glm(Breakdowns ~ Year + HGV + Slope + Limit + Traffic+ Length + Direction+
                       Urban + Type  + SlopeType + Tunnel + Company, 
                     family="poisson", data=dff)
 model2 <- step(Poissonfull2, direction = 'backward') 
 
 # Output : Step:  AIC=Inf
 # Breakdowns ~ Year + HGV + Slope + Limit + Traffic + Length + 
   # Direction + Urban + Type + SlopeType + Tunnel
 # Residual deviance:  298.31  on  937  degrees of freedom
 # summary(model2)
 
 ### Forward selection of variables ######
 Poissonempty2 <- glm(Breakdowns ~1,        family="poisson",data=dff)
 
 model_e2 <- step(Poissonempty2, scope = ~Year + Direction +Traffic + HGV + Slope +
                   Urban + Type + Length + Limit + SlopeType + Tunnel + Company,
                 direction = 'forward')
 
 # Output:  Step:  AIC=Inf
 # Breakdowns ~ 1
 # Residual deviance: 1106.2  on 1041  degrees of freedom -> 1106.2/1041 = 1.062632
 # Maybe I could have a model that suffers from over dispersion !!!
 # summary(model_e2)
 
 # Slightly better model than the with the previous choice of re-scaling parameters
 # --> we keep the one found using backward selection
 
 # It seems that this improves a bit the model --> we will keep this one
 
# Negative binomial model -------------------------------------------------
 # We have previously notice a problem of over dispersion which could be solve 
 # by using a negative binomial model.
 # We proceed the following : we consider a different manner of scaling the 
 # parameters in the data base. In particular, we won't scale y=Breakdowns any
 # more, as we do not require E[y]=Var[y] in order to use a negative binomial model.
 dfnb <- mBreakdowns2nb()
 
 ### Backward selection of variables ######
 nbfull <- glm.nb(Breakdowns ~ Year + HGV + Slope + Limit + Traffic+ Length + Direction+
                    Urban + Type  + SlopeType + Tunnel + Company, data = dfnb)
 nbmodel <- step(nbfull, direction = 'backward') 
 # summary(nbmodel)
 # Output: Step:  AIC=6475.58
 # Breakdowns ~ Year + HGV + Slope + Direction + Tunnel
 
 ### Forward selection of variables ######
 nbempty <- glm.nb(Breakdowns ~ 1, data = dfnb)
 nbmodel_e <- step(nbempty, scope = ~Year + Direction +Traffic + HGV + Slope +
                     Urban + Type + Length + Limit + SlopeType + Tunnel + Company,
                   direction = 'forward')
 
 # Output: Step:  AIC=6475.58
 # Breakdowns ~ Tunnel + Slope + Direction + Year + HGV

 # Both selections of variables based on AIC yields to the same model. 
 # We can notice that the deviance here is much higher than in the previous case.
 # The model achieved is yet much simpler -> which one to choose ?
 # Need to make diagnostics !

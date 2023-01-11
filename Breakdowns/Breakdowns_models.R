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
# Modify the data set so that it make sens --> explanation in "Breakdowns_functions.R"
df <- mBreakdowns()
dff <- mBreakdowns2()
# dff$Traffic <- 10^df$Traffic/10^8 regarder le changement !!
# Linear model ------------------------------------------------------------
# The very first try is to fit a linear model. It is expected not to work well
# since y=Breakdowns does not follow a Gaussian law.
linearmodel <- lm(formula = Breakdowns~., data = df)
summary(linearmodel)
# Most significative variables : Year, HGV, Slope, Limit, some tunnels...
# Multiple R-squared:  0.7325,	Adjusted R-squared:  0.7028

# Poisson regression ------------------------------------------------------
### Backward selection of variables ######
Poissonfull <- glm(Breakdowns ~ Year + HGV + Slope + Limit + Traffic+ Length + Direction+
                Urban + Type  + SlopeType + Tunnel + Company, 
                family="poisson", data=df)
model <- step(Poissonfull, direction = 'backward')
# Output: Step:  AIC=8514.27
# Breakdowns ~ Year + HGV + Slope + Length + Direction + Tunnel
# What about problem of high deviance ???? -->next part on glm.nb

### Forward selection of variables ######

 # Poissonempty0 <- glm(Breakdowns ~ Traffic, family="poisson",data=df)
 Poissonempty <- glm(Breakdowns ~1,        family="poisson",data=df)
 
 model_e <- step(Poissonempty, scope = ~Year + Direction +Traffic + HGV + Slope +
                 Urban + Type + Length + Limit + SlopeType + Tunnel + Company,
               direction = 'forward')
 
 # Output: Step:  AIC=8514.27
 # Breakdowns ~ Tunnel + Slope + Year + HGV + Direction + Length
 # What about the large deviance ? --> See next part
 
 # An important thing to notice here is that both backward and forward selection
 # based on AIC yields to the same choice of model :)
 

# Second Poisson ----------------------------------------------------------
 ### Backward selection of variables ######
 Poissonfull2 <- glm(Breakdowns ~ Year + HGV + Slope + Limit + Traffic+ Length + Direction+
                       Urban + Type  + SlopeType + Tunnel + Company, 
                     family="poisson", data=df)
 model2 <- step(Poissonfull, direction = 'backward') 
 
 # Output : AIC=8507.42
 # Breakdowns ~ Year + HGV + Slope + Traffic + Length + Direction + Tunnel
 
 ### Forward selection of variables ######
 Poissonempty <- glm(Breakdowns ~1,        family="poisson",data=dff)
 
 model_e2 <- step(Poissonempty, scope = ~Year + Direction +Traffic + HGV + Slope +
                   Urban + Type + Length + Limit + SlopeType + Tunnel + Company,
                 direction = 'forward')
 
 # Output:  Step:  AIC=8507.42
 # Breakdowns ~ Tunnel + Slope + Year + HGV + Direction + Length + Traffic
 
 # It seems that this improves a bit the model --> we will keep this one
 
# Improvements ------------------------------------------------------------
 # In this section, we start from the optimal model to see if it can be improve
 # it using a way to avoid overdispersion.


#In this file is the best Poisson model for Accidents
library('glmtoolbox')
load("Accidents/Accidents.RData")
library('lme4')
#log(Traffic) ok 5317
#log(Traffic) + log(Length) ok 5309
#log(Traffic) + log(Length) + Slope ok AIC 5297
# avec limit pb
#log(Traffic) + log(Length) + Slope + Type AIC 5294
#avec HGV fail to converge
#log(Traffic) + log(Length)  + HGV 5311
#avec Year fail 
#avec Direction fail 
#log(Traffic) + log(Length) + Slope + Type + Width 5291
#avec slopeType fail
modeltest<- glmer(Acc~ log(Traffic) + log(Length) + Slope + Type + Width + (1 | Tunnel) + (1 | Company), family=poisson, data = Accidents)
summary(modeltest)

# Note : all investigations have been made in "Accidents/Accidents_models_Celine.R" 
# and "Accidents/accidents_clean.Rmd"
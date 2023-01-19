#In this file is the best Poisson model for Accidents
library('glmtoolbox')
load("Accidents/Accidents.RData")

fullmodel <- glm(Acc ~ Lanes + Direction + log(Traffic) + HGV + Slope + Urban +
                   Type + log(Length) + Limit + SlopeType + Width + Lanes + Year,
                 family=poisson, data=Accidents)

stepCriterion(fullmodel, criterion = 'aic', direction = c('forward', 'backward'))

model <- glm(Acc ~ log(Traffic) + SlopeType + log(Length) + Limit + Type + 
               Width + Direction + Slope + HGV + Year , family=poisson, 
             data=Accidents)
summary(model)

# Note : all investigations have been made in "Accidents/Accidents_models_Celine.R" 
# and "Accidents/accidents_clean.Rmd"
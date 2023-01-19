library('glmtoolbox')
load("Fires/Fires.RData")

# Best Model Only ---------------------------------------------------------


fullmodel <- glm(Fires ~ log(Traffic) + HGV + Slope + Limit+ log(Length) + Direction + Urban + Type + Length + Traffic, family="poisson", data=Fires)
model <- stepCriterion(fullmodel, direction = 'backward')
# The best model according to AIC includes the following variables :
model$final
# The best model
m2 <- glm(Fires ~ log(Traffic) + HGV + log(Length) + Traffic, family="poisson", data=Fires)
summary(m2)

# Note : all investigations have been made in "Fires/Fires_models_Celine.R" 
# and "Fires/fires_clean.Rmd
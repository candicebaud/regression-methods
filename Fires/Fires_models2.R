#In this file is the best mixed model for Fires
library('lme4')
load("Fires/Fires.RData")
mixed_model <- glmer(Fires ~ log(Traffic) + HGV + log(Length) + (1 | Tunnel ), data = Fires, family=poisson)
summary(mixed_model)
# Note : all investigations have been made in "Fires/Fires_models_Celine.R" 
# and "Fires/fires_clean.Rmd
# In this file, there are all the models fitted for the data set Breakdowns
# We have chosen to keep the first model that appear in this file. Old models 
# and tries are kept under the section "Old models"
# Libraries and files -----------------------------------------------------
library(ggplot2)
library("lme4")
library(MASS)
library(readr)
library(GGally)
library(DHARMa)
source(file = "Breakdowns/Breakdowns_functions.R")
# Loading data set --------------------------------------------------------
load("Breakdowns/Breakdowns.RData")
# Modify the data set so that it make sense --> explanations in "Breakdowns_functions.R"
df <- mBreakdowns()
dff <- mBreakdowns2()
dfnb <- mBreakdowns2nb()
# Negative binomial model -------------------------------------------------
 # We have notice a problem of over dispersion in Poisson regression which could 
 # be solve by using a negative binomial model.
 ### Backward selection of variables ######
 nbfull <- glm.nb(Breakdowns ~ Year + HGV + Slope + Limit + Traffic+ Length + Direction+
                    Urban + Type  + SlopeType + Tunnel + Company, data = dfnb)
 nbmodel <- step(nbfull, direction = 'backward') 
 summary(nbmodel)
 # Output: Step:  AIC=6475.58
 # Breakdowns ~ Year + HGV + Slope + Direction + Tunnel
 # Residual deviance: 1196.3  on  939  degrees of freedom

 ### Forward selection of variables ######
 nbempty <- glm.nb(Breakdowns ~ 1, data = dfnb)
 nbmodel_e <- step(nbempty, scope = ~Year + Direction +Traffic + HGV + Slope +
                     Urban + Type + Length + Limit + SlopeType + Tunnel + Company,
                   direction = 'forward')
 
 # Output: Step:  AIC=6475.58
 # Breakdowns ~ Tunnel + Slope + Direction + Year + HGV
 # Residual deviance: 1196.3  on  939  degrees of freedom
 # Here we do a simple check
 summary(nbmodel_e)

 # Both selections of variables based on AIC yields to the same model. 
 # We can notice that the deviance here is much higher than Poisson(2).
 # The model achieved is yet much simpler -> which one to choose between Poisson(2)
 # and this model --> Need to make diagnostics !

 # Old models -------------------------------------------------------------
 # ------------------------------------------------------------------------
 # Linear model ------------------------------------------------------------
 # The very first try is to fit a linear model. It is expected not to work well
 # since y=Breakdowns does not follow a Gaussian law.
 # linearmodel <- lm(formula = Breakdowns~., data = df)
 # summary(linearmodel)
 # Most significant variables : Year, Direction,  HGV, Slope, Limit, some tunnels...
 # Multiple R-squared:  0.7843,	Adjusted R-squared:  0.7603
 
 # ------------------------------------------------------------------------
 # Poisson regression ------------------------------------------------------
 # # Based on the first choice of changes in the scales of variables
 # ### Backward selection of variables ######
 # Poissonfull <- glm(Breakdowns ~ Year + HGV + Slope + Limit + Traffic+ Length + Direction+
 #                      Urban + Type  + SlopeType + Tunnel + Company, 
 #                    family="poisson", data=df)
 # model <- step(Poissonfull, direction = 'backward')
 # 
 # # Output: Step:  AIC=Inf
 # # Breakdowns ~ Year + HGV + Slope + Limit + Traffic + Length + 
 # # Direction + Urban + Type + SlopeType + Tunnel
 # # Residual deviance:  298.14  on  937  degrees of freedom
 # # Slope seems to be significant, Limit maybe also
 # 
 # # Here we do a simple check
 # simResids <- simulateResiduals(model)
 # plot(simResids)
 # # This is very skewed...
 # 
 # # What about problem of AIC = Inf  ???? -->next part on glm.nb
 # # summary(model)
 # 
 # ### Forward selection of variables ######
 # 
 # # Poissonempty0 <- glm(Breakdowns ~ Traffic, family="poisson",data=df)
 # Poissonempty <- glm(Breakdowns ~1,        family="poisson",data=df)
 # 
 # model_e <- step(Poissonempty, scope = ~Year + Direction +Traffic + HGV + Slope +
 #                   Urban + Type + Length + Limit + SlopeType + Tunnel + Company,
 #                 direction = 'forward')
 # 
 # # Output: Step:  AIC=Inf
 # # Breakdowns ~ 1
 # # Residual deviance: 1106.2  on 1041  degrees of freedom. 1106.2/1041 = 1.062632
 # # Maybe I could have a model that suffers from over dispersion !!!
 # 
 # # What about the large AIC ? --> See next part
 # # summary(model_e)
 # # Here we do a simple check
 # simResids <- simulateResiduals(model_e)
 # plot(simResids)
 
 
 

 # ------------------------------------------------------------------------
 # Poisson regression (2) ----------------------------------------------------
 # # Based on the second choice of changes in the scales of variables
 # ### Backward selection of variables ######
 # Poissonfull2 <- glm(Breakdowns ~ Year + HGV + Slope + Limit + Traffic+ Length + Direction+
 #                       Urban + Type  + SlopeType + Tunnel + Company, 
 #                     family="poisson", data=dff)
 # model2 <- step(Poissonfull2, direction = 'backward') 
 # 
 # # Output : Step:  AIC=Inf
 # # Breakdowns ~ Year + HGV + Slope + Limit + Traffic + Length + 
 # # Direction + Urban + Type + SlopeType + Tunnel
 # # Residual deviance:  297.96  on  937  degrees of freedom
 # # summary(model2)
 # # Here we do a simple check
 # simResids <- simulateResiduals(model2)
 # plot(simResids)
 # 
 # # This is very skewed
 # ### Forward selection of variables ######
 # Poissonempty2 <- glm(Breakdowns ~1,        family="poisson",data=dff)
 # 
 # model_e2 <- step(Poissonempty2, scope = ~Year + Direction +Traffic + HGV + Slope +
 #                    Urban + Type + Length + Limit + SlopeType + Tunnel + Company,
 #                  direction = 'forward')
 # 
 # # Output:  Step:  AIC=Inf
 # # Breakdowns ~ 1
 # # Residual deviance: 1106.2  on 1041  degrees of freedom -> 1106.2/1041 = 1.062632
 # # Maybe I could have a model that suffers from over dispersion !!!
 # # summary(model_e2)
 # # Here we do a simple check
 # simResids <- simulateResiduals(modele2)
 # plot(simResids)
 # 
 # # Slightly better model than the with the previous choice of re-scaling parameters
 # # --> we keep the one found using backward selection
 # 
 # # It seems that this improves a bit the model --> we will keep this one
 # ------------------------------------------------------------------------
 # Including cross-variables effects ---------------------------------------
 
  # This model is almost the same in terms of performance as the one without cross-
  # effects. We would prefer to keep the simplest one.
 
# # As we had notice that some variables where correlated, we will try to add these 
# # relations in the model
#  nbfullc <- glm.nb(Breakdowns ~ Year + HGV + Slope + Limit + Traffic+ Length + Direction+
#                     Urban + Type  + SlopeType + Tunnel + Company + Traffic*HGV+
#                      Traffic*Length+ Traffic*SlopeType, data = dfnb)
#  nbmodelc <- step(nbfullc, direction = 'backward')
#  
#  # Step:  AIC: 6477.6
#  # Breakdowns ~ Year + HGV + Slope + Traffic + Direction + SlopeType + 
#    # Tunnel + Traffic:SlopeType
#  # Residual deviance: 1197.7  on  935  degrees of freedom
#  summary(nbmodelc)
#  simResids <- simulateResiduals(nbmodelc)
#  plot(simResids)

 # ------------------------------------------------------------------------
 # Including random effects Poisson ---------------------------------------
 # TAKE MUCH TOO MUCH TIME TO COMPUTE....
 # In this Poisson set-up, we will see if adding any random effect could improve
 # the model and decrease deviance. In this data set, Tunnel, Company and Year 
 # seems to be great candidates to randomness. A simple first comparison will be 
 # to take the model selected by backward selection (model2) and see if by turning
 # Year and Tunnel to random would increase or decrease deviance.
 
 # rmodel2 <-glmer(Breakdowns ~ 1+(1|Tunnel),
 # data = dff, family=poisson)
 
 # Comparison with the previous model2
 # anova(rmodel2,model2)
 # ------------------------------------------------------------------------
 # Including random effects glm.nb ----------------------------------------
 
 # This had not significantly improved the model : we will therefore keep it simple
 # and keep all variables as fixed effects !!!
 
 # # As from now, we have seen that the model that seems fit the best the data is 
 #  # the negative binomial one (QQ-plot is almost a strait line !). Deviance is still
 #  # quite high, so we will try to reduce it by introducing random effects. In this 
 #  # data set, Tunnel, Company and Year seems to be great candidates to randomness.
 #  # Let's see.
 #  nb1 <- glm.nb(Breakdowns ~ 1 + Tunnel, data = dfnb)
 #  nbrfull <- glmer.nb(Breakdowns ~ 1+(1|Tunnel), data = dfnb)
 #  anova(nbrfull,nb1)
 #  # Adding Tunnel as random seems not to improve significantly the model !
 #  
 #  nb1 <- glm.nb(Breakdowns~1 + Year, data = dfnb)
 #  nbrfull <- glmer.nb(Breakdowns ~ 1+(1|Year), data = dfnb)
 #  anova(nbrfull,nb1)
 #  # Adding Tunnel as random seems not to improve significantly the model !
 #  
 #  nbrfull <- glmer.nb(Breakdowns ~ Slope + HGV + Direction + (1|Year)+ (1|Tunnel),
 #                      data = dfnb)
 #  # modelfullr <- step(nbrfull, direction = 'backward') 
 #  summary(nbrfull)
 #  # AIC : 6717.7, Deviance : 6703.7
 #  # This yields to higher deviance than without random effects, we won't keep them
 #  # as random.
 #  # Here we do a simple check
 #  simResids <- simulateResiduals(nbrfull)
 #  plot(simResids)
 #  # Adding random effect does not seem to improve the model. we keep all variables
 #  # as fixed ones !
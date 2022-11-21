# packages and data -------------------------------------------------------
lapply(c("dplyr","chron","ggplot2","tidyr","questionr","survival","forcats",
         "data.table","table1","lubridate", "ggpubr","viridis","finalfit",
         "ggpubr", "ggthemes", "gridExtra", "tidyverse", "rstatix","ggsci",
         "wesanderson","kableExtra", "naniar","boot","scales","psych","ggcorrplot","ggradar", "caret"), 
       library, character.only=TRUE)

Sys.setlocale("LC_TIME", "English")
setwd("C:/Users/candi/Desktop/ETUDES/EPFL1A/semestre 1/regression methods/project")
load("C:/Users/candi/Desktop/ETUDES/EPFL1A/semestre 1/regression methods/project/Accidents.RData")

# Exploratory -------------------------------------------------------------
## visualisation of the number of accidents
Accidents %>% group_by(Acc) %>% summarise(number=n()) %>% mutate(freq = number/sum(number)) %>% 
  ggplot(aes(x=Acc, y = freq)) + geom_bar(stat='identity')
hist(Accidents$Acc)
#comment : the distribution looks like an exponential/poisson distribution, there is one obvious outlier (Acc>80), and some possible outliers (at 50)

## Plot of y = number of accidents against covariates (slide 55) and look for outliers, I start with numerical variables and then I do for categorical variables
plot(Accidents$Traffic, Accidents$Acc)
#comment: some extreme values after 1.2 e+08 in Traffic and at 80 in Acc

plot(Accidents$HGV, Accidents$Acc)
#comment : some extreme values of HGV at 0.6?, looks like an exponential distribution 


plot(Accidents$Slope,Accidents$Acc)
#comment : looks almost normal, big concentration at 0 where there are greater number of accidents 
hist(Accidents$Slope)
#comment : looking at the distribution of Slope, it seems quite logical because there are more tunnels with a slope of 0, normal distribution
summary(glm(Acc~Slope, family=gaussian, data=Accidents))
#AIC is 7993.2 and the slope coefficient is not significative


plot(Accidents$Length, Accidents$Acc)
#comment : some extreme values for the length
test <- glm((Acc+1) ~ Length, family=Gamma, data=Accidents)
summary(test,dispersion=1)
#AIC is 6137.5 and the Length coefficient is significative
f <- function(x, param){
  return(param*exp(-param*x))
}
x <- seq(0,12000,by=0.01)
plot(Accidents$Length, Accidents$Acc)
#hist(Accidents$Acc)


plot(Accidents$Limit, Accidents$Acc)
#comment : idk


plot(Accidents$Width, Accidents$Acc)
#comment : idk

plot(Accidents$Lanes, Accidents$Acc)
Accidents %>% group_by(Lanes, Acc)%>% summarise(number=n())%>% mutate(nb_acc = number*Acc) %>%
  ggplot(aes(x=Lanes, y=nb_acc)) + geom_bar(stat = 'identity')
#comment : it looks like for small and big lanes, there are less accidents but when we look at the distribution of the lanes:
hist(Accidents$Lanes)
#there are very few lanes=5 
LinReg <- lm(Acc ~ Lanes, data=Accidents)
plot(Accidents$Lanes, Accidents$Acc,bg="red")
abline(LinReg, lwd=3, col="blue")
#not convincing to me

#Categorical variables 
#year
Accidents%>% group_by(Year, Acc)%>% summarise(number=n())%>% mutate(nb_acc = number*Acc) %>%
  ggplot(aes(x=Year, y=nb_acc)) + geom_bar(stat = 'identity')
#comment : number of accidents is increasing with the years

#quick verification that my computations are correct
sum(Accidents$Acc)
test <- Accidents %>% group_by(Year, Acc)%>% summarise(number=n()) %>% mutate(nb_acc = number*Acc)
sum(test$nb_acc)

#Urban
Accidents%>%group_by(Urban, Acc)%>% summarise(number=n())%>%mutate(nb_acc=number*Acc)%>% 
  ggplot(aes(x=Urban, y=nb_acc)) + geom_bar(stat='identity')
#comment : most accidents are in an urban environment

Accidents%>%group_by(Type, Acc)%>% summarise(number=n())%>%mutate(nb_acc=number*Acc)%>% 
  ggplot(aes(x=Type, y=nb_acc)) + geom_bar(stat='identity')
#comment : most accidents are in an unidirectional tunnel 

Accidents%>%group_by(Direction, Acc)%>% summarise(number=n())%>%mutate(nb_acc=number*Acc)%>% 
  ggplot(aes(x=Direction, y=nb_acc)) + geom_bar(stat='identity')
#comment : more accidents are in direction 1 but significatitiveness ? 

Accidents%>%group_by(SlopeType, Acc)%>% summarise(number=n())%>%mutate(nb_acc=number*Acc)%>% 
  ggplot(aes(x=SlopeType, y=nb_acc)) + geom_bar(stat='identity')
#comment : unknown parameters -> we will have to deal with missing data 
#comment : most accidents are in a continuous slope and in basin

Accidents%>%group_by(Tunnel, Acc)%>% summarise(number=n())%>%mutate(nb_acc=number*Acc)%>% 
  ggplot(aes(x=Tunnel, y=nb_acc)) + geom_bar(stat='identity')
#comment : big differences between the different companies, very heterogeneous

Accidents%>%group_by(Company, Acc)%>% summarise(number=n())%>%mutate(nb_acc=number*Acc)%>% 
  ggplot(aes(x=Company, y=nb_acc)) + geom_bar(stat='identity')
#comment : big differences between the different companies, very heterogeneous


## Plot of covariates against each other to look at dependencies (slide 55)
#numerical variables 
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


#numerical variables
pairs(~ Acc + Traffic + HGV + Slope + Limit + Length + Width + Lanes,data=Accidents, upper.panel = panel.cor, diag.panel = panel.hist)
pairs(~ Traffic + Acc, data=Accidents, upper.panel = panel.cor, diag.panel = panel.hist)
#categorical variables

#table1
table1(~ Traffic + HGV + Slope + Limit + Length + Width + Lanes + Year + Direction +
         SlopeType + Tunnel + Company | Acc, data = Accidents,  render.continuous = c("Mean +/- SD" = "MEAN (SD)", "Median (Q1-Q3)" = "MEDIAN (Q1 - Q3)", "Min - Max" = "MIN - MAX"), render.missing=NULL, na.is.category = FALSE, digits = 2, overall = 'Total')

#missing data -> no missing data 
gg_miss_fct(x = Accidents, fct = Acc)

## Outliers -> I will fit the models with and without outliers and see if there is a big difference : if no big difference it's fine and if big difference, I will have to look in detail if the outlier has to be taken in account or not


# Model building ----------------------------------------------------------
#linear regression 
#with all the variables
Reg <- lm(Acc ~ ., data = Accidents)
summary(Reg)
par(mfrow=c(2,2))
plot(Reg)
#normal QQ doesn't look normal, residuals no heteroskedasticitys

#without tunnel and company
Reg2 <- lm(Acc ~ Traffic + HGV + Slope + Limit 
           + Length + Width + Lanes + Direction + Urban + Type + SlopeType, data=Accidents)
summary(Reg2)
plot(Reg2)
#doesn't look normal (QQPlot), residuals strange

#without all the categorical variables 
Reg3 <- lm(Acc ~ Traffic + HGV + Slope + Limit 
           + Length + Width + Lanes , data=Accidents)
summary(Reg3)
#Slope and HGV do not seem to have an effect; idem for lanes, direction and urban
plot(Reg3)
plot(predict(Reg3, newdata=Accidents[, -14]), rstandard(Reg3), xlab = 'Fitted values', ylab = 'Standardized residuals') 
#residuals aren't normal, outliers at 62, 1140

#standardised residuals
resstan <- rstandard(Reg3)
for (i in colnames(Accidents)) {
  plot(Accidents[[i]], resstan, xlab = i, ylab = 'Standardized residuals')
}

#without Slope and HGV
Reg4 <- lm(Acc ~ Traffic + Limit 
              + Length + Width + Lanes , data=Accidents)
summary(Reg4)
#length doesn't look significant

#log transformation (we put +1 because not defined at 0)
Reg5 <- lm(log(Acc+1) ~ Traffic + Limit 
           + Length + Width + Lanes , data=Accidents)
summary(Reg5)
plot(Reg5)
#length isn't significant and the Rsquared increased
#normal QQ looks good

#without length
Reg6 <- lm(log(Acc+1) ~ Traffic + Limit 
           + Width + Lanes , data=Accidents)
summary(Reg6)
#R squared didn't change that much 


# with AIC selection ------------------------------------------------------
nullmodel <- lm(Acc ~ 1, data = Accidents)
fullmodel <- lm(Acc ~ ., data = Accidents)

# Forward selection
model.step.f <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = 'forward')

# Backward elimination
model.step.b <- step(fullmodel, direction = 'backward')

# Stepwise selection
model.step <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = 'both')

# AICs and BICs
AIC(model.step.f)
BIC(model.step.f)
AIC(model.step.b)
BIC(model.step.b)
AIC(model.step)
BIC(model.step)

#they all give the model fitted with tunnel, year and lanes
#the AIC and BIC aren't that different


# Dummies ? ---------------------------------------------------------------
install.packages(c("fastDummies", "recipes"))
library('fastDummies')
datadummies <- dummy_cols(Accidents, select_columns = c('Tunnel', 'Company', 'SlopeType', 'Direction', 'Year'))


#if we do the same as before and we also get only Tunnel but it is not satisfying for a model
Reg7 <- lm(Acc ~ ., data=datadummies)
AIC7 <- MASS::stepAIC(Reg7, k = log(nrow(datadummies)))
summary(AIC7)

#linear regression may not be the most appropriate 
corr(Accidents)

# Analysis ----------------------------------------------------------------



# Discussion --------------------------------------------------------------



#In this file, we test if the distribution of the response y follows a Poisson
#distribution for each of the 3 dataset
#The procedures discribed below follows from the course MATH-517 : Statistical 
#Computation and visualisation, slide 22 of week 11 :
#https://github.com/TMasak/StatComp/blob/master/Slides/11_Bootstrap.pdf
#The principle is quite simple : --complete here net time

# Libraries and files -----------------------------------------------------
load("Data/Fires.RData")
load("Data/Accidents.RData")
load("Data/Breakdowns.RData")

set.seed(517)
# Poisson ---------------------------------------------------------------
MLE_poi <- function(xn){ #Compute MLE of a sample x1,...xn ~ Poi(lambda) (iid)
  return(mean(xn))
}

Param_bootstrap_poi <- function(xn, Bboot = 10000){
  #Compute MLE of sample xn
  n = length(xn)
  lambda <- MLE_poi(xn)
  
  #Bootstrap procedure
  Tb <- rep(NA,Bboot)
  for (b in 1:Bboot){
    #Generate sample from approximate candidate law Poi(lambda)
    xb_star <- rpois(n,lambda)
    #Estimate MLE of the sample
    lambda_b_star <- MLE_poi(xb_star)
    #Compute EDF
    F_Nb <- ecdf(xb_star)
    #Compute absolute difference
    grid <- 1:100
    difabs <- abs(F_Nb(grid)-ppois(grid,lambda_b_star))
    #Store result
    Tb[b]<-max(difabs)
    
     # plot(F_Nb)
     # points(grid,ppois(grid,lambda_b_star), type="l", color="red")
  }
  # plot(density(Tb))
  # abline(quantile(Tb,0.95))
  #Take 95-quantile
  return(quantile(Tb,0.95))
  
}


# Exponential -------------------------------------------------------------
MLE_expo <- function(xn){ #Compute MLE of a sample x1,...xn ~ exp(lambda) (iid)
  return(length(xn)/sum(xn))
}

Param_bootstrap_expo <- function(xn, Bboot = 10000){
  #Compute MLE of sample xn
  n = length(xn)
  lambda <- MLE_expo(xn)
  
  #Bootstrap procedure
  Tb <- rep(NA,Bboot)
  for (b in 1:Bboot){
    #Generate sample from approximate candidate law Poi(lambda)
    xb_star <- rexp(n,lambda)
    #Estimate MLE of the sample
    lambda_b_star <- MLE_expo(xb_star)
    #Compute EDF
    F_Nb <- ecdf(xb_star)
    #Compute absolute difference
    grid <- 1:100
    difabs <- abs(F_Nb(grid)-pexp(grid,lambda_b_star))
    #Store result
    Tb[b]<-max(difabs)
    
    # plot(F_Nb)
    # points(grid,ppois(grid,lambda_b_star), type="l", color="red")
  }
  # plot(density(Tb))
  # abline(quantile(Tb,0.95))
  #Take 95-quantile
  return(quantile(Tb,0.95))
}



# Dataset Fire ------------------------------------------------------------
yf <- Fires$Fires
syf<- Param_bootstrap_poi(yf)
expyf <- Param_bootstrap_expo(yf)

# Dataset Accident --------------------------------------------------------
ya <- Accidents$Acc
sya <- Param_bootstrap_poi(ya)
expya <- Param_bootstrap_expo(ya)

# Dataset Breakdowns ------------------------------------------------------
yb <- Breakdowns$Breakdowns
syb <- Param_bootstrap_poi(yb)
expyb <- Param_bootstrap_expo(yb)


# Tests Poisson -------------------------------------------------------------------
# Here, we expect quite small differences if we take a sample of the Poisson 
# distribution and big differences if we take an other distribution. 
# Thus, a large output value means that we don't have the same distribution and 
# a small value means that we have no evidence against the fact that the sample
# follows a law of Poisson.

# # Test 1
# s1 <- rpois(100, 1)
# r1 <- Param_bootstrap_poi(s1)
# 
# # Test 2
# s2 <- rpois(100, 4)
# r2 <- Param_bootstrap_poi(s2)
# 
# # Test 3 : we can see that MLE < 0, thus it can clearly not be Poisson sample...
# # s3 <- rnorm(100)
# # r3 <- Param_bootstrap_poi(s3)
# 
# # Test 4
# s4 <- rexp(100)
# r4 <- Param_bootstrap_poi(s4)

# Tests Exponential -------------------------------------------------------
# #Test 1
#e1 <- rexp(100, 1)
#res1 <- Param_bootstrap_expo(s1)


# #Test 2 
#e2 <- rexp(100, 4)
#res2 <- Param_bootstrap_expo(s2)
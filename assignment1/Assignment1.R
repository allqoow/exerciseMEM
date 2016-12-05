# Initialisation
remove(list=ls())
workDir = "C:/Users/user/OneDrive/2016Semester2_TUBerlin/Kurse/Microeconometrics/Assignments"
setwd(workDir)

# Task 1
# Custom functions for Task 1
countGreaterThan = function(seq, x){
  count = 0
  for (i in 1:length(estm_some_college)) {
    if (estm_some_college[i] > x){
      count = count+1
    }
  }
  return(count)
}

countSmallerThan = function(seq, x){
  count = 0
  for (i in 1:length(estm_some_college)) {
    if (estm_some_college[i] < x){
      count = count+1
    }
  }
  return(count)
}
# Importing required pacakges
install.packages("sandwich")
install.packages("foreign") 
library(sandwich)
library(foreign) 

# Importing the data
data<-read.dta("nels.dta") 

# Managing the data
data$famincsqr= data$faminc**2

# Linear Probability Model
lpm = lm(data$some_college~data$hscath+data$hsrural+data$grades+data$faminc+data$famincsqr+data$famsiz+data$asian+data$hispan+data$black+data$par_coll)
summary(lpm)
vcov(lpm)
vcovHC(lpm)

# Effect of median family income on the probabilty
(2.936e-03)*median(faminc)+(-9.855e-06)*(median(faminc)**2)

# Plot for faminc
min = min(faminc)
max = max(faminc)
curve((2.936e-03)+2*(-9.855e-06)*x, from=min, to=max, xlab='faminc',ylab='Marginal Effect')

# Plot for grades
min = min(grades)
max = max(grades)
curve(-6.370e-02+(x*0), from=min, to=max, xlab='grades',ylab='Marginal Effect')

# Estimates bigger than 1/less than 0
estm_some_college = 1.034e+00+(6.205e-02)*hscath+(-3.236e-02)*hsrural+(-6.370e-02)*grades+(2.936e-03)*faminc+(-9.855e-06)*famincsqr+(-1.612e-02)*famsiz+(4.297e-02)*asian+(8.537e-02)*hispan+(8.604e-02)*black+(1.449e-01)*par_coll
countGreaterThan(estm_some_college, 1)
countSmallerThan(estm_some_college, 0)



# Task 2
logitFunction = function(scalar){
  return(exp(scalar)/(1+exp(scalar)))
}

# logit
logitModel = glm(formula=data$some_college~data$hscath+data$hsrural+data$grades+data$faminc+data$famincsqr+data$famsiz+data$asian+data$hispan+data$black+data$par_coll, family=binomial(link = "logit"))
summary(logitModel)

vecCoeff = logitModel$coefficients[1:11]
vecMedian = c(1,median(data$hscath),median(data$hsrural),median(data$grades),median(data$faminc),median(data$famincsqr),median(data$famsiz),median(data$asian),median(data$hispan),median(data$black),median(data$par_coll))
vecMean = c(1,mean(data$hscath),mean(data$hsrural),mean(data$grades),mean(data$faminc),mean(data$famincsqr),mean(data$famsiz),mean(data$asian),mean(data$hispan),mean(data$black),mean(data$par_coll))

# DPE with median
ipMedCoeff = vecMedian%*%vecCoeff
deltaByMedian = 1*logitModel$coefficients[11]
estmDpeMedian = logitFunction(ipMedCoeff+deltaByMedian) - logitFunction(ipMedCoeff)
estmDpeMedian

# DPE with mean
ipMeanCoeff = vecMean%*%vecCoeff
deltaByMean = 1*logitModel$coefficients[11]
estmDpeMean = logitFunction(ipMeanCoeff+deltaByMean) - logitFunction(ipMeanCoeff)
estmDpeMean

# MPE with mean
estmMpeMean = logitFunction(ipMeanCoeff)*(1-logitFunction(ipMeanCoeff))*logitModel$coefficients[11]
estmMpeMean


# Custom functions for Task 2

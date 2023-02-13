# ROPT
Optimizing portfolio returns of a financial stocks data using genetic algorithm model in R

library(quantmod)

ENV.STOCK<- new.env()
getSymbols(c("BAYN.DE","ALV.DE","FRE.DE","SAP.DE","VNA.DE",
             "IFX.DE","LIN.DE","BAS.DE","DPW.DE","HEI.DE"),
             src="yahoo",
             from= as.Date("2017-1-1"), 
             to = as.Date("2020-6-6"),
             peridiocity = "daily", 
             env = ENV.STOCK)

XTS.ADJUSTED<- do.call(merge,eapply(ENV.STOCK,Ad))
View(XTS.ADJUSTED)
#RETURNS
XTS.LOGRETURNS<- diff(log(XTS.ADJUSTED), lag=1)
XTS.LOGRETURNS <- na.omit(XTS.LOGRETURNS)
View(XTS.LOGRETURNS)

# Define the objective function
fitfunc <-  function(XTS.ADJUSTED) {
  port_returns <- sum(XTS.ADJUSTED * XTS.LOGRETURNS)
  return(-mean(port_returns))
  # The minus sign is used because the GA package optimizes the minimization of a function
}


# Define the constraints
#constrFun <- function(weights){
  #sum_weights <- sum(weights)
  #c(sum_weights - 1, -weights)
#}

# Defining the bounds
bounds <- rbind(rep(0, 10), rep(1, 10))

# Running the genetic algorithm
ga_output <- ga(type="real-valued", 
                fitness=fitfunc, 
                lower=bounds[1,],
                upper= bounds[2,],
                maxiter=1000, 
                run=100)

result <- as.vector(summary(ga_output)$solution)
bestportfolio <- cbind(names(XTS.LOGRETURNS),result)

sum(result)

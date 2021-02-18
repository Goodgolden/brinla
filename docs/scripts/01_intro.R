## library ---------------------------------------------
library(INLA)
library(brinla)
library(tidyverse)
inla.prune()


## quick start ---------------------------------------------
### lm model ---------------------------------------------
data(hubble, 
     package = "brinla")
# View(hubble)

plot(y ~ x, 
     xlab = "Distance(Mpc)", 
     ylab = "Velocity(km/s)", 
     data = hubble)

lmod <- lm(y ~ x - 1, 
           data = hubble)

coef(lmod)
summary(lmod)

hubtoage <- function(x) {
  3.09e+19/(x * 60^2 * 24 * 365.25 * 1e+09)}

hubtoage(coef(lmod))
(bci <- confint(lmod))
hubtoage(bci)

### inla model ---------------------------------------------
#### imod1: prec = 1e-09---------------------------------------
imod1 <- inla(formula = y ~ x - 1, 
             family = "gaussian", 
             quantiles=c(0.025, 0.5, 0.975),
             control.fixed = list(
               ## control.fixed=list(
               ## prec=list(a=1, b=2, default=0.01))
               ## assign 'prec=1' to fixed effect 'a' , 
               ## 'prec=2' to effect 'b' and
               ## 'prec=0.01' to all others. 
               ## (default 0.001)
               ## prec.intercept	
               ## default precision the intercept
               prec = 1e-09), 
             data = hubble)
?inla
?control.fixed
# View(imod)
# summary(imod)
# str(imod)

ibci1 <- imod1$summary.fixed
## estimate and ci
ibci1; ibci1[c(3,5)]

plot(imod1$marginals.fixed$x, 
     type = "l", 
     xlab = "beta", 
     ylab = "density", 
    xlim = c(60, 100))
abline(v = ibci1[c(3, 5)], 
       lty = 2,
       col = "red")

## [1]mean 
## [3]0.025quant 
## [4]0.5quant 
## [5]0.975quant     
## [6]mode
hubtoage(ibci1[c(1, 3, 4, 5, 6)])

## transforms the marginal (inla.tmarginal)
## marginal{INLA} operates on marginals
ageden <- inla.tmarginal(
  ## compute the expectation against, 
  ## or which define the transformation 
  ## new = fun(old)
  fun = hubtoage, 
  ## marginal object from either inla or inla.hyperpar(), 
  ## which is either list(x=c(), y=c()) 
  ## with density values y at locations x, 
  ## or a matrix(,n,2) for which 
  ## the density values are the second column and 
  ## the locations in the first column.
  marginal = imod1$marginals.fixed$x)

plot(ageden, 
     type = "l", 
     xlab = "Age in billions of years", 
     ylab = "density")
abline(v = hubtoage(ibci1[c(3, 5)]), 
       lty = 2,
       col = "red")


#### imod2: prec = 1/(12^2) ------------------------------------------
imod2 <- inla(y ~ x - 1, 
             family = "gaussian", 
             control.fixed = list(
               mean = 65,
               prec = 1/(12^2)), 
             data = hubble)

(ibci2 <- imod2$summary.fixed)
ibci2; ibci2[c(3,5)]

## [1]mean 
## [3]0.025quant 
## [4]0.5quant 
## [5]0.975quant     
## [6]mode
hubtoage(ibci2[c(1, 3, 4, 5, 6)])

#### imod3: prec = 1/((0.05 * uhub)^2) -----------------------------------
(uhub3 <- hubtoage((2016 + 4004 - 1)/1e+09))
imod3 <- inla(y ~ x - 1, 
             family = "gaussian", 
             control.fixed = list(
               mean = uhub3, 
               prec = 1/((0.05 * uhub3)^2)),
             data = hubble)
(ibci3 <- imod3$summary.fixed)
## [1]mean 
## [3]0.025quant 
## [4]0.5quant 
## [5]0.975quant     
## [6]mode
hubtoage(ibci3[c(1, 3, 4, 5, 6)])

## bayes theory ---------------------------------------


## prior and posterior distributions ---------------------------------------


## model checking ----------------------------------------------


## model selection ----------------------------------------------


## hypothesis testing --------------------------------------------------


## bayesian computation -------------------------------------------------

sessionInfo()

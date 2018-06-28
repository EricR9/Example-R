# Author:  Eric Rannenberg
# Reference:  https://www.youtube.com/watch?v=_nhgHjdLE-I&t=1s
# Reference:  R in Action, Robert Kabacoff 2nd ed, p. 291

# This code demonstrates various uses of the bootstrap (resampling) technique.
# Useful for calculating standard error, confidence intervals, hypothesis tests, predictions

# The number of iterations can (perhaps should) be increased until the results don't change
# as more iterations are added.

# Example 1 - Manual method of assessing a resampled dist vs. mult new samples -------------
set.seed(333)
x <- rnorm(30) # This is sampling from random dist, in practice use the origional data
n_sample <- 1000 # Using 1000 to make the code work, otherwise use length of response

bootMean <- rep(NA, n_sample) # Creates a blank vector with NAs ()
sampledMean <- rep(NA, n_sample)

for (i in 1:length(bootMean)){
    bootMean[i] <- mean(sample(x, replace=TRUE)) # Samples x, n_sample times
}
for (i in 1:length(sampledMean)){
    sampledMean[i] <- mean(rnorm(30)) # Samples from a rand dist, n_sample times
}

plot(density (bootMean))
lines(density(sampledMean), col="red") 

# Example 1 Continued - Using the boot package
meanFunc <- function (x,i){
    mean(x[i])
}

library(boot)
# Needs three arguments: data, function to calc in each subsample, & num of resamples 
bootMean <- boot(data=x, statistic=meanFunc, R=1000); bootMean  

plot(density(bootMean$t)) # t is the bootstrap values
lines(density(sampledMean), col="red")


# Use bootstrap to create confidence intervals without any parametric assumptions ---------
data(nuclear) # Dataset contained within library (boot)

# Single plot of just the data
nuke.lm <- lm(log(cost) ~ date, data=nuclear) #Example using log of cost as response
plot(nuclear$date, log(nuclear$cost), pch=19)
abline(nuke.lm, col="red", lwd=2)
# Example 2:  Three manual resamples 
par(mfrow=c(1,3))
for(i in 1:3){      #Sample from all the rows WITH replacement
    nuclear0 <- nuclear[sample(dim(nuclear)[1], replace=TRUE),]
    nuke.lm0 <- lm(log(cost) ~ date, data=nuclear0)
    plot(nuclear0$date, log(nuclear0$cost), pch=19)
    abline(nuke.lm0, col="red", lwd=2)
}
par(mfrow=c(1,1))

# Example 2 continued using boot function:  
bs <- function (data, index, formula){
    d <- data[index,]
    fit <- lm (formula, data=d)
    return (coef(fit))
}


# Two functions working together to create "R" models and extracting the coefficients
# Data, statistic, and R are same as before, need boot to pass formula to bs function also
results <- boot(data=nuclear, statistic=bs, R=1000, formula=log(cost)~date)

plot(density(results$t[,2]), col="red", lwd=2)
lines(rep(nuke.lm$coefficients[2],10), seq(0,8, length=10), col="blue", lwd=3)

boot.ci(results) # This only shows CI of intercept

confint(nuke.lm) # This is the parametric CI, shows intercept and slope

# **see "An Introduction to Statistical Learning", James et. all, p. 196 to do it right**


# Hypothesis testing with bootstrap -----------------------------------------------
resid <- rstudent(nuke.lm) # Studentized residuals
fit0 <- fitted(lm(log(cost) ~ 1, data=nuclear)) # Fit only the intercept term
newNuc <- cbind(nuclear, resid=resid, fit0=fit0)

bs <- function(data, index){ # Estimates if there was no relationship between cost/date
    coef(glm(data$fit0 + data$resid[index] ~ data$date, data=data))
}
results <- boot(data=newNuc, statistic=bs, R=1000)

plot(density(results$t[,2]), lwd=2, col="blue") # plot of no relationship cost/date
lines(rep(coef(nuke.lm)[2],10), seq(0,3,length=10), col="red", lwd=2) # real value

# computing an empirical p-value
B <- dim(results$t)[1]
(1 + sum((abs(results$t[,2]) > abs(coef(nuke.lm)))))/(B+1) # Standard formula-one sided











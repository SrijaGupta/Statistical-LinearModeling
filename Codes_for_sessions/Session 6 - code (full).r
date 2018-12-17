#====================================
# Statistics for Managers, Session 6
#====================================
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================

# Measures of probability distributions 
#= 

# Car loans example (discrete variable)
#-
# Expected value:
# x is a random discrete variable, that has the following values; p represents the probabilities of each value 
x <- seq(from=0, to=4, by=1) 
p <- c(0.1, 0.2, 0.45, 0.15, 0.1) 
# Let's plot the two. Note type ="h" which gives us bars to what otherwise would have been a scatterplot. 
plot(p~x, type="h", main="Probability of car loans per hour", col="steelblue", lwd=4, ylab="P(loans)", xlab="") 
# expected value = mean 
expected_value <- sum(x*p) 
# variance = we already know what this is from the descriptives class! 
variance <- sum((x-expected_value)^2 * p)
# and we already know what standard deviation is  
standard_deviation <- sqrt(variance)

# So...  
expected_value
variance
standard_deviation
# We can now compare against probabilities in this and other dealerships, different hours/days, etc. 

#=====
#Solutions 
#=
f <- c(9, 10, 22, 8, 1)
p <- f/50 
# just to confirm: 
sum(p)
x <- seq(from=0, to=4, by=1) 
plot(p~x, type="h", main="Probability of walk-ins per hour", col="steelblue", lwd=4, ylab="P(walk-ins)", xlab="") 
mean_walkins <- sum(x*p) 
sd_walkins <- sqrt(sum((x-expected_value)^2*p))
mean_walkins
sd_walkins
#=====

# Normal distribution (the most common case of continuous variable)
#=
netrev <- rnorm(mean=1500, sd=300, 100000)
newcustomers <- rnorm(mean=21, sd=5, 100000)
par(mfrow=c(1,2))
hist(netrev, main="Net revenue per weekly sales in USD", col="steelblue", xlab="Mean=1500, SD=300", freq=F)
lines(density(netrev), col="yellow", lwd=3) 
hist(newcustomers, main="New customers gained per week", col="darkred", xlab="Mean=21, SD=5", freq=F)
lines(density(newcustomers), col="gray", lwd=3) 

# Calculating percentiles, given a normal distribution 
#- 
pnorm(1800, mean=1500, sd=300) # Abbey's in the 84th percentile of sales 
pnorm(24, mean=21, sd=5) # Ben's in the 72th percentile of sales 

# What if we need to get percentiles to several sales people? No problem. 
pnorm(c(1800, 900, 2210), mean=1500, sd=300)

# But what if we want to find the value of the nth percentile? Also no problem. -- This one's for Abbey 
round(qnorm(.841, mean=1500, sd=300),0)

# What's the 95% of sales -- these guys get a bonus! 
qnorm(.95, mean=1500, sd=300)

#=====
#Solutions 
#=
toolow<- pnorm(35.8, mean=36, sd=.11)
toohigh <- 1-pnorm(36.2, mean=36, sd=.11)
1-(toolow+toohigh)
#=====


#### Additional notes 
#=
# To review and practice main propability concepts, use the 
# OpenIntro Statistics textbook, Chapter 3 (up to 3.3), and practice with 3.6 up to 3.6.4. 



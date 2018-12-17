#====================================
# Statistics for Managers, Session 9
#====================================
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================

# Load libraries and data 
#==========================
library(readr)
library(psych)
load("session9.Rdata")
# Sampling and confidence intervals 
#=
describe(donors$donation_total)
tot<-donors$donation_total 
hist(tot) # Can't see much here
hist(tot, breaks=10) # Still unclear 
hist(tot, breaks=100) # Vast majority of very small donations and a few relatively large donations. 
hist(tot, breaks=100, xlim=c(0,1000)) # Even better. Still, this is an extremely right-skewed variable. 

# Sampling 
#=
# But we rarely have the entire population! Instead, let's try to "estimate" donations using a sample of n=30  
set.seed(123)
n <- 30
tot_samp <- sample(tot, n)

mean(tot_samp)
mean(tot)-mean(tot_samp) #-18.07; Not good, but because of the skewness, we really care about the median 
median(tot)-median(tot_samp) # 5; Not bad! 
# As a reminder
length(tot)
100*length(tot_samp)/length(tot) # only 1.5% 

#=====
#Solutions 
#=
plot(density(realestate$price))
set.seed(123)
price_samp <- sample(realestate$price, 30)
plot(density(price_samp))
mean(realestate$price) - mean(price_samp)
median(realestate$price) - median(price_samp)
sd(realestate$price) - sd(price_samp)

# Increasing the sample size
#==========================
n <- 150
set.seed(123)
tot_samp <- sample(tot, n)
mean(tot)-mean(tot_samp) # A bit better: 12.03 as opposed to -18.07 with n=30. But again, we care more about the median. 
median(tot)-median(tot_samp) # 0! 
sd(tot)-sd(tot_samp) # 56.90 Getting closer. 

#=====
#Solutions 
#=
set.seed(123)
n <- 100
price_samp1 <- sample(realestate$price, n)
mean(realestate$price) - mean(price_samp1)
median(realestate$price) - median(price_samp1)
sd(realestate$price) - sd(price_samp1)


# More samples 
#==========================
# Initialize a vector for each parameter 
n <- 30 
sample_means <- rep(NA, 100)
sample_medians <- rep(NA, 100)
sample_sds <- rep(NA, 100)

# Run the for loop
for (i in 1:100) {
  s<-sample(tot, n)
  sample_means[i]<-mean(s)
  sample_medians[i]<-median(s)
  sample_sds[i]<-sd(s)
  }
 
mean(tot)-mean(sample_means) 
median(tot)-mean(sample_medians) 
sd(tot)-mean(sample_sds) 
 
# Again, but with more samples  
sample_means <- rep(NA, 10000)
sample_medians <- rep(NA, 10000)
sample_sds <- rep(NA, 10000)

for (i in 1:10000) {
  s<-sample(tot, n)
  sample_means[i]<-mean(s)
  sample_medians[i]<-median(s)
  sample_sds[i]<-sd(s)
  }

mean(tot)-mean(sample_means) 
median(tot)-mean(sample_medians) 
sd(tot)-mean(sample_sds) # Not as good. More samples don't seem to matter. 

# Histograms of sample parameters 
par(mfrow=c(1,3))
hist(sample_means); hist(sample_medians); hist(sample_sds) # Not really a normal distribution. This is because we have extreme values. Increasing n will take care of this 
dev.off()
n <- 150
sample_means <- rep(NA, 10000)

for (i in 1:10000) {
  s<-sample(tot, n)
  sample_means[i]<-mean(s)
  }

hist(sample_means) # Much closer to a normal distribution, even for an extremely skewed distribution as donation total.  


#=====
#Solutions 
#=
set.seed(123)
n <- 100
sample_means_re <- rep(NA, 1000)
sample_medians_re <- rep(NA, 1000)
sample_sds_re <- rep(NA, 1000)

for (i in 1:1000) {
  s<-sample(realestate$price, n)
  sample_means_re[i]<-mean(s)
  sample_medians_re[i]<-median(s)
  sample_sds_re[i]<-sd(s)
}

mean(realestate$price)-mean(sample_means_re) 
median(realestate$price)-mean(sample_medians_re) 
sd(realestate$price)-mean(sample_sds_re) 


# Confidence Interval 
#==========================
# What is the a typical donation?  
tot<-donors$donation_total

# We start by taking a sample of 10%, and computing the SE 
n <- 0.1 * length(tot)
samp <- sample(tot, n)
mean(samp)-mean(tot)# not a big difference, but donation amounts are small 
median(samp)-median(tot) # 0 ; Wow! But this is a large sample size (10%), and, 
# not considering a few outliers, there isn't much variability. 

#Now with a more "reasonable" size, given the distribution of the data: 
n <- 200
samp <- sample(tot, n)
# Now compute 95% CI 
se <- sd(samp)/sqrt(n)  
lbound <- mean(samp) - 1.96*se
ubound <- mean(samp) + 1.96*se
c(lbound, ubound) 

# Now let's try it iteratively  
sample_means <- rep(NA, 200) # We only have 2000 observations, and we cannot exceed 10% 
sample_sds <- rep(NA, 200)
n <- 200

# For loop goes here:
for (i in 1:n) {
s <- sample(tot,n)
sample_means[i]<-mean(s)
sample_sds[i]<-sd(s)
}

lbound <- sample_means - 1.96 * (sample_sds/sqrt(n))
ubound <- sample_means + 1.96 * (sample_sds/sqrt(n))


# How about we plot it? 
plot(c(lbound, ubound)) # Not very informative. 
# Let's try the custom plot_ci function 
source(plot_ci.R) # This is how we load/run the content of a script in R 
tot_mean <- mean(tot)
plot_ci(lbound, ubound, tot_mean) # Good! 

#=====

# Hypothesis testing
#==========================
x <- seq(from=0, to=30, by=0.1)
demo_null<-t.test(x, alternative="two.sided", mu=14.5, conf.int=0.95)
demo_null
# We fail to reject the null hypothesis 
# Notice that we use t.test, which uses a t-distribution. 
# t-distibution is very similar to z-distribution, but is a little wider ("flatter"). 
# We use t-distribution to evaluation population mean, as opposed to inidivudal values. 

# Let's use a real example 
# Cars models earlier than 2010 will have the same mean price as cars 2010 and after

cars$agecat[cars$year<2010]<-"oldies" 
cars$agecat[cars$year>=2010]<-"newies" 
mean(cars$price[cars$agecat=="oldies"])

# So, the null hypothesis is that mu=11196.07 for newies
price_null <- t.test(cars$price[cars$agecat=="newies"], alternative="greater", mu=11196.07, conf.level=0.95)
price_null
# We reject the null. 
# Actually, the better (and easier) way to do it is to compare 
#samples from both populations: 
price_hypothesis_2samp <- t.test(cars$price[cars$agecat=="newies"], cars$price[cars$agecat=="oldies"])
price_hypothesis_2samp

# Note that we can access every bit of result directly: 
names(price_hypothesis_2samp) # shows various result and function parameter options, e.g., 
price_hypothesis_2samp$p.value 
# Or 
price_hypothesis_2samp[["p.value"]] 

# Let's plot the difference 
hist(cars$price[cars$agecat=="oldies"], main="Testing price differences")
hist(cars$price[cars$agecat=="newies"], col="steelblue", add=TRUE)
legend("topleft", c("oldies", "newies"), col=c("white", "steelblue"), pch=c(19,19), cex=0.8)

#=====
#Solutions 
#=
#e.g., 
realestate$storiescat[realestate$stories==1]<-"One story"
realestate$storiescat[realestate$stories>1]<-"More than one story"

mean(realestate$price[realestate$storiescat=="More than one story"]) #276636

stories_null <- t.test(realestate$price[realestate$storiescat=="One story"], alternative="less", 
                       mu=276636, conf.level=0.9999)

stories_null

#***************************************************
# Additional options for highly skewed data (none of which are perfect)
#-

# Option 1: Drop outliers 
plot(density(tot[tot<100])) # Not quite normal, but let's see if this works. 
length(tot[tot<100]) #1617
tot_trim<- tot[tot<100]
samp <- sample(tot_trim, n)
# Now compute 95% CI 
se <- sd(samp)/sqrt(n)  
lbound <- mean(samp) - 1.96*se
ubound <- mean(samp) + 1.96*se
c(lbound, ubound) # Still not great. 

# Option 2: Use non-parametric bootstrap method 
library(simpleboot)
library(boot)
bt_mean<-one.boot(tot, mean, R=2000, trim=0.2) # This is  s l o w  
boot.ci(bt_mean, type=c("perc", "bca")) # 95%   Not very good. 

# How about trying CI for a median? 
bt_median<-one.boot(tot, FUN="median", R=1000) 
boot.ci(bt_median, type=c("perc", "bca")) # Note the error message! This is the best we can do with such a variable without heavy "trimming", or transformations -- we will talk about this later in the course. 

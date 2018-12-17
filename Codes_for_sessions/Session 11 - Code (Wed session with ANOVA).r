#====================================
# Statistics for Managers, Session 11
#====================================
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================

## Load libraries and data 
load("session11data.Rdata") # training, cars, realestate 
library(corrplot)
library(psych)

# ANOVA 
#=

# Let's start by summarizing the data by group 
aggregate(emp_train$productivity ~emp_train$training, FUN="mean") 
emp_train %>% group_by(training) %>% summarize(avg = mean(productivity), std = sd(productivity)) 

# Let's view the data using a couple of plots 
boxplot(productivity~training, data=emp_train, col=2:4, xlab="Treatment Group") # We get a pretty good picture of "treatment effects"
  # Plots means also provides confidence intervals! 
plotmeans(emp_train$productivity~emp_train$training, xlab="Training", ylab="Productivity", lwd=3, col="red", p=0.99)

# Now for ANOVA 
emp_train.aov <- aov(productivity~training, data=emp_train)
emp_train.aov
summary(emp_train.aov) # Null rejected.

# But we don't know which training level is best. 
# We use Tukey pairwise comparisons
emp_train.tk<-TukeyHSD(emp_train.aov)
round(emp_train.tk$training,3)

# verify ANOVA assumptions
dev.off()
par(mfrow=c(2,2))
plot(emp_train.aov) # We have a few "outliers"
# Normal distribution assumption 
dev.off()
 
##============
## Solutions  
##============ 
# one-way ANOVA
# ===============
sales %>% group_by(calls) %>% summarize(avg = mean(actual_sales), std = sd(actual_sales)) 
boxplot(actual_sales~calls, data=sales, col=2:7, xlab="Number of Calls") # We get a pretty good picture of "treatment effects"
plotmeans(sales$actual_sales~sales$calls, xlab="Calls", ylab="Sales", lwd=3, col="red", p=0.99)
sales.aov <- aov(actual_sales~calls, data=sales)
sales.aov
summary(sales.aov) # Null rejected. But what number of calls? 

# But we don't know what number of calls, so we use Tukey pairwise comparisons
TukeyHSD(sales.aov) # Oops. We need to convert this into factor 
sales$calls <- as.factor(sales$calls)
sales.aov <- aov(actual_sales~calls, data=sales)
sales.tk <- TukeyHSD(sales.aov)
round(sales.tk$calls,2)

## Simple regression  
#=
## Simple linear regression 
#============================
# Our DV is price
# Let's correlate numeric vars (in this case only the DV and two IVs: mileage, year)
carsnum <- cars[,c(1,3,4)]
cormat <- cor(carsnum)
corrplot(cormat, addCoef.col = "gray")

# Both IVs exhibit a relationship 

# Now we can model (regress) price given mileage 
mod1<-lm(price ~ mileage, data=cars)
mod1
summary(mod1) 
confint(mod1)

# What about price given year? 
mod2<-lm(price ~ year, data=cars)
mod2
summary(mod2) 
	# we can also set confidence interval at 99% 
confint(mod2, level=0.99)

# To predict specific values based on a model
round(coefficients(mod1),3) 

round(coefficients(mod2),3) 

# For example, the predicted price of a car with 100k miles: 17091.52+(-0.093)*mileage. So, a car with 100,000 = 7791.52
predict(mod1 , data.frame(mileage =(c(5000 ,10000 ,100000) )), interval ="confidence", level=0.95) # the difference is due to rounding in the line above

# For example, the predicted price of a car from year 2002,2005,2008: -2395092+(1199)*year. So, a car from 2002 = 5306
predict(mod2 , data.frame(year =(c(2002 ,2005 ,2008) )), interval ="confidence", level=0.95) # the difference is due to rounding in the line above

# How well did our model perform? 
head(cars$price)-head(predict(mod1)) # or using this function 
summary(residuals(mod1))
plot(cars$price~cars$mileage, pch=16, col="lightblue")
abline(mod1, col="red", lwd=3) # and this is what we've been plotting all along! 

# R-sqrd
summary(mod1)$adj.r.squared # The simple (single IV) model explains about 65% of the variance 

# Diagnostics 
par(mfrow=c(2,2)) # sets up the canvas to 4 plots, so that we don't need to go through them one by one 
plot(mod1)

# or 
dev.off()
plot( predict (mod1), residuals (mod1)) # we are looking for a "no pattern"/non-linearity 
# finding outliers can be done this way as well
plot(hatvalues(mod1)) 
identify(hatvalues(mod1), col="red")
# or because it looks like we have 2 outliers, we can do this 
tail(sort(hatvalues(mod1)), n=2)

outliers <- c(90, 149)
cars1<-cars[-outliers,]
mod1.1<-lm(price ~ mileage, data=cars1) 
summary(mod1.1); summary(mod1) # Yes, we can see some improvement 

########################
re_mod1 <- lm(price ~ lotsize, data=realestate)
# The predicted price for lot 500, 1000, 2000, at 99% confidence 
predict(re_mod1 , data.frame(lotsize=(c(500 ,1000 ,2000) )), interval ="confidence", level=0.99) 
# Adjusted R-sqrd is: 
summary(re_mod1)$adj.r.squared 
# Outliers 
tail(sort(hatvalues(re_mod1)), n=5)
# regress without 5 most "extreme" outliers 
realestate1 <- realestate[-c(491,366,383,365,369)]
re_mod2 <- lm(price ~ lotsize, data=realestate1) # Adjusted R-sqrd hasn't really changed because the data are heavily skewed 
summary(re_mod2)$adj.r.squared 
# regress with logged DV and IV 
re_mod3 <- lm(log(price) ~ log(lotsize), data=realestate) # Adjusted R-sqrd hasn't really changed because the data are heavily skewed 
summary(re_mod3)$adj.r.squared 
# [1] 0.3257167  # Yes, this is an improvement
########################

## NOTES 

# If you consider more than a single factor (group), perform a two/three/N-way ANOVA test (MANOVA) 
# When the data are skewed and you are testing a single factor use the non-parametric Kruskal–Wallis test (kruskal.test) 
# When the data are skewed and you are testing multiple factors use the non-parametric Friedman test (friedman.test) 

# For more detailed information on ANOVA, Chi-square test, and bi-variate correlation, Read Ch. 5-7 in the OpenIntro book. 


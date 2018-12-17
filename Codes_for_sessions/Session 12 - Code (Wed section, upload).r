#====================================
# Statistics for Managers, Session 12
#====================================
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================

## Multiple regression (recap)  
#= 
# It is the same syntax as simple linear regression. 
# Only add (with a plus sign) other IVs. 
# Let's start with one more numeric IV. 
# create an age variable from the year variable (because it is easier to interpret!) 
table(cars$year)
cars$age <- as.numeric(abs(cars$year-max(cars$year)))
table(cars$age)
plot(density(cars$age)) 
mod2 <- lm(price ~ mileage+age, data=cars)
summary(mod2)

###########################################################################
# Using the realestate data, build a regression model for price using lotsize
# Then, build a model with lotsize and bedrooms 
# Interpret the results 
###########################################################################

# Dummy variables (recap) 
#- 
# What about factors? 
table(cars$transmission); table(cars$model); table(cars$color) # color has too few observations in some categories
# We'll just add transmission and model 
mod3 <- lm(price ~ mileage+age+transmission+model, data=cars) # color does not explain much. we'd better drop it 
round(coef(mod3),2)
# Notice the reference categories! 
levels(cars$model); levels(cars$transmission)
cars$model <- relevel(cars$model, ref=2) 
summary(lm(price ~ mileage+age+transmission+model, data=cars))
summary(mod3) # drop transmission
mod4 <- lm(price ~ mileage+age+model, data=cars) # or update the model, like so:
mod4.1 <- update(mod3,.~.-transmission) 

summary(mod4)
summary(mod4)$adj.r.squared-summary(mod2)$adj.r.squared # about 4% improvement 

###########################################################################
# Using the realestate data, examine two factors that you hypothesize should affect price 
# Examine the variables at a univariate level 
# Examine the relationship of each variable and price 
# Build a regression model with the two factors 
# Interpret the results 
# Now add lotsize to the regression model. Did the model improve? By how much? 
###########################################################################

## Regression Diagnostics 
#=
plot(predict(mod4), residuals(mod4)) # we are looking for a "no pattern"/non-linearity 
# and more comprehensively, we simply run 
par(mfrow=c(2,2)) # sets up the canvas to 4 plots, so that we don't need to go through them one by one 
plot(mod4) # we have a few outliers
# we can also find outliers this way 
plot(hatvalues(mod4)) 
identify(hatvalues(mod4), col="red")
tail(sort(hatvalues(mod4)))
# finally, we can get values for cook's distance  
round(sort(cooks.distance(mod4)),2)
# 90 has a high value, and we saw 1 earlier as being an outlier 
outliers <- c(90, 1)
cars1<-cars[-outliers,]
mod5<-lm(price ~ mileage+age+model, data=cars1) 
summary(mod5); summary(mod4) 
round(coef(mod4),2); round(coef(mod4),2)
summary(mod5)$adj.r.squared-summary(mod4)$adj.r.squared # not bad for removing a couple of rows

## Multicollinearity
#-
# This is another stipulation of the regression model
# First, let's revisit the relationship between age and mileage
cor(cars1$age, cars1$mileage) # highly correlated
# A more precise analysis is using VIF (Variance Inflation Factor)
# It looks for multicollinearity in a model as a whole 
vif(mod5) 
sqrt(vif(mod5)) > 2 # if any variable is true, we would need to drop it 

##################################################################
# Perform regression diagnostics on your best realestate model 
# Which observations were found to be outliers? 
# After removing the outliers, did you model improve? By how much? 
# Did you find evidence of multicollinearity in this model? 
##################################################################

## Transformations 
#=
# Numeric transformations 
#-
# We can remove outliers, or try transforming a variable.  
# The two most common transformations are log and sqrt. 
plot(density(cars$price)) # a bit off
plot(density(log(cars$price))) 
plot(density(sqrt(cars$price))) # somewhat better 

# let's look at mileage 
plot(density(cars$mileage)) 
plot(density(log(cars$mileage))) 
plot(density(sqrt(cars$mileage))) # sqrt it is

# Now at age 
plot(density(cars$age))
plot(density(log(cars$age))) 
plot(density(sqrt(cars$age))) # sqrt it is again

# Now model with transformations 
mod6 <- lm(sqrt(price)~sqrt(mileage)+sqrt(age)+model, data=cars) 
summary(mod6)# notice the model dummy 
summary(mod6)$adj.r.squared-summary(mod5)$adj.r.squared 
 
##################################################################
# Are any of the numeric variables in the realestate data skewed? 
# If so, perform a "good" transformation to this/these variables
# Re-run the regression after transformation. What did you find? 
##################################################################

# Converting numeric variables to factors 
#- 
# What about odd variables like bathrooms? 
table(realesate$bathrms) # not great
plot(density(realesate$bathrms))
plot(density(log(realesate$bathrms)))
plot(density(sqrt(realesate$bathrms))) # nope 
# What can we do, then? 
# Convert into a factor! 
realesate$bathrms_f[realesate$bathrms==1]<-"one"
realesate$bathrms_f[realesate$bathrms>1]<-"more than one"

real_mod3 <- update(real_mod2,.~.-bathrms+bathrms_f) 
summary(real_mod3)$adj.r.squared-summary(real_mod2)$adj.r.squared 

# But actually, 
real_mod4 <- lm(log(price)~log(lotsize)+stories+driveway+recroom+fullbase+gashw+airco+garagepl+prefarea+bathrms_f, data=realestate) 
summary(real_mod4)$adj.r.squared-summary(real_mod2)$adj.r.squared 
summary(real_mod4)$adj.r.squared-summary(real_mod3)$adj.r.squared 

# Of course, we now need to re-run regression diagnostics
# So, plotting again, re-transforming if needed, removing outliers, checking for VIF. 
# It is an iterative process! 

#################################################################################
# This how all the above might apply to the coach or baseball salary data. 
# What is the best model that you have found? 
# Now, thinking about the variables in your final project, what is your modeling strategy? 
# Start applying it and be prepared to discuss it. 
#################################################################################


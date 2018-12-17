#====================================
# Statistics for Managers, Session 10
#====================================
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================

# Load data and packages (install if needed, especially gmodles and gplots)
#=
load("Session 10 data.Rdata")
library(car)
library(dplyr)
library(gmodels)
library(gplots)
library(psych)

# Hypothesis testing variations 
#=
# One-sample test
#-

# Paired-sample test
#-
dev.off() 
first_year <- rnorm(40, mean=10, sd=3)
second_year <- rnorm(40, mean=10, sd=3)
hist(first_year, main="Testing paired sample differences", col="lightgray")
hist(second_year, col="steelblue", add=TRUE)
legend("topleft", c("first year", "second year"), col=c("lightgray", "steelblue"), pch=c(19,19), cex=0.8)

t.test(first_year, second_year, paired=T)

# Hypothesis testing skewed data 
#=
milesnew <- cars$mileage[cars$agecat=="newies"]
milesold <- cars$mileage[cars$agecat=="oldies"]
par(mfrow=c(1,2))
hist(milesnew)
hist(milesold)

# When data are skewed, we use Wilcoxon Signed-Rank Test
# It is another test, which is more "robust". It does not assume normal distribution  
pairedskew <- wilcox.test(milesnew, milesold, alternative="less") # use paired=T for paired samples. 
options(scipen=999)
pairedskew

# Correlation 
#=
plot(cars$price~cars$mileage, pch=16, col="lightblue")
abline(lm(cars$price~cars$mileage), lwd=3) # Looks like a strong negative correlation

# Now, let's get a numeric value for the correlation 
cor(cars$price,cars$mileage, use="complete.obs", method="pearson") 
	# complete.obs is when we have NAs (missing data), which we don't in this case.
  # Other methods are: spearman and kendall

# What about the age of the car and price?
cor(cars$price,cars$year) # positive relationship, or negative relationship with age 

# And, now we are curious about the age of the car and mileage
cor(cars$year, cars$mileage) 

# To run all correlations in a dataframe we need to select all relevant numeric variables (columns). This is called a "Correlation matrix"
carsnum <- cars[,c(1,3:4)]
cormat <- cor(carsnum) # Select only numeric variables. Otherwise, you'd get an error message. 
round(cormat, 2) # Rounded to 2 decimals 

# We can now get scatterplots of all numeric variables
pairs(carsnum)
# Or use variable names -- easier to sort 
pairs(~price+mileage+year, data=cars)

# We can get more details using the following the scatter plot matrix from the car pacakge 
scatterplotMatrix(~price+mileage+year, data=cars, main="Correlations of Numeric Variables in the Cars Data")

# We can also use a cool plotting techniques to illuste the correlation matrix
# This is particulary useful when we have many numeric variables 
corrplot(cormat, method="circle")
corrplot(cormat, method="circle", addCoef.col="black") # With correlation values! Extremely useful when there are many variables.  
	
# We can also compare correlations by transmission type. There are various ways to do this. 
# We'll start with a simple one.  
stick <- cars[cars$transmission=="MANUAL",]
	sticknum <- stick[,c(1,3:4)]
auto <- cars[cars$transmission=="AUTO",]
	autonum <- auto[,c(1,3:4)]
cormat_stick <- round(cor(sticknum),2) 
cormat_auto <- round(cor(autonum),2) 

cormat_stick; cormat_auto

# And using the scatter plot matrix
scatterplotMatrix(~price+mileage+year|transmission, data=cars, main="Correlations of Numeric Variables by Transmission")

# We know which is a strong correlation, but which ones are significant? 
# We test the hypothesis that there is a relationship (null is no relationship)
cor.test(cars$price, cars$mileage)
cor.test(cars$price, cars$year)
cor.test(cars$year, cars$mileage)
	# Most correlation coefficients -0.3<=r<=0.3 will test significant 

##============

# Chi-squared test for contingency tables 
#=
# Car dealership example 

# At 95% confidence, are customers of a car dealerships evenly distributed throughout the week

observed<-c(108,98,102,93,111,116,87)
expected <- rep(sum(observed)/7, 7)
chi_sq <- sum((observed-expected)^2/expected)
chi_sq
# Why not use a built-in function? 
dealership <- as.table(c(108,98,102,93,111,116,87))
dimnames(dealership)<-list(weekdays = c("M", "T", "W", "Th", "F", "S", "Su"))
chisq_test<-chisq.test(dealership)
chisq_test
chisq_test$p.value

# And what if the observation changes, for example, on Tuesday and Wednesday? 
dealership <- as.table(c(108,81,132,93,111,116,87))
dimnames(dealership)<-list(weekdays = c("M", "T", "W", "Th", "F", "S", "Su"))
chisq_test<-chisq.test(dealership)
chisq_test
chisq_test$p.value
##============

# Animal shelter data 
table(dogshelter$AnimalType, dogshelter$OutcomeType)

# Is there a difference bewteen the outcome of dogs and cats
dogstab<- table(dogshelter$AnimalType, dogshelter$OutcomeType)
addmargins(dogstab,c(1,2))
pdogtab<-prop.table(dogstab)
round(pdogtab,2)
addmargins(round(pdogtab,2),c(1,2))
mosaicplot(pdogtab)
barplot(pdogtab, col=c("orange", "gray"))
legend("top", c("cats", "dogs"), lty=1,lwd=4, col=c("orange", "gray"))

# Now for the chi-square test 
chisq.test(dogstab)

# What if we reverse the rows and columns? 
dogstab1<- table(dogshelter$OutcomeType, dogshelter$AnimalType )
identical(chisq.test(dogstab1)$p.value, chisq.test(dogstab)$p.value)

# Let's examine another factor: pet's color 
table(dogshelter$Color)
table(dogshelter$Color=="Black/White") # a little over 10%
dogshelter$color_cat[dogshelter$Color=="Black/White"]<-"BW"
dogshelter$color_cat[dogshelter$Color!="Black/White"]<-"NotBW"
table(dogshelter$color_cat) # Looks good
col_tab<-table(dogshelter$color_cat, dogshelter$OutcomeType)
chisq.test(col_tab)

# Maybe we need to add another common color? Tricolor
dogshelter$color_cat[dogshelter$Color=="Black/White"|dogshelter$Color=="Tricolor"]<-"BWT"
dogshelter$color_cat[dogshelter$Color!="Black/White"&dogshelter$Color!="Tricolor"]<-"NotBWT"
col_tab<-table(dogshelter$color_cat, dogshelter$OutcomeType)
col_tab
chisq.test(col_tab)

# Let's make it look better
CrossTable(dogshelter$OutcomeType, dogshelter$color_cat, prop.c = F, prop.r = F, expected=T, 
dnn = c('actual outcome', 'expected outcome'), format=c("SPSS"), chisq = T)

##============

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
round(emp_train.tk$training,2)

# verify ANOVA assumptions
dev.off()
par(mfrow=c(2,2))
plot(emp_train.aov) # We have a few "outliers"
# Normal distribution assumption 
dev.off()
emp_train.fit <- lm(productivity~training, data=emp_train)
summary(emp_train.fit)
qqPlot(emp_train.fit, col="steelblue", pch=16, envelope=T, col.lines=palette()[1])
 
##============

## NOTES 

# If you consider more than a single factor (group), perform a two/three/N-way ANOVA test (MANOVA) 
# When the data are skewed and you are testing a single factor use the non-parametric Kruskal–Wallis test (kruskal.test) 
# When the data are skewed and you are testing multiple factors use the non-parametric Friedman test (friedman.test) 

# For more detailed information on ANOVA, Chi-square test, and bi-variate correlation, Read Ch. 5-7 in the OpenIntro book. 


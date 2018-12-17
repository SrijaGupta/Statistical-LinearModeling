#====================================
# Statistics for Managers, Session 3
#====================================
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================
setwd("C:/Users/zack/Desktop/Session 4")
# cars and real estate data, now with correct variable types 
load("Session 4 data.Rdata") 

# But always confirm
str(cars)
str(realestate) 

# Quick recap of last week plus a few more functions for descriptive statistics 

# Measures of spread/distribution: variance, and standard deviation 
#=
range(cars$mileage)
range(cars$mileage)[2] # It's a vector with min and max 
fivenum(cars$mileage) 
quantile(cars$mileage) # hmmm.... 
fivenum(cars$mileage[2:length(cars$mileage)]) 
quantile(cars$mileage[2:length(cars$mileage)]) # aha! 

IQR(cars$mileage) # This is simply q3-q1, so: 
55124.50-27200.25

# Variance (The average of the squared differences from the Mean)
varmil <- var(cars$mileage)
# Stadnard deviation (square root of variance) 
sqrt(varmil)
sd(cars$mileage) 

#===== Exercise =====# 

# Plotting numeric variables 
#=
hist(cars$price)
hist(cars$price, probability=TRUE)
hist(cars$price, breaks=20)
hist(cars$price, breaks=10, col=c("steelblue", "red"))
box()

# Now for mileage
hist(cars$mileage, col=c("steelblue", "red"))
box() 

# Now add rug plot (actual data elements)
rug(jitter(cars$mileage), col="darkgray")

# Another solution to the binning problem is to use a "smoother": the kernel density plot
den<-density(cars$mileage)
plot(den)
# or in one lines
plot(density(cars$mileage))

# Now combine all three plots together: 
hist(cars$mileage, col=c("steelblue", "red"), freq=F) # Note frequency = F
rug(jitter(cars$mileage), col="darkgray")
lines(density(cars$mileage), col="yellow", lwd=3) # Note how the lines function is used to overlay the density plot 
box() 

## End of review, now to where we left off. 

# Boxplot 
boxplot(cars$mileage, main="Used Car Mileage")
boxplot(cars$price, main="Used Car Prices", col="orange")
# Interpretation anyone? 

# Multiple plots in the same output 
dev.off()
par(mfrow=c(2,2)) 
hist(cars$mileage, col=c("steelblue", "red"), freq=F)
rug(jitter(cars$mileage), col="darkgray")
lines(density(cars$mileage), col="yellow", lwd=3)
box() 
hist(cars$price, col=c("steelblue", "red"), freq=F)
rug(jitter(cars$price), col="darkgray")
lines(density(cars$price), col="yellow", lwd=3)
box() 
boxplot(cars$mileage, main="Used Car Mileage", col="green")
boxplot(cars$price, main="Used Car Prices", col="orange")

# Save output into a pdf file
pdf("plots.pdf",width=7,height=5)
par(mfrow=c(2,2)) 
hist(cars$mileage, col=c("steelblue", "red"), freq=F)
rug(jitter(cars$mileage), col="darkgray")
lines(density(cars$mileage), col="yellow", lwd=3)
box() 
hist(cars$price, col=c("steelblue", "red"), freq=F)
rug(jitter(cars$price), col="darkgray")
lines(density(cars$price), col="yellow", lwd=3)
box() 
boxplot(cars$mileage, main="Used Car Mileage", col="green")
boxplot(cars$price, main="Used Car Prices", col="orange")
dev.off()

#===== Exercise =====# 

# Two factors 
#=
# Numerically, we use a two-way table 
emp<-read.csv("emp_train.csv", header=T)

# Is there a realationship between training and performance? 
tab <- table(emp$Training, emp$Performance_change)
tab # A bit hard to interpret 
ptab <- prop.table(tab)
ptab 
# Or for better labels
xtabs(~emp$Training+emp$Performance_change)
prop.table(xtabs(~emp$Training+emp$Performance_change))

# Graphically, we have various options
plot(tab) # Same as mosaicplot:
mosaicplot(tab, main="Performance Change After Training", col=c("lightgreen", "orange", "red"))
# Barplot
barplot(tab, col=c("coral", "greenyellow"),main="Performance Change After Training") 
# Now that we can see numeric information, we should also check the proportions of the table (percentages)
tab1<-prop.table(tab, margin=2) # Uses column margin (proportion); margin=1 uses row margins (proportion). 
barplot(tab1, col=c("coral", "greenyellow"), main="Performance Change After Training") 
# Still unclear. We need to add a legend!
legend("bottomright", 
    legend = c("No Training", "Training"), 
    fill = c("coral", "greenyellow"))	  # In this particular plot the bottom right area is probably best for adding a legend. 

#===== Exercise =====# 

# Two numeric variables
#=
# Simple! Let's go back to the used cars data
plot(cars$mileage,cars$price,  col="blue")
plot(cars$price~cars$mileage, col="red") # Note the differences! It may be better to use this format. It is the same format as modelling. 
identify(cars$price~cars$mileage, labels=rownames(cars))

# Now let's make this plot a little more understandable
plot(cars$price~cars$mileage, col="red", 
	main="Relationship of mileage to the price of used cars", 
	xlab="Mileage", 
	ylab="Price", 
	pch=16) 
abline(lm(cars$price~cars$mileage), col="darkgreen", lwd=2.5) # Regression line slope is distinct, and not too far from observations 
# We can also add a "smoother"
lines(lowess(cars$price~cars$mileage), col="steelblue", lwd=2.5) # Smoother is similar to regression line, indicating a linear relationship
lines(lowess(cars$price~cars$mileage, f=0.1), col="orange", lwd=2.5) # Overfitting?
lines(lowess(cars$price~cars$mileage, f=0.01), col="green", lwd=2.5) # Overfitting!

#===== Exercise =====# 

# Categorical and numeric variable  
#=
# Numeric stats
table(emp$Performance_num, emp$Gender) # Not great. We will explore aggregate stats of a numeric variable as they relate to a categorical variable. 

# Boxplots are a good example of this procedure -- they help show differences in categories as they relate to a numeric variable.  
boxplot(Performance_num ~ Gender, data=emp, main="Performance Change After Training by Gender", 
	xlab="Gender", ylab="Performance",
	col=c("orange", "lightblue4")) # Better! 
	
# Density plots, histograms etc. can also be used for the same purpose! 
m<-emp[emp$Gender=="Male",]
f<-emp[emp$Gender=="Female",]
plot(density(m$Age), col="red", lwd=2.5, main="Distribution of Age by Gender")
lines(density(f$Age), col="blue", lwd=2.5)

plot(density(m$Performance_num), col="red", lwd=2.5, main="Distribution of Performance Change by Gender")
lines(density(f$Performance_num), col="blue", lwd=2.5) # There is a more noticable difference. 

#===== Exercise =====# 


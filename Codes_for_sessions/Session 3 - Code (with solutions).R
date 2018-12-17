#====================================
# Statistics for Managers, Session 3
#====================================
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  


### Load data into R 
#--------------------
# We are going to examine two data sets: cars and realestate
# We can use RStudio GUI, or load it directly from R: 
setwd("C:/Users/zack/Desktop/Session 3")
cars <- read.csv(file.choose(), header=T) 

## Cars data
head(cars)
tail(cars)
dim(cars)
str(cars)
View(cars)

#==============================
# Download the two datasets found under today's folder in Blackboard. 
# Load realestate.csv as realestate data frame. 
# What are the dimensions of the realestate data? 
# How many numeric variables are in the realestate data and how many are factor?  
# Does any of these variables need to be converted into a different type? (e.g., numeric to factor, or factor to numeric) 
# Also load cars.csv as a cars dataframe. We will work on it later. 
#==============================

#Solutions
#=
realestate <- read.csv(file.choose(), header=T)  # I've already loaded the car data. 

# There are 500 observations and 12 variables
# Six variables are numeric, the rest are factors. 
# Not really, but it could be interesting to convert several of the numeric variables to factors. It depends on your reserach question and  analytical strategy.  

# Factors (categorical variables)  
#=
attach(cars)
summary(transmission)
table(transmission) # same? 
s<-summary(transmission)
t<-table(transmission)
identical(s,t) # No! Why?
class(s)
class(t) 

# Bar plot 
plot(transmission) # Oops 
barplot(t, main = "Bar Plot", xlab = "Transmission", ylab = "Frequency")
ptab<-prop.table(t)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Bar Plot", xlab = "Transmission", ylab = "Proportion")
barplot(ptab, main = "Bar Plot", xlab = "Transmission", ylab = "Proportion", col="steelblue")
barplot(ptab, main = "Bar Plot", xlab = "Transmission", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,95))
box()

# Horizontal bar plot
barplot(ptab, main = "Horizontal Bar Plot", xlab = "Proportion", ylab = "Transmission", col=c("orange", "steelblue"), xlim=c(0,95), horiz = TRUE)
box(lwd=2)

# Pie chart 
# Best option is to use the package plotrix (supports 3d)
install.packages('plotrix', repos="http://cran.r-project.org")
library(plotrix)
pie3D(table(cars$color), labels = levels(cars$color), explode = 0.1, main = "3D Pie Chart")
# Looks jagged. 
pdf("pie.pdf")
pie3D(table(cars$color), labels = levels(cars$color), explode = 0.1, main = "3D Pie Chart")
dev.off() 

# Nice! 

#Solutions 
#=
tre <- table(realestate$prefarea) # 372 no, 128 yes. 
prop.table(tre) # 74.4% not in a preferable location, 25.6% are 

barplot(table(realestate$driveway), col=c("orange", "darkgreen"), main="Distribution of Driveway in Real Estate Market")

rlow<-realestate[realestate$price<=350000,]
rhigh<-realestate[realestate$price>350000,]
prop.table(table(rlow$recroom))
prop.table(table(rhigh$recroom)) # About 10% differnece.  


# Normal distribution
#=
set.seed(123)
plot(density(rnorm(10)), xlim=c(-5, 5), ylim=c(0, 0.6), col="gray", lwd=3)
lines(density(rnorm(1000)), col="lightgreen", lwd=3)
lines(density(rnorm(100000)), col="steelblue", lwd=3)

# Numerical Variables 
#=
# First, make sure the the variable type is correct 
summary(mileage)
summary(as.numeric(mileage)) # Odd numbers. Need to convert it, but also take care of separator.  
cars$mileage1<-sub(",", "", cars$mileage)
summary(cars$mileage1) # One more step
cars$mileage1<-as.numeric(cars$mileage1)
summary(cars$mileage1) # It works
# Let's do this in one step, with another variable 
cars$year<-as.factor(sub(",", "", cars$year))
str(cars) # price needs to be converted. For now, we will leave date as is. 
cars$price<-as.numeric(sub(",", "", cars$price))
str(cars) # Better! 

# Now let's review different measures we have to examine the distribution of numeric variables 
min(cars$mileage)
max(cars$mileage)
mean(cars$mileage) 

# What is I want to see these stats for several variables at the same time? 
# There are different functions and packages to summarize data. For now, let's assign numeric vars to a separate data frame. 
carsnum<-cars[,c("price", "mileage")]
detach(cars)
attach(carsnum)
sapply(carsnum, FUN=mean)
sapply(carsnum, FUN=mean, trim=0.1) # One way to handle possible extreme values ("outliers"), trim top and bottom values, here set at 10% 
sapply(carsnum, FUN=median) # Median is another way to address outliers 
sapply(carsnum, FUN=quantile) # Break it down 
sapply(carsnum, FUN=quantile, probs=c(0,.33,.66, 1))
sapply(carsnum, FUN=quantile, probs=seq(from=0,to=.5, by=0.05))
# IQR or interqunrtile range, is also useful. We can easily compute it: 
quantile(mileage)[4]-quantile(mileage)[2] # Or
IQR(mileage)

# What about mode? 
mode(mileage) 

# The mode function is equivalent to the class() function. Not what we wanted. 

our_mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}
our_mode(price) # Mileage has only unique values 
table(price)


#Solutions 
#=
range(realestate$price) # 96250 731500
mean(realestate$price)-median(realestate$price) # 24473.23
rvhigh<-realestate[realestate$price>400000,]
quantile(rvhigh$price, probs=c(0.4, 0.97)) # 466312 673750 
#=

# Measures of spread: variance, and standard deviation 
range(mileage)
range(mileage)[2] # It's a vector with min and max 
identical(range(mileage)[2], max(mileage)) # True 
# Variance (The average of the squared differences from the Mean)
var(mileage)
# Stadnard deviation (square root of variance) 
std(mileage) # Doesn't work. Let's compute "by hand" 
varmil<-sum((mileage-mean(mileage))^2 /(length(mileage)-1))
sqrt(varmil)

sd(mileage) 

# One last function, MAD (median average deviation). Finds the median of absolute difference from the median, then multipled by a constant (1.4826). Why this constant? Because it makes it comparable with the sd for normal distribution. 
median(abs(price - median(price))) * 1.4826

# And R's function 
mad(price) 

# Standard error (R doesn't have a built-in function, but various packages do). Let's compute again "by hand"
n <- length(price)
s <- sd(price)
s/sqrt(n)
# Expressed as an R function that we create 
se <- function(x) sd(x)/sqrt(length(x)) # Let's try it: 
sapply(carsnum, FUN=se)
	
# Finally measures of skewness 
# Skew 
m <- mean(price)
sum((price-m)^3/s^3)/n 
# or simply
skew(price)

# Kurtosis
sum((price-m)^4/s^4)/n - 3
# or simply 
kurtosi(price)

#Solutions 
#=
realnum <- realestate[,c("lotsize", "price", "bedrooms")]

spread.funs <- function(x) {
      c(mean = mean(x), mean_trim = mean(x, trim=0.1), median = median(x), var = var(x), sd = sd(x), mad = mad(x), skew = skew(x), kurtosis = kurtosi(x))
}
sapply(realnum, spread.funs)

# Or just use a package, which gives all the stats in neat table
install.packages("psych")
library(psych)
sapply(realnum, FUN=describe)

# Lotsize seems to be reoughtly normally distributed (mean, sd, median, kurtosis), with a slight right skew (skew). But it has outliers (e.g., long tail). Note trimmed 
# Price seems to be roughtly normally distributed, also with a slight right skew. It too has a few outliers. 
# Bedrooms are the most normally distributed, but kurtosis is very low--something is off with the shape of the "peak".  


# Plotting distributions 
attach(carsnum)
hist(price)
hist(price, probability=TRUE)
hist(price, breaks=20)
hist(price, breaks=10, col=c("steelblue", "red"))
box()

# Now for mileage
hist(mileage, col=c("steelblue", "red"))
box() 

# Now add rug plot (actual data elements)
rug(jitter(price), col="darkgray")

# Another solution to the binning problem is to use a "smoother": the kernel density plot
den<-density(mileage)
plot(den)

# Now combine all three plots together: 
hist(mileage, col=c("steelblue", "red"), freq=F)
rug(jitter(mileage), col="darkgray")
lines(density(mileage), col="yellow", lwd=3) # Note how the lines function is used to overlay the density plot 
box() 

#### Additional notes 
#=

# Colors: 
#--------
# colors()
# demo("colors") 
# Also check out: 
# http://research.stowers-institute.org/efg/R/Color/Chart/index.htm 
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf  
# and the package RColorBrewer

# Saving output: 
#--------
# To append plots you could output to a pdf using a package or a rather complicated code in R base. 
# See discussion here: http://stackoverflow.com/questions/13273611/how-to-append-a-plot-to-an-existing-pdf-file
# You can also: 
# 1) Use RMarkdown to embed both code and output
# 2) Save output as an image file, e.g., png("histograms.png", width=3.25,height=3.25,units="in",res=1200, bg="transparent")


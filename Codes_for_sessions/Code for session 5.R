#====================================
# Statistics for Managers, Session 5
#====================================
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================

# Load libraries and data 
library('tidyverse') # for manipulating data 
library('psych')
load("Session 5.Rdata")
# Categorical and numeric variable  
#=
# Quick recap 
# Numeric stats
table(emp$Performance_num, emp$Gender) 
# Boxplots are a good example of this procedure -- they help show differences in categories as they relate to a numeric variable.  
boxplot(Performance_num ~ Gender, data=emp, main="Performance Change After Training by Gender", 
	xlab="Gender", ylab="Performance",
	col=c("orange", "lightblue4")) # Better! 
	
# Density plots, histograms etc. can also be used for the same purpose! 
m<-emp[emp$Gender=="Male",]
f<-emp[emp$Gender=="Female",]

plot(density(m$Performance_num), col="red", lwd=2.5, main="Distribution of Performance Change by Gender")
lines(density(f$Performance_num), col="blue", lwd=2.5) # There is a more noticable difference. 
legend("topright", 
       legend = c("Female", "Male"), 
       fill = c("blue", "red"))	  # Female exhibit bimodal distribution 

# Aggregate data and statistical parameters. 
# Three often used options: 
aggregate(Revenue ~ Metro, data=sales, FUN="mean")
aggregate(Revenue ~ Metro, data=sales, FUN="sd")

# To get most common parameters, use the psych package 
describeBy(sales$Revenue , sales$Metro) 
  # But formatting is an issue... 

# Better formatting and control with dplyr (from tidyverse)
revenue_by_metro <- sales %>% group_by(Metro) %>% summarise(avg = mean(Revenue), median = median(Revenue), sd = sd(Revenue))
is.data.frame(revenue_by_metro)
View(revenue_by_metro) # good. now you can save this table. 
write.csv(revenue_by_metro, "revenue_metro.csv", row.names = F)

# We can also group more than one factor and a numeric variable! 
sales %>% group_by(Metro, MarketSegment) %>% summarise(avg = mean(Revenue), sd = sd(Revenue))
 

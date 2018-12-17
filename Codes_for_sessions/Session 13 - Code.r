#=============================================================
# Statistics for Managers, Session 13
#=============================================================
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit 
# permission from the instructor.  
#=============================================================

## Load libraries and data 

load("session 13 data.Rdata") 

library(ggplot2)
library(gridExtra)
library(lubridate) 
library(tidyverse) 
library(zoo) 

## 1. Data vis with ggplot2
#= 

# graphics is what base R uses 
# developed later, grid is the foundation for lattice and ggplot2 
# lattice is now a part of R, ggplot2 is a separate package 

# Today, ggplot2 is the most popular library for plotting in R (and increasingly in Python)
# It can get a bit complicated, becase there are many options
# It is based on the  grammar of graphics, similar to 
# the notion of verbs (like dplyr, tidyr, and other packages in tidyverse) 

# qplot = simple version. We will start with this one. 
#=
# scatterplot 
qplot(y=pct, x=pay, data=baseball, main="PCT and Pay")
# it can be more useful (and look nicer) if we add a smoother 
qplot(y=pct,x=pay, data=baseball, main="PCT and Pay", geom=c("point", "smooth"))


# boxplot 
qplot(league,pct, data=baseball, ylab="%wins", xlab="League", main="PCT by League", geom="boxplot") 

# histogram 
qplot(pct, data=baseball, geom="histogram") # not great 
qplot(pct, data=baseball, geom="histogram", binwidth=0.05, col=I("steelblue"), fill=I("blue"), alpha=I(.4))
	# notice the level of control we have on colors! 

# it can get better 
qplot(pct, data=baseball, geom="histogram", binwidth=0.05, col=I("steelblue"), fill=I("blue"), alpha=I(.4)) +
geom_vline(xintercept = median(baseball$pct), color="red", lwd=2) 

# faceting 
qplot(x=pay,y=pct, data=baseball, ylab="%wins", xlab="Pay", main="Relationship between PCT and Pay, by League", facets=~division)

qplot(x=pay,y=pct, data=baseball, ylab="%wins", xlab="Pay", main="Relationship between PCT and Pay, by League", facets=~division, geom=c("point", "smooth")) # not ideal when we have so few observations 

# Now for the real thing: ggplot2 
#=
# data + aesthetic attributes (variables) + geometric objects (plot type) + stats (e.g., lm) 
# + scale + coordinates + faceting + annotation 

# Example 
#-
ggplot(data=baseball, # data 
	aes(x=pay, y=pct, color=division)) + # aesthetic 
	geom_point(pch=20, size=4) + # geometry used 
	stat_smooth(method="lm", se=F, lwd=1.5) + # statistical transformation (default is with CI, se=T)
	labs(title="Relationship between PCT and Pay by League", x="Pay", y="%wins") # annotation 

# Univariate
#- 
ggplot(data=baseball, aes(x=pay)) + geom_histogram() 

# Numeric and factor  

ggplot(data=baseball, aes(x=pay, fill=division)) +
       geom_density(alpha=.3)

# Two factors  

ggplot(realestate, aes(x=prefarea, fill=airco)) + 
   geom_bar(position="stack") 

ggplot(realestate, aes(x=prefarea, fill=airco)) + 
   geom_bar(position="dodge")

ggplot(realestate, aes(x=prefarea, fill=airco)) + 
   geom_bar(position="fill") 
   
# which one is better? 
	# Notice that arguments in different places will "behave" differently: 
	# fill=y in aes will represent each level of the factor in the barplot
	# fill="orange" in geom_bar will be the color of the barplot
	# fill="orange" in aes will mean variable="red" , which will lead to odd results 
	   
# Two numeric and two factors! 
ggplot(realestate, aes(x=lotsize, y=price/1000, color=prefarea, shape=airco)) +
       geom_point() + geom_jitter() 
	   
# Facets
#- 
# These trellis graphs (facets) are useful. They allow breaking up plots by groups. 
ggplot(data=cars, aes(x=price)) + 
     geom_histogram() + 
     facet_wrap(~transmission)

ggplot(data=cars, aes(x=mileage, y=price, color=transmission)) + 
     geom_point() + 
     facet_grid(.~model)
cars$year_f[cars$year<2008] <- "older"
cars$year_f[cars$year>=2008] <- "newer"

# Two numeric variables, with two factors, AND facets! 
ggplot(data=cars, aes(x=mileage, y=price, color=transmission, shape=year_f)) + 
     geom_point() + 
     facet_grid(.~model)

# Statistics (smoothers)
#-
p1 <- ggplot(data=realestate, aes(x=log(lotsize), y=log(price))) +
       geom_point()  # it is possible, and advisable, to assign ggplot output into an object 
   
p1 + geom_smooth(method="loess")  + geom_smooth(method="lm", color="orange") # se = F will remove the CI band 

# Plot parameters  
#-
# ggplot2 offers pretty neat deafults. Here's what to do to change them: 
ggplot(data=cars, aes(x=mileage, y=price, color=transmission, shape=year_f)) + 
	scale_color_manual(values=c("orange", "steelblue")) + # this is how we control color 
    geom_point() + 
    facet_grid(.~model) +
	scale_y_continuous(breaks=c(5000, 10000, 15000, 20000),
	labels=c("$5K", "$10K", "$15K", "$20K")) + # this is how we control the appearance of the y-axis 
	labs(title="Car Price by Mileage, Transmission and Age", x="", y="") + # this is how we control titles 
	theme(legend.position="none") # theme is used to control various parameters, including the legend 
	
# Themes 
#- 
classtheme <- theme(plot.title = element_text(face="bold.italic", size="16", color="steelblue"), 
	axis.title=element_text(face="bold.italic", size=10, color="gray"), 
	axis.text=element_text(face="bold", size=8, color="darkblue"), 
	panel.background=element_rect(fill="white", color="lightgreen"), 
	panel.grid.major.y=element_line(color="grey"),
    panel.grid.minor.y=element_line(color="brown", linetype=2),
    legend.position="top")

ggplot(realestate, aes(x=lotsize, y=price, color=prefarea, shape=airco)) +
       geom_point() + geom_jitter() + classtheme

# Arranging ggplots on canvas 
#- 
par(mfrow=c(1,2))
ggplot(data=baseball, aes(x=pay)) + geom_histogram() 
ggplot(data=baseball, aes(y=pct, x=pay)) + geom_point() 
	# doesn't work! instead, 

p1 <- ggplot(data=baseball, aes(x=pay)) + geom_histogram() + ggtitle("Distribution of Baseball Salaries")
p2 <- ggplot(data=baseball, aes(y=pct, x=pay)) + geom_point() + ggtitle("Relationship between Salaries and PCT")  
gridExtra::grid.arrange(p1, p2, ncol=2) # or use the multiplot function from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Saving ggplots 
#- 
ggsave(file="payhist.pdf", plot=p1, width=5, height=5.5) # width and height are in inches 
# can also save as .pdf and many other formats 

###########################################################################
# Using ggplot2, plot data from your project. Try different plotting options
# including, a theme for your team! 
###########################################################################


## 2. (more advanced) Data wrangling 
#= 

# Basic cleaning 

bp_samp$ESTIMATED_COST1 <- as.numeric(gsub("\\$", "", bp_samp$ESTIMATED_COST))

bp_test <- bp_samp %>% filter(!is.na(ISSUE_DATE)&!is.na(ESTIMATED_COST1))

bp_test[1,4] # it was NA in the original 


# Select columns with dplyr 
names(bp_samp) 

bp <- bp_samp %>% select(PERMIT_TYPE, ISSUE_DATE, ESTIMATED_COST="ESTIMATED_COST1", AMOUNT_WAIVED)

bp1 <- bp_samp %>% select(-c(ID, PERMIT_TYPE)) 
 
names(bp) 

names(bp1) 

# Select rows with dplyr 

table(bp$PERMIT_TYPE)
bp1.0 <- bp %>% filter(PERMIT_TYPE!="PERMIT - EASY PERMIT PROCESS")
bp1.1 <- bp %>% filter(PERMIT_TYPE=="PERMIT - EASY PERMIT PROCESS")
bp1.2 <- bp %>% filter(PERMIT_TYPE=="PERMIT - EASY PERMIT PROCESS" | PERMIT_TYPE=="PERMIT - SIGNS" )
bp1.3 <- bp %>% filter(PERMIT_TYPE=="PERMIT - EASY PERMIT PROCESS" & ESTIMATED_COST>3275)
dim(bp1.0); dim(bp1.1); dim(bp1.2); dim(bp1.3)

# Dates 
#= 
# Dates are an important data type
# Conversion is a bit of a process 
class(bp$ISSUE_DATE) 
head(bp$ISSUE_DATE) #we'll focus on ISSUE_DATE
as.Date(head(bp$ISSUE_DATE)) #oops. one more time
as.Date(head(bp$ISSUE_DATE), format="%m/%d/%Y")
bp$date <- as.Date(bp$ISSUE_DATE, format="%m/%d/%Y") 
class(bp$date)
	# but who wants to deal with these abbreviations? better use lubridate! 

mdy(head(bp$ISSUE_DATE)) # voila 
# Date AND Time? We could continue with base R, but it's much easier to use lubridate
sometimes <- c("1/12/2017 7:00:01 PM", "18/12/2017 7:00:20 AM", "23/12/2017 7:02:15 PM")
dmy_hms(sometimes, tz="UTC") # UTC is actually read automatically from my computer. 
# Finally, what if I want to convert to year or quarter? 
head(year(bp$date))  # uses lubridate 
head(as.yearqtr(bp$date)) # uses zoo 

###########################################################################
# Using the above packages, better organize the data for your project.  
###########################################################################



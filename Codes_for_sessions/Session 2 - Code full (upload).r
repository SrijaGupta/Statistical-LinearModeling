#====================================
# Statistics for Managers, Session 2 
#====================================
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  


#Vectors 
#=
# Numeric vector
somenum  <- c(1,3.5,5)
somenum <- c(seq(from=1.5, to=11, by=0.2), somenum)
# Character vector
mixed<- c(pi, 1, "sometext", TRUE) # Mixed vector, turns into character
# Logical vector
right <- c(TRUE, T, FALSE) # T and F are equivalent to TRUE and FALSE
# Factor
sales_quota <- as.factor(c("high", "low", "high", "medium", "low", "high","medium", "medium","low"))

# Let's examine the structure of these vectors
# What does it contain?
head(somenum)
tail(somenum, n=10)
length(somenum) # Returns the number of observations in a vector
length(mixed)
nchar(mixed) # Counts the character in each observation in a character vector. 
class(sales_quota)
str(sales_quota)
is.numeric(right)
is.factor(sales_quota)

# Finally, we need to check for any missing data (NA)  
is.na(c(grades, mixed, NA, "NA", ".","")) # Returns a logical vector testing for NA


#Solutions
#=
statmngr<-c(9, 8, 9, 8.2, 7.1)
class(statmngr)# numeric
mean(statmngr) # 8.36
statmngr<-c(statmngr, NA, 81,7)
mean(statmngr) # gives NA, check help to know how to deal with NAs > ?mean()
mean(statmngr, na.rm=TRUE) # 18.471

# Vector subsetting  
#=
somenum[2]  # First 
somenum[2:3]  # Second to third
somenum[c(1,3)] # First and third
somenum[-c(1,3)] # Exclude first and third 
sort(somenum) # Order a vector 
somenum
order(somenum) # What's this? Ah, it is the element index number! 
somenum[49]
somenum[1]
sort(somenum, decreasing=TRUE) # Reverse sort 

#Solutions 
#=
numvec<-seq(from=1, to=99, by=2)
length(numvec) # 50
morethan50<-numvec[numvec>50]
morethan50<-sort(morethan50, decreasing=T)
newnums<-morethan50[c(12,8,20)] # 77, 85, 61

# Change vector elements
#=
# By adding elements to an existing vector
mixed.new <- c(mixed,"Alex","Beth") 
somenum.new <- c(1,6,somenum,0,NA)

# By substituting elements in existing (indexed) positions 
somenum[2]<-5  # Substitutes the second element 
somenum[c(1,3)]<-c(5,10) # Substitutes the first and third elements 

#Solutions 
#=
num<-seq(from=9,to=99, by=2)
quantile(num) # 31.5 and 76.5
sd(num[num>20]) # 23.81176
# various ways to solve this one. Perhapse the easiest to understand: 
n1<-num[num<51] 
n2<-num[num>71] 
n3<-c(n1,n2)
mean(n3) # 51.8

# Missing data and changing vector type 
#=
# Change vector type
# First let's perform a logical test to determine vector type
is.character(sales_quota) 
is.character(mixed) 
mixed
as.numeric(mixed) # Really did coerce a vector conversion. Turned non-numeric elements to NAs
as.character(sales_quota) # Converted factor levels into characters or "strings"
as.numeric(sales_quota) # But we can also convert a factor into numeric: factor "levels" turn into numeric values 

# Solutions 
#= 
statmngr<-sort(statmngr)
statmngr[c(2)]<-7.9
statmngr[statmngr==9]<-NA
as.factor(statmngr) # There are some stylistic differences, but the vector is essentially unchanged 

# Data frame
#=
dat <- data.frame(8:10, c("exec", "mid", "exec")) 
dat <- data.frame(score = 8:10, role = c("exec", "mid", "exec")) # Better! This one has clear, understandable column names
dat<-data.frame(somenum, mixed, sales_quota) # Oops. What is happening here? Right! We need to make sure that vetors have the same length. 
dat<-data.frame(somenum[1:4], mixed, sales_quota[1:4])
length(dat) # Odd. Okay, that's the length/number of columns. But really, length is for vectors.  
dim(dat) # Better 
class(dat)
str(dat)
colnames(dat) # Getting a vector of column names. Let's change these 
colnames(dat)<-c("score", "other", "sales_quota")
dat
dat<-data.frame(dat, vertical=c("govt", "health", "tech", "govt"),stringsAsFactors=TRUE) # Usually you'd want stringsAsFactors=F, but here both nominal variables are indeed factors. 
dat$other # Use the $ symbol to identify a column name
dat$sales_quota
attach(dat) # After attaching a dataframe, there is no need to specify the full: df$variable. Use detach(df) when you are done. 
sales_quota 

# Data frame subsetting 
#=
dat[1:6] # Oops. We need to also specify columns
dat[,1]
dat[1,c(1:3)]  # First row, column 1 through 3
dat[,c("vertical","sales_quota")] # Select only these columns
paygrade<-c(1,3,2,1) 
dat<-cbind(dat, paygrade) # Added the paygrade vector (column) into the dataframe 
dat<-rbind(dat,c(5,TRUE, "medium", "tech")) # Added a row 
dat$paygrade<-NULL # This is how we drop a column 

# Solutions 
#=
hr<-data.frame(
	Experience=c(1,2,1,8),
	Performance=c(10,8,7,10),
	Employees=c(3,4,2,10), 
	stringsAsFactors=FALSE
	)
	
hr<-data.frame(hr, 
ID=c(12,13,14,15),
	stringsAsFactors=FALSE)

hr[3,2]<-9
mean(hr$Performance) # 9.25



























 



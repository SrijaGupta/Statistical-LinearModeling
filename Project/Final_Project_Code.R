#Car insurance dataset
#Loading.

load('Final_ProjectData.RData')
# original data is with 'insurance' name
View(insurance)
str(insurance)
options(scipen = c(999))

#dataset has 7702 variable and 16 variables.

#trnsforming data varibles to factor for our analysis.
insurance$Year<- as.factor(insurance$Year)
insurance$Mfr.Model<- as.factor(insurance$Mfr.Model)
insurance$Type <- as.factor(insurance$Type)
insurance$Gender <- as.factor(insurance$Gender)
insurance$Channel <- as.factor(insurance$Channel)
insurance$Age <- as.factor(insurance$Age)
insurance$State <- as.factor(insurance$State)
insurance$City <- as.factor(insurance$City)
insurance$ClaimsInd <- as.factor(insurance$ClaimsInd)
insurance$PaymentFrequency <- as.factor(insurance$PaymentFrequency)
insurance$Cover.Type <- as.factor(insurance$Cover.Type)

insurance$State <- tolower(insurance$State)
insurance$City <- tolower(insurance$City)
insurance$State <- as.factor(insurance$State)
insurance$City <- as.factor(insurance$City)

#install.packages("plyr")
library(plyr)

#dividing into age groups 
insurance$Age <- revalue(insurance$Age, c("18-25"="18-24", "25 - 30"="25-34", "30 - 40" = "35-44", "40 - 50" = "45-54"
                                          ,"50 - 60"="55-64", "60 - 70" = "65+", "70+"= "65+"))

#Derived column zones
insurance$Zone[insurance$State == "assam" | insurance$State == "west bengal"] <- "East"
insurance$Zone [insurance$State == "madhyapradesh" | insurance$State =="madhya pradesh"|insurance$State == "gujarat" | insurance$State == "maharashtra"] <- "West"
insurance$Zone[insurance$State == "haryana"   | insurance$State == "uttar pradesh"| insurance$State == "punjab"| insurance$State == "ncr"] <- "North"
insurance$Zone[insurance$State == "karnataka"  | insurance$State == "tamilnadu"| insurance$State == "kerala"] <- "South"
insurance$Zone <- as.factor(insurance$Zone)

#Derived column Vehicle_Cat; defining car according to cc
insurance$Vehicle_Cat[insurance$Cubic.Capacity <= 1250 ] <- "CC-small sized"
insurance$Vehicle_Cat [insurance$Cubic.Capacity > 1250 & insurance$Cubic.Capacity <= 1800] <- "CC-medium sized"
insurance$Vehicle_Cat[insurance$Cubic.Capacity>1800 ] <- "CC-large sized"
insurance$Vehicle_Cat <- as.factor(insurance$Vehicle_Cat);insurance$Vehicle_Cat
table(insurance$Vehicle_Cat)

#Derived column Revenue.
insurance$Revenue <- insurance$Premium - insurance$Claim.Amount

#saved our dataset as ins.Rdata
#save.image(file="ins.RData")
#load("ins.Rdata")

ins1<-insurance #storing value in a different variable for analysis

#Shifting scale of Revenue and Calculating profit as Proportion of Revenue
ins1$Revenue <-(ins1$Revenue)-(min(ins1$Revenue))+1
ins1$profit <-(ins1$Revenue/max(ins1$Revenue)*100)

#Examining variables and their relation.
str(ins1)

## We would be working with the following variables.
  # Factors: Zone,Gender,Age,ClaimsInd,Vehicle_cat,Year
  # Numeric: IDV, profit

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Univariate Analysis >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

library(ggplot2)
library(psych)

# Numeric Variable Analysis
  #>>>PROFIT
describe(ins1$profit)
#vars  n  mean   sd  median trimmed mad min max range  skew kurtosis   se
#1  7702 80.45  6.32  81.95   81.51 1.3   0 100   100 -3.14  18.23   0.07
ggplot(ins1,aes(x = profit))  +  geom_density(color="steelblue", fill="steelblue")+ ggtitle("Density plot of Profit")
ggplot(ins1,aes(x = '1', y = profit)) + geom_boxplot()+ ggtitle("Boxplot of Profit")
# too many outliers
# highly left-skewed


  #>>>IDV 
describe(ins1$IDV)
# vars n   mean     sd   median  trimmed   mad    min     max    range  skew  kurtosis  se
# 1   7702   385617.3 246733.8 305093 343581.4 136637.9 111822 1790603 1678781 2.08  5.63   2811.43
ggplot(ins1,aes(x = IDV))  +  geom_density(color="steelblue", fill="steelblue")+ ggtitle("Density plot of IDV")
ggplot(ins1,aes(x = '', y = IDV))  +  geom_boxplot() + ggtitle("Boxplot of IDV")
# Right-skewed
# too many outliers


# Factors Variable Analysis

  #>>>ZONE
t<-table(ins1$Zone);t;(prop.table(t))*100;
#East North South  West 
#935  4163  1728   876 
#North zone has the most data
ggplot(ins1,aes(x =Zone ))  +  geom_bar(fill="steelblue") + ggtitle("Distribution of Zone")

  #>>>Age
a<-table(ins1$Age);a;(prop.table(a))*100;
#18-24 25-34 35-44 45-54 55-64   65+ 
#1145  1500  2089  1654   782   532 
#plot(ins1$Age, col = c("light green","pink"),ylim=c(0,2000),main="Age Group Analysis for Original data")
#age group 35-44 has most of the data
ggplot(ins1,aes(x =Age ))  +  geom_bar(fill=c("steelblue")) + ggtitle("Distribution of Age")


  #>>>Gender
g<-table(ins1$Gender);g;(prop.table(g))*100;
#Female   Male 
#2134   5568 
#plot(ins1$Gender, col = c("light green","pink"),ylim=c(0,6000),main="Gender  Analysis for Original data")
ggplot(ins1,aes(x =Gender ))  +  geom_bar(fill=c("steelblue")) + ggtitle("Distribution of Gender")
#Males are more than females in our dataset.


  #>>>Vehicle_cat
v<-table(ins1$Vehicle_Cat);v;(prop.table(v))*100;
#CC-Large Sized CC-Medium Sized  CC-Small Sized 
#1023            2571            4108 
#plot(ins1$Vehicle_Cat, col = c("light green","pink","light blue"),ylim=c(0,5000),main="Vehicle category  Analysis for Original data")
ggplot(ins1,aes(x =Vehicle_Cat ))  +  geom_bar(fill=c("steelblue")) + ggtitle("Distribution of Vehicle Category")

  #>>>Year
y<-table(ins1$Year);y;(prop.table(y));
#2005 2006 2007 2008 2009 2010 2011 
#272  693  870  988 1154 1980 1745
ggplot(ins1,aes(x =Year ))  +  geom_bar(fill=c("steelblue")) + ggtitle("Distribution of Year")


  #>>>ClaimsInd
c<-table(ins1$ClaimsInd);c
#0    1 
#5825 1877 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(gridExtra);library(lubridate) ;library(tidyverse) ;library(zoo); library(corrplot)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Bivariate Analysis >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

# BUT  FIRST HANDLING OUTLIERS

# Creating function outlier to retrieve extreme outliers i.e + or - 3 times IQR of upper quartile an lower quartile respectively
outliers <- function(column) {
  lowerq <- as.vector(quantile(column)[2]) # returns 1st quartile
  upperq <- as.vector(quantile(column)[4]) # returns 1st quartile
  iqr <- upperq-lowerq  
  extreme.outliers.upper <- (iqr * 3) + upperq
  extreme.outliers.lower <- lowerq - (iqr * 3)
  extreme.outliers<-which(column > extreme.outliers.upper 
                          | column < extreme.outliers.lower)
  print(paste("Extreme outlier:", extreme.outliers))
  return(extreme.outliers)
}

#>>>>>> Two NUMERIC variable analysis i.e profit vs IDV >>>>>>>>>>>#

# plotting profit vs IDV from original data
ggplot(ins1, aes(x=IDV,y=profit)) + geom_point() + 
  ylab('Profit')+  ggtitle("Relationship between Profit & IDV without handling outliers")+
  geom_smooth(method="lm", se=FALSE) 

# doing correaltion
c.o<-cor(ins1$IDV,ins1$profit);c.o; # with original data 0.2067221; no correlation ;

# From our univariate analysis we saw we have highly skewed data for IDV.
# So get all extreme outliers from our IDV variable
Ex_out_IDV <- outliers(ins1$IDV) ;length(Ex_out_IDV)#188 # Ex_out_IDV = extreme oulier of IDV :-)
ins_rm_ex_IDV <-ins1[-Ex_out_IDV,] # ins_rm_ex_IDV = insuance data removing extreme oulier of IDV :-)
ggplot(ins_rm_ex_IDV, aes(x=IDV,y=profit)) + geom_point() + 
  ylab('Profit')+ ggtitle("Relationship between Profit & IDV with handling only IDV outliers")+
  geom_smooth(method="lm", se=FALSE) 


# Also from our univariate analysis we saw we have highly skewed data for Profit too
# So get all extreme outliers from our profit variable
EX_out_proIDV <-outliers(ins_rm_ex_IDV$profit);length(EX_out_proIDV)#1279 # EX_out_proIDV = extreme oulier of profit after removing IDV outliers :-)
ins_rm_ex_IDV_pro <-ins_rm_ex_IDV[-EX_out_proIDV,] #ins_rm_ex_IDV_pro = insuance data removing extreme oulier of profit after IDV :-)
ggplot(ins_rm_ex_IDV_pro, aes(x=IDV,y=profit)) + geom_point() + 
  ylab('Profit')+ ggtitle("Relationship between Profit & IDV with handling both profit & IDV outliers")+
  geom_smooth(method="lm", se=FALSE) 
#much better,strong relation

# Now doing correaltion again
c.h<-cor(ins_rm_ex_IDV_pro$profit,ins_rm_ex_IDV_pro$IDV);c.h; #0.7021029 , much better positive correlation

((c.h-c.o)/c.o)*100 # 239% imrovement in correlation value after handling outliers from both numeric variables;Awsome.

corplot_idv_pro <-ins_rm_ex_IDV_pro[,c(3,20)]
cormat <- cor(corplot_idv_pro)
corrplot(cormat, addCoef.col = "gray")


#>>>>>>>> Two Factors variable analysis>>>>>>>># 
  
  #claim

#compairing claim with gender,age,zone,revenue,IDV,Vehicle category.

# ClaimsInd and gender
ggplot(ins_rm_ex_IDV_pro, aes(x=Gender, fill=ClaimsInd)) + 
  geom_bar(position="fill")+ 
  ggtitle("Distribution of  Gender by Claim ")
#Descriptive: Female claims slightly more than male.

# Claimed vs age
ggplot(ins_rm_ex_IDV_pro, aes(x=Age, fill=ClaimsInd)) + 
  geom_bar(position="fill") +
  ggtitle("Distribution of Age by Claim ")
#Descriptive age: 35-44 claim the most of the insurance

# Claimed vs Zone
ggplot(ins_rm_ex_IDV_pro, aes(x=Zone, fill=ClaimsInd)) + 
  geom_bar(position="dodge") +
  ggtitle("Distribution of Zone by Claim ")
#North people claim more comapritively than others zone.

# Claimed vs vehicle category
ggplot(ins_rm_ex_IDV_pro, aes(x=Vehicle_Cat, fill=ClaimsInd)) + 
  geom_bar(position="fill") +
  ggtitle("Distribution of Vehicale Category by Claim ")
#calim is comaritively equal among all category


############################ ANALYSIS OF FACTOR AND NUMERIC ###########################################

# profit among Gender 
aggregate(ins_rm_ex_IDV_pro$profit ~ins_rm_ex_IDV_pro$Gender, FUN="mean") 
ins_rm_ex_IDV_pro %>% group_by(Gender) %>% summarize(avg = mean(profit), std = sd(profit)) 

qplot(Gender,profit, data=ins1, ylab="%Profit", xlab="Gender", main="Profit by Gender in original data with Outliers", geom="boxplot") 
qplot(Gender,profit, data=ins_rm_ex_IDV_pro, ylab="%Profit", xlab="Gender", main="Profit by Gender in cleaned data", geom="boxplot") 

# doing anova testing to verify relation;
  #data without outliers
gender_No_outlier_anova <- aov(profit~Gender,data=ins_rm_ex_IDV_pro)
summary(gender_No_outlier_anova)
# p value 0.155, still no relation, we fail to reject null hypothesis,hence there is no relation b/w profit by gender


#Age
aggregate(ins_rm_ex_IDV_pro$profit ~ins_rm_ex_IDV_pro$Age, FUN="mean") 
ins_rm_ex_IDV_pro %>% group_by(Age) %>% summarize(avg = mean(profit), std = sd(profit)) 

qplot(Age,profit, data=ins1, ylab="%Profit", xlab="Age", main="Profit by Age Group in original data with Outliers", geom="boxplot") 
qplot(Age,profit, data=ins_rm_ex_IDV_pro, ylab="%Profit", xlab="Age", main="Profit by Age Group in cleaned data", geom="boxplot") 

# doing anova testing to verify relation;

  #data without outliers
Age_No_outlier_anova <- aov(profit~Age,data=ins_rm_ex_IDV_pro)
summary(Age_No_outlier_anova)
#no improvement; still no relation


#Zone
aggregate(ins_rm_ex_IDV_pro$profit ~ins_rm_ex_IDV_pro$Zone, FUN="mean") 
ins_rm_ex_IDV_pro %>% group_by(Zone) %>% summarize(avg = mean(profit), std = sd(profit)) 

qplot(Zone,profit, data=ins1, ylab="%Profit", xlab="Zone", main="Profit by Zone in original data with Outliers", geom="boxplot") 
qplot(Zone,profit, data=ins_rm_ex_IDV_pro, ylab="%Profit", xlab="Zone", main="Profit by Zone in Cleaned data", geom="boxplot") 

# doing anova testing to verify relation;
  # data without outliers
zone_No_outlier_anova <- aov(profit~Zone,data=ins_rm_ex_IDV_pro)
summary(zone_No_outlier_anova)
# pvalue=0.539, no relation


#Vehicle_Cat
aggregate(ins_rm_ex_IDV_pro$profit ~ins_rm_ex_IDV_pro$Vehicle_Cat, FUN="mean") 
ins_rm_ex_IDV_pro %>% group_by(Vehicle_Cat) %>% summarize(avg = mean(profit), std = sd(profit)) 

qplot(Vehicle_Cat,profit, data=ins1, ylab="%Profit", xlab="Vehicle_Cat", main="Profit by Vehicle Category in original data with Outliers", geom="boxplot") 
qplot(Vehicle_Cat,profit, data=ins_rm_ex_IDV_pro, ylab="%Profit", xlab="Vehicle_Cat", main="Profit by Vehicle Category in cleaned data", geom="boxplot") 

# doing anova testing to verify relation;
  # data without outliers
VehicleCat_No_outlier_anova <- aov(profit~Vehicle_Cat,data=ins_rm_ex_IDV_pro)
summary(VehicleCat_No_outlier_anova)
T_VehicleCat_No_outlier_anova <- TukeyHSD(VehicleCat_No_outlier_anova)
T_VehicleCat_No_outlier_anova 
# There is a relation between profit and vehicle category,
  # with large-sized vehicle,the most profitable for the company

#Note:
  # We have removed the outliers (1279+188) but we have also compared our analysis with the original data for all factors.
  # There is no major change in the trend of our anlysis, so we can safely assume to remove our outliers.

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
library(car)
#>>>>>>>>>>>>>>>>>>>>>>> Regression MODELLING >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#trying few transformation for a better distribution of profit
plot (density (ins_rm_ex_IDV_pro$profit))
plot (density (log(ins_rm_ex_IDV_pro$IDV)))#no effect of taking log; 
plot (density (sqrt(ins_rm_ex_IDV_pro$IDV)))#no effect of taking sqrt;
# default it is;

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Mdel #1

# creating model by taking only numeric variable i.e IDV
mod.1<- lm (profit~IDV, data=ins_rm_ex_IDV_pro) 
summary(mod.1)# R-squared: 49.29%
par(mfrow=c(2,2))
plot(mod.1)
dev.off()

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Mdel #2

# creating model by taking all the variable
mod.2<- lm (profit~IDV+Gender+Vehicle_Cat+Age+Zone+Year, data=ins_rm_ex_IDV_pro)
summary(mod.2) # R-squared: 51.69%; Improvement is there
par(mfrow=c(2,2))
plot(mod.2)
dev.off()

#checking for Multicollinearity.
#using VIF (Variance Inflation Factor)
vif(mod.2) 
sqrt(vif(mod.2)) > 2 # if any variable is true, we would need to drop it 
# Year  & Age need to go :-)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Mdel #3

# creating model by dropping age and year
mod.3<- lm (profit~IDV+Gender+Vehicle_Cat+Zone, data=ins_rm_ex_IDV_pro) 
summary(mod.3) # R-squared: 49.48%; back to close to 1st model :-(
par(mfrow=c(2,2))
plot(mod.3)
dev.off()
#have to drop zone as very less significant

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Mdel #4

# creating model by taking only variables with which we have got relation in our testing and adding Gender
mod.4<- lm (profit~IDV+Vehicle_Cat+Gender, data=ins_rm_ex_IDV_pro) 
summary(mod.4) # R-squared: 49.45%; no improvement still :-(

#>>>>>>>>>>>>>>>>>>>>>>Now Regression diagnostics on Mdel #4
#
par(mfrow=c(2,2)) # sets up the canvas to 4 plots, so that we don't need to go through them one by one 
plot(mod.4) # we have a few outliers
# we can also find outliers this way 3809,4613,4296
plot(hatvalues(mod.4)) 
identify(hatvalues(mod.4), col="red")
dev.off()
tail(sort(hatvalues(mod.4)))
outliers_s <- c(   532   ,     4025    ,    2029    ,    3933   )
ins_rm_ex_IDV_pro_s<-ins_rm_ex_IDV_pro[-outliers_s,] #remove the final outliers.

mod.4<- lm (profit~IDV+Vehicle_Cat+Gender, data=ins_rm_ex_IDV_pro_s) 
summary(mod.4)  # no improvement

#trying to remove outliers using Cooks distance
tail(round(sort(cooks.distance(mod.4))),10) # to get the outlier from the model; we are getting 10
outliers_r <- c(3471 , 661 ,4305 ,3127 ,3809, 4503 ,3795, 4296 ,7554 ,4613 )
ins_rm_ex_IDV_pro_r<-ins_rm_ex_IDV_pro[-outliers_r,] #remove the final outliers.

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Mdel #5

# creating model by removed outlier data from model 4
mod.5<- lm (profit~IDV+Gender+Vehicle_Cat, data=ins_rm_ex_IDV_pro_r) 
summary(mod.5) # R-squared: 49.4%; no improvement from model.4

# So finally going forward with model.4 so we can explain variance in our model at best by 49.45% #
qplot(x=IDV,y=profit, data=ins_rm_ex_IDV_pro, ylab="Revenue", xlab="IDV", main="Relationship between Profit and IDV, by Vehicle Cat", facets=~Vehicle_Cat, geom=c("point", "smooth"))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#End#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#ThankYou#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

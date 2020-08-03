#======================================================
#     PotentialPurchaseLoan - EDA
#======================================================

#Setting working directory
setwd("<Working Directory>")

#Verify the Working Directory
getwd()

#Install necessary Packages 
library(pacman)
p_load(readr,readxl,dplyr,ggplot2,tidyr)

#Loading Dataset & View 
TheraData <- read_excel("<dataset>")
View(TheraData)
glimpse(TheraData)


#DataSummary 
dim(TheraData) 

#Variable names
colnames(TheraData)

#Renaming the column names with space or special characters
names(TheraData)[names(TheraData) == "Age (in years)"] <- "Age_in_years"
names(TheraData)[names(TheraData) == "Experience (in years)"] <- "Experience_in_years"
names(TheraData)[names(TheraData) == "Income (in K/month)"] <- "Income_in_K"
names(TheraData)[names(TheraData) == "ZIP Code"] <- "Zip_Code"
names(TheraData)[names(TheraData) == "Family members"] <- "Family_members"
names(TheraData)[names(TheraData) == "Personal Loan"] <- "Personal_Loan"
names(TheraData)[names(TheraData) == "CD Account"] <- "CD_Account"
names(TheraData)[names(TheraData) == "Securities Account"] <- "Securities_Account"

# Structure of the dataset - All the variables are numbers.
str(TheraData)


##Converting target and the 4 variables into factor

TheraData$Personal_Loan = as.factor(TheraData$Personal_Loan)
TheraData$Securities_Account = as.factor(TheraData$Securities_Account)
TheraData$CD_Account = as.factor(TheraData$CD_Account)
TheraData$Online = as.factor(TheraData$Online)
TheraData$CreditCard = as.factor(TheraData$CreditCard)
str(TheraData)

#Checking Summary statistics
summary(TheraData)

#There are some NA in Family members column.
#For the problem given, Cust ID holds no importance for prediction. So , need to drop this column
#Age of customers ranges from 23 to 67 where median and mean are almost same - normally distributed
#Customers subscribed for online IB is almost 60% 
#customers having loans/accounts is less than or almost 9-10%
#customers having credi card is higher than than loan/accounts




#Drop ID Column 
TheraData <- TheraData[-c(1)]

#Checking for null values
anyNA(TheraData)
#true - there are null values

sum(is.na(TheraData))
#18

#find column names that have NA
colnames(TheraData)[apply(is.na(TheraData), 2, any)] 

unique (unlist (lapply (TheraData, function (x) which (is.na (x)))))# unique rows of NA
# [1]   21   59   99  162  236  290  488  722 1461 1462 2400 #2833 3702 4136 4139 4403 4404 4764

# Family member - treat NA with median which is 2 as number # of rows affected >15
TheraData[is.na(TheraData)] = 2


# Treating Experience in years - negative values
summary(TheraData$Experience_in_years)
library(modeest)
# finding the most frequent value and retreating with the neagtive value
mfv(TheraData$Experience_in_years) 
hist(TheraData$Experience_in_years , breaks = 40) 
#32 years is the most frequent value

#replace negative values with 32
TheraData$Experience_in_years <- replace(TheraData$Experience_in_years,TheraData$Experience_in_years < 0 , 32)

summary(TheraData$Experience_in_years)
str(TheraData)

#======================================================
# Univariate Analysis
#======================================================

#partition to get 2 graphs in a row
par(mfrow = c(1,2)

#Age
hist(TheraData$Age_in_years)
boxplot(TheraData$Age_in_years)
#Observations: No outliers, looks normal distribution,
#range - 23 to 67 ; median - 45 years
#customers in 20-25 and 65-70 range are very less which means
#bank has got customers who have mostly got work #experience >3 years
#and there is a fall in the phase of retirement


#Experience
hist(TheraData$Experience_in_years)
boxplot(TheraData$Experience_in_years)
# observations: normally distributed
# Range - 0 to 43 years and median - 21
# There are less customers above 35 years of experience 

    
#Income
hist(TheraData$Income_in_K, breaks = 20 )
boxplot(TheraData$Income_in_K)
# observations - range - 8 - 224 ; median - 64
# right outliers - customers with high income > 180K
# more customers - 20-50k
# Second most - 50-90k
# so customers having 20-90k majorly have assosiation with bank


#Familymembers
hist(TheraData$Family_members)
boxplot(TheraData$Family_members)
#observation - There are less customers with three family #members 
#when compared who have 1/2/4


#CCAverage
hist(TheraData$CCAvg)
boxplot(TheraData$CCAvg)
#observations - Most of the customers spend up to 1k on CC
# Outliers - who spend more than 5K on CC
# This gives us a suggestion to check if these outliers #customers
# CCavg vs loans vs income  - to see if theere is a pettern
# and have accepted for loans in last campaign

#Education
hist(TheraData$Education)
boxplot(TheraData$Education)
#This variable has values from 1-3 
#and doesnt look to be properly captured
#As most customers have only one year of education but
#income is really high - business

#Mortage
hist(TheraData$Mortgage)
boxplot(TheraData$Mortgage)
#observation - most of cutomers have 0-50k mortage(close to ##3500)
# and there are also outliers - Again gives us suggestion to #check
# in bivariate/multivariate againgst target variable to check 
# if there is a pattern 



#personal loan
#library(plotrix)
PLPie <- table(TheraData$Personal_Loan)
PLPieLabel <- paste(names(PLPie), "\n", PLPie, sep="")
pie3D(PLPie,radius = 0.9, labels = PLPieLabel,  col=c("darkgreen","red"),
    main="Pie Chart for Customers having Personal Loan at TheraBank")
barplot(PLPie,main = "Personal Loan",xlab = "count of customers")
prop.table(table(TheraData$Personal_Loan))*100
#observation - 9.6% of customers have taken personal loan 
#as mentioned in the problem statement


#Securities account
prop.table(table(TheraData$Securities_Account))*100
SABar <- table(TheraData$Securities_Account)
barplot(SABar,main ="Securties Acc", xlab = "Sec account")
SAPieLabel <- paste(names(SABar), "\n", SABar, sep="")
pie3D(SABar,radius = 0.9, labels = SAPieLabel,  col=c("red","green"),
      main="Pie Chart for Securities Account")
#observation - 10% of customers have securities amount

#CD account
prop.table(table(TheraData$CD_Account))*100
CDBar <- table(TheraData$CD_Account)
barplot(CDBar,main ="CD Account", xlab = "CD account")
CDPieLabel <- paste(names(CDBar), "\n", CDBar, sep="")
pie3D(CDBar,radius = 0.9, labels = CDPieLabel,  col=c("lightgreen","darkblue"),
      main="Pie Chart for Customers having CD Acc at TheraBank")

#observation - 6% of customers have CD account with bank


# Customers using Online banking
prop.table(table(TheraData$Online))*100
OnlineBar <- table(TheraData$Online)
barplot(OnlineBar,main ="Online Banking", xlab = "Count of usage of IB")
OnlinePieLabel <- paste(names(OnlineBar), "\n", OnlineBar, sep="")
pie3D(OnlineBar,radius = 0.9, labels = OnlinePieLabel,  col=c("yellow","red"),
      main="Pie Chart for Customers using IB at TheraBank")

#observation - 60% of customers use IB facilities

# Customers using Credit card issued by bank

prop.table(table(TheraData$CreditCard))*100
CreditBar <- table(TheraData$CreditCard)
barplot(CreditBar,main ="Credit Card", xlab = "Count of usage of creditcards")
CreditPieLabel <- paste(names(CreditBar), "\n", CreditBar, sep="")
pie3D(CreditBar,radius = 0.9, labels = CreditPieLabel,  col=c("purple","red"),
      main="Pie Chart for Customers having Creditcard at TheraBank")

#observation - 29% of customers have credit cards 

#======================================================
# BiVariate Analysis
#======================================================

# money spent on credit card vs income for cutomers applied/not applied PL
qplot(TheraData$Income_in_K,TheraData$CCAvg,data=TheraData,facets = .~TheraData$Personal_Loan)

# Observation:
#1. from the pattern observed, customers whose income is less than 100k
# and  with money spent on CC <3k have not applied PL
#2.Almost all the customers having income >100k has applied for PL
# and CC spent is spread and doesnt clearly lead us to any conclusion

#Mortagehouse value vs income for personal loas accepted
qplot(TheraData$Income_in_K,TheraData$Mortgage,data=TheraData,color = TheraData$Personal_Loan)

#observation: customers who have income less athan 100k and #house mortage
# <35 have not taken personal loan
# no clear observation for cutomers applied PL with respect to mortage
# doesnt have impact on PL applied

qplot(TheraData$Mortgage,TheraData$CCAvg,data=TheraData,color = TheraData$Personal_Loan)
# mortage < 200 and cc spent < 2.5k have definitely not #applied PL

ggplot(TheraData, 
       aes(x = TheraData$Income_in_K, 
           fill = TheraData$Personal_Loan)) +
  geom_density(alpha = 0.4) +
  labs(title = "Personal loan for Income")
table(TheraData$Age_in_years,TheraData$Personal_Loan)

ggplot(TheraData, 
       aes(x = TheraData$Income_in_K, 
           y = TheraData$Personal_Loan)) +
  geom_point() + 
  labs(title = "Salary distribution by rank")


ggplot(TheraData, 
       aes(x = TheraData$Experience_in_years, 
           fill = TheraData$Personal_Loan)) +
  geom_density(alpha = 0.4) +
  labs(title = "Personal loan accepted by Age")
table(TheraData$Age_in_years,TheraData$Personal_Loan)

ggplot(TheraData, 
       aes(x = TheraData$Experience_in_years, 
           y = TheraData$Personal_Loan)) +
  geom_point() + 
  labs(title = "Salary distribution by rank")


ggplot(TheraData, 
       aes(x = TheraData$Online, 
           fill = TheraData$Personal_Loan)) + 
  geom_bar(position = "dodge")

ggplot(TheraData, 
       aes(x = TheraData$CD_Account, 
           fill = TheraData$Personal_Loan)) + 
  geom_bar(position = "dodge")


ggplot(TheraData, 
       aes(x = TheraData$CreditCard, 
           fill = TheraData$Personal_Loan)) + 
  geom_bar(position = "dodge")


ggplot(TheraData, 
       aes(x = TheraData$Securities_Account, 
           fill = TheraData$Personal_Loan)) + 
  geom_bar(position = "dodge")
#======================================================
#     PotentialPurchaseLoan - CART
#======================================================

#Load the required packages
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(data.table)
library(ROCR)
library(InformationValue)

#Set seed to constant value
set.seed(1000)

#split Test ana Train Data
splitTheraCART = sample.split(TheraDataCART$Personal_Loan, SplitRatio = 0.7)
trainTheraCART = subset(TheraDataCART, splitTheraCART == T)
View(trainTheraCART)
dim(trainTheraCART)
testTheraCART = subset(TheraDataCART, splitTheraCART == F)
dim(testTheraCART)


#Setting the control parameters
r.ctrl = rpart.control(minbucket = 3, cp = 0)

#Build the CART model
CARTThera <- rpart(formula = Personal_Loan~Age_in_years+Experience_in_years+Income_in_K+Zip_Code+Family_members+CCAvg+Education+Mortgage+Securities_Account
                   +CD_Account+Online+CreditCard, data = trainTheraCART, method = "class", control = r.ctrl)

#Plot the decision tree
fancyRpartPlot(CARTThera)

#======================================================
#     Pruning
#======================================================

#Setting the control parameters
r.ctrl = rpart.control(minbucket = 3, cp = 0.015)

#Tune the CART model
CARTThera <- rpart(formula = Personal_Loan~Age_in_years+Experience_in_years+Income_in_K+Zip_Code+Family_members+CCAvg+Education+Mortgage+Securities_Account
                   +CD_Account+Online+CreditCard, data = trainTheraCART, method = "class", control = r.ctrl)

#Displaying the decision tree
fancyRpartPlot(CARTThera)


#INTERPRETATION OF CART MODEL OUTPUT:
#The root node identified by the CART model out of all the 12 #variables is Income. This indicated that income variable #plays a very important loan for the case study where #customers have accepted the personal loan.
#The first node is highlighted in green which means 90% of #the data in the node when no split is done is 0 which #belongs to customers who have not accepted personal loan #offer and 10% have accepted. 
#Traversing to left side of the tree, i.e., when income #< 115k , CART model has picked CCAvg which is the average #money spent on credit card per month. So, this variable #holds the next level of importance along with education. #This node is also highlighted in green which means customers #having income < 115 k and CCavg < 3 have not applied #personal loan.
#When CCAvg > 3 and income < 115k , CART model has picked up #CD_Account to be next level of important variable to find #set of customers who have accepted personal loan. So, when #customer has income <115k, CCAvg > 3 and having CD account #with the bank is highlighted blue and 83% customers in this #category have accepted the personal loan earlier in the #campaign run by the bank. Thus, Bank can focus on customers #with this set of criteria who will more likely accept #personal loans.
#On Traversing to the right side of the model at the first #node split, CART model has picked up education variable #which holds importance after income variable in the dataset. #From the tree, most of the personal loans are accepted in #the right side split. 
#When income > 115k and education is > 1.5 then 7% of the #customers have accepted the personal loan and this is #highlighted as blue as acceptance of personal loan is #greater.
#So, Bank can focus on customers with income > 115k and #education > 1.5 for higher acceptance of Personal loan.
#CART model has picked up Family members variable next to #Education spilt. 1% of the customers has accepted personal #loan when income > 115k, education >1.5 and family members > #2.5.

#Scoring/Predicting the training dataset
trainTheraCART$predict.class <- predict(CARTThera, trainTheraCART, type="class")
trainTheraCART$predict.score <- predict(CARTThera, trainTheraCART, type = "prob")
View(trainTheraCART)

# Scoring test sample and validating the same
testTheraCART$predict.class <- predict(CARTThera, testTheraCART, type="class")
testTheraCART$predict.score <- predict(CARTThera, testTheraCART, type = "prob")
View(testTheraCART)

## CART Model Confusion Matrix
CART_CM_train = table(trainTheraCART$Personal_Loan,trainTheraCART$predict.class)
CART_CM_test = table(testTheraCART$Personal_Loan,testTheraCART$predict.class)

CART_CM_train
CART_CM_test

## Error Rate

(CART_CM_train[1,2]+CART_CM_train[2,1])/nrow(trainTheraCART) #0.01428571
(CART_CM_test[1,2]+CART_CM_test[2,1])/nrow(testTheraCART) #0.02133333


##Accuracy

(CART_CM_train[1,1]+CART_CM_train[2,2])/nrow(trainTheraCART)#0.9857143
(CART_CM_test[1,1]+CART_CM_test[2,2])/nrow(testTheraCART)#0.9786667

#Building the ROC curve and lift charts
trainpredROCCART <- prediction(trainTheraCART$predict.score[,2], trainTheraCART$Personal_Loan)
trainperfROCCART <- performance(trainpredROCCART, "tpr", "fpr")
plot(trainperfROCCART,main = "ROC curve")


#Building the ROC curve and lift charts
testpredROCCART <- prediction(testTheraCART$predict.score[,2], testTheraCART$Personal_Loan)
testperfROCCART <- performance(testpredROCCART, "tpr", "fpr")
plot(testperfROCCART,main = "ROC curve")


##KS

max(trainperfROCCART@y.values[[1]]-trainperfROCCART@x.values[[1]]) #0.9125843
max(testperfROCCART@y.values[[1]]-testperfROCCART@x.values[[1]]) #0.9242871

##AUC

auctrainCART <- performance(trainpredROCCART,"auc"); 
auctrainCART <- as.numeric(auctrainCART@y.values)
auctrainCART #0.9814722


auctestCART <- performance(testpredROCCART,"auc"); 
auctestCART <- as.numeric(auctestCART@y.values)
auctestCART #0.9825877


##gini

ineq(trainTheraCART$predict.score,"gini")#0.4959807
ineq(testTheraCART$predict.score,"gini")#0.4959692


##Concordance

Concordance(actuals=trainTheraCART$Personal_Loan,predictedScores = trainTheraCART$predict.score)
#$Concordance - 0.002093869
#$Discordance-[1] 0.9979061
#$Tied-[1] 0
#$Pairs-[1] 1063104
Concordance(actuals=testTheraCART$Personal_Loan,predictedScores = testTheraCART$predict.score)
#$Concordance -[1] 0.0018539
#$Discordance-[1] 0.9981461
#$Tied-[1] 0
#$Pairs-[1] 195264







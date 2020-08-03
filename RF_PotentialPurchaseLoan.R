#======================================================
#     PotentialPurchaseLoan - RF
#======================================================

#Load the required packages
library(randomForest)

#Set seed to constant value
set.seed(1000)

#Build RF Model
TheraRFModel = randomForest(Personal_Loan ~ ., data = trainTheraRF,ntree=501, mtry = 3, nodesize = 10,importance=TRUE)

print(TheraRFModel)
TheraRFModel$confusion

#plotting the RF Model
plot(TheraRFModel)

##Importance of variables 
randomForest::importance(TheraRFModel)
varImpPlot(TheraRFModel)


##Tune up the RF model to find out the best mtry

tuneTheraRFModel = tuneRF(x=trainTheraRF[,c(-9)],y=trainTheraRF$Personal_Loan,mtrystart = 3,stepfactor=1.5,ntree=401,improve=0.0001,
                  nodesize=10,trace=TRUE,plot=TRUE,doBest=TRUE,importance=TRUE)

#At mtry = 6 , the OOB error is less and this value is #picked to rebuild the RFmodel.


TheraRFModel2 = randomForest(Personal_Loan ~ ., data = trainTheraRF, 
                            ntree=501, mtry = 6, nodesize = 10,
                            importance=TRUE)

print(TheraRFModel2)
TheraRFModel$confusion
TheraRFModel2$confusion

#plotting the RF Model
plot(TheraRFModel2)

##Importance of variables 
randomForest::importance(TheraRFModel2)
varImpPlot(TheraRFModel2)

library(caret)
library(e1071)


trainTheraRF$RF.Pred = predict(TheraRFModel2,data=trainTheraRF,type="class")
trainTheraRF$RF.Score = predict(TheraRFModel2,data=trainTheraRF,type="prob")[,"1"]
testTheraRF$RF.Pred = predict(TheraRFModel2,testTheraRF,type="class")
testTheraRF$RF.Score = predict(TheraRFModel2,testTheraRF,type="prob")[,"1"]

tbl=table(testTheraRF$Personal_Loan, testTheraRF$RF.Pred)
tbltrain=table(trainTheraRF$Personal_Loan, trainTheraRF$RF.Pred)
tbltrain
tbl

#Accepting personal loan
Person_loan_accepted = tbl[4]/sum(tbl[2]+tbl[4])
Person_loan_acceptedtrain = tbltrain[4]/sum(tbltrain[2]+tbltrain[4])
Person_loan_acceptedtrain
Person_loan_accepted
#Not accepting personal loan
Person_loan_notaccepted = tbl[1]/sum(tbl[1]+tbl[3])
Person_loan_notacceptedtrain = tbltrain[1]/sum(tbltrain[1]+tbltrain[3])
Person_loan_notacceptedtrain
Person_loan_notaccepted

#Highest values in the Meandecreaseaccuracy and #meandecreasegini from the importance fuction of RF model #indicated the variables that is more important which majorly #impacts in accepting the Personal Loan.
#From the tuned RF model importance output, Income,education, #Family members variables majorly helps in predicting the #acceptance of the personal loan.
#varImPlot helps to plot the important variables picked by RF #model in a graph and the variables on the right side holds #most importance for persona loan acceptance. From the RF #tuned graph, we can see that Income, Education,CC Avg,Family #members are variables that is towards the right of the #plotted graph.

## RF Model - ROC
library(ROCR)
library(ineq)

trainpredROCRF <- prediction(trainTheraRF$RF.Score,trainTheraRF$Personal_Loan)
preftrainROCRF = performance(trainpredROCRF,"tpr","fpr")
plot(preftrainROCRF)

testpredROCRF = prediction(testTheraRF$RF.Score,testTheraRF$Personal_Loan)
preftestROCRF = performance(testpredROCRF,"tpr","fpr")
plot(preftestROCRF)

##KS

max(preftrainROCRF@y.values[[1]]-preftrainROCRF@x.values[[1]])#0.9389749
max(preftestROCRF@y.values[[1]]-preftestROCRF@x.values[[1]])#0.9706244

##AUC

auctrainRF=performance(trainpredROCRF,"auc")
as.numeric(auctrainRF@y.values)#0.9966513
auctestRF=performance(testpredROCRF,"auc")
as.numeric(auctestRF@y.values) #0.9979797


##gini

ineq(trainTheraRF$RF.Score,"gini")#0.8947148
ineq(testTheraRF$RF.Score,"gini")#0.8992329


##Concordance

Concordance(actuals=trainTheraRF$Personal_Loan,predictedScores = trainTheraRF$RF.Score)
#$Concordancev- 0.9966476
#$Discordance - 0.003352447
#$Tied- -4.206704e-17
#$Pairs - 1063104
Concordance(actuals=testTheraRF$Personal_Loan,predictedScores = testTheraRF$RF.Score)
#$Concordance 0.9979617
#$Discordance 0.002038266
#$Tied  3.035766e-18
#$Pairs 195264


rm(list=ls())
setwd("C:/Users/yusha/OneDrive/Desktop/SDSU/Project 620 Group")

library(ISLR)
library(dplyr)
library(caret)
library(psych)
library(ggplot2)
library("VIM")
library(ROSE)
library(DMwR)
library(AppliedPredictiveModeling)
library(pROC)
 library("e1071")
library(ROCR)

# load the merged dataset into R

Data<-read.csv("Data_Final.csv",header=T,na.strings="")
str(Data)  # 78369 observation of 19 varaible

names(Data)
summary(Data)
# change negative value to positvie
Past_avg_custom_otherdiscount<-abs(Data$Past_avg_custom_otherdiscount)
past_custom_disc_per_brand<-abs(Data$past_custom_disc_per_brand)
Data<- Data[,-c(17,18)]
Data<- mutate(Data,Past_avg_custom_otherdiscount,past_custom_disc_per_brand)
summary(Data)
str(Data)


###############################################preprocess the dataset
# since some of the features are highly skewed,we need preprocess data to scale the predictors
pp_Data<- preProcess(Data[, -c(3,4,5,9,12,13,14,15,17)], 
                     method = c("BoxCox","center", "scale"))
pp_Data #can see results of processing

#now we juse need to apply the processing model to the data
transformed <- predict(pp_Data, newdata =Data)
head(transformed)
hist(transformed$Past_avg_custom_otherdiscount)

transformed$redemption_status<-as.factor(transformed$redemption_status)

# divide data into train and test.  Campaign id smaller than 14 as training data
traindata<- subset(transformed,transformed$campaign_id<14)   # 78%
Testdata<-subset(transformed,transformed$campaign_id>14)     #22%

# use smote and rose to balance train data

#lets relevel the redeem factor so Yes becomes the Positive class by redeem (Sensitivity)
levels(traindata$redemption_status) <- c("NO", "YES")
traindata$redemption_status <- relevel(traindata$redemption_status,ref="YES") #yes has 620,No has 60531
table(traindata$redemption_status)


levels(Testdata$redemption_status) <- c("NO", "YES") 
Testdata$redemption_status <- relevel(Testdata$redemption_status,ref="YES")  # yes has 109,No has 17109
table(Testdata$redemption_status)

traindata<-traindata[,-c(1,2,3)]  # remove non-uesful features
Testdata<-Testdata[,-c(1,2,3)]

###############balance dataset

set.seed(50)
smote.train<- SMOTE(redemption_status ~ .,data=traindata)
table(smote.train$redemption_status)  # No has 2480,  Yes has 1860


set.seed(50)
rose.train<- ROSE(redemption_status~.,data=traindata)$data
table(rose.train$redemption_status) 
rose.train$redemption_status <- relevel(rose.train$redemption_status,ref="YES")

library(doParallel)

#register 12 cores to split of training
cl <- makePSOCKcluster(12)
clusterSetRNGStream(cl, 5672)  #set seed for everymember of cluster
registerDoParallel(cl)


#Test both Smoted and Rosed data frames on Logistic Regression firstly

#setup control function for resampling and binary classification performance
#using 5 fold cross validation
ctrl <- trainControl( method="cv", number=5,
                      classProbs=TRUE,
                     #function used to measure performance
                     summaryFunction = twoClassSummary,
                     savePredictions=T) #binary class

names(smote.train)


##################################Model building start from here################
                   
##########logistic regression model ###############
set.seed(199)  #ALWAYS USE same SEED ACROSS trains to ensure identical cv folds

# create logistical regression model before balance data
glm_train<-  train(redemption_status ~ ., data=traindata, method="glm", metric="ROC", trControl=ctrl) 
glm_train   #ROC 96.93%  
getTrainPerf(glm_train)
summary(glm_train) #model summary
varImp(glm_train) #rank important variables

# Test data
glm_predict <- predict(glm_train, Testdata)
confusionMatrix(glm_predict, Testdata$redemption_status)   # Balanced accuracy 53%


#############now create model using data after balance 
set.seed(199)
L.smote.glm<-  train(redemption_status~ ., data=smote.train, method="glm",metric="ROC", family="binomial",trControl=ctrl)
L.smote.glm  # ROC 97.78%   TPR 96.8%  FPR:9.3%
summary(L.smote.glm)  
varImp(L.smote.glm)

# Test data
L.smote.p <- predict(L.smote.glm, Testdata)    
confusionMatrix(L.smote.p, Testdata$redemption_status)     # accuray 92%, TPR is 85.32%, FPR 8%


##### use ROSE method
set.seed(199)
L.rose.glm<-train(redemption_status~ ., data = rose.train,
                   trControl = ctrl, metric = "ROC", method = "glm")    
L.rose.glm
getTrainPerf(L.rose.glm)  # ROC 95.61%
summary(L.rose.glm) 
varImp(L.rose.glm)

Lrose.predict <- predict(L.rose.glm, Testdata)
confusionMatrix(Lrose.predict, Testdata$redemption_status)  # 88% accuracy, tpr 99%,FPR  12%

#compare the performance of all models trained today
rValues <- resamples(list(rose=L.rose.glm, smote=L.smote.glm))

bwplot(rValues, metric="ROC")
bwplot(rValues, metric="Sens") #Sensitvity
bwplot(rValues, metric="Spec")

#calculate ROC and AUC on test data

Log.smote.prob <- predict(L.smote.glm, Testdata, type="prob") 
Log.rose.prob<- predict(L.rose.glm, Testdata, type="prob")


#for smote method
score_Log_smote<-Log.smote.prob[,c("YES")]
actual_log<-Testdata$redemption_status=='YES'
pred_smote<-prediction(score_Log_smote,actual_log)
perf_smote<-performance(pred_smote,"tpr","fpr")

# for rose method
score_log.rose<-Log.rose.prob[,c("YES")]
pred_rose<-prediction(score_log.rose,actual_log)
perf_rose<-performance(pred_rose,"tpr","fpr")


plot(perf_smote, lwd=2, xlab="False Positive Rate (FPR)", col="blue",ylab="True Positive Rate (TPR)")
plot(perf_rose,add=T, col="red")
abline(a=0,b=1,col="gray50",lty=3)
legend(x=.34, y=.3, cex=0.5, legend=c("Smote.log","Rose.log"), col=c("blue", "red"), lwd=5)

AUC.SMOTE.log<-performance(pred_smote,"auc")
auc_smote.log<-unlist(slot(AUC.SMOTE.log,"y.values"))
auc_smote.log   # AUC is  96.88%


AUC.ROSE.log<-performance(pred_rose,"auc")
auc.rose.log<-unlist(slot(AUC.ROSE.log,"y.values"))
auc.rose.log   # AUC is 97.15%



dev.off()

##########################svm model


# training a simple linear SVM on SMOTE training sets
library(kernlab)
set.seed(199)
svm_smote.simple<- ksvm( redemption_status~ ., data = smote.train,
                          kernel = "vanilladot")
svm_smote.simple 

## Evaluating model performance ----
# predictions on testing dataset
svm_predictions <- predict(svm_smote.simple , Testdata)
head(svm_predictions)
#confusion matrix
confusionMatrix(svm_predictions,Testdata$redemption_status)  #90.6% balanced accuracy

# training a simple linear SVM on ROSE
set.seed(199)
svm_rose.simple <- ksvm( redemption_status~ ., data = rose.train,
                        kernel = "vanilladot")
svm.rose_predictions <- predict(svm_rose.simple , Testdata)
confusionMatrix(svm.rose_predictions,Testdata$redemption_status) # 93.04%     



##svm with radial kernel
# SVM model for SMOTE training set
m.svm <- train(redemption_status~ ., 
               trControl = ctrl,
               metric = "ROC",
               data = smote.train, 
               method = "svmRadial")

plot(m.svm) #view performance against parameters
getTrainPerf(m.svm)
svm.SMOTE_predictions <- predict(m.svm, Testdata)
#confusion matrix
confusionMatrix(svm.SMOTE_predictions,Testdata$redemption_status)  #83.93% balanced accuracy



# looks like rose method performance better than smote method

###############################################################################################3  
#decision tree model building
library("rpart")
library("rpart.plot")
library(tree)

#review root prob/entropy
table(smote.train$redemption_status)

2480/4340  #didn't redeem
1860/4340  #redeemed

#base entropy
-(.5714*log2(.5714) + .4286*log2(.4286))

# Make a simple decision tree by only keeping the categorical variables
fit <- rpart(redemption_status ~.,method="class", data=smote.train,control=rpart.control(minsplit=1),
         parms=list(split='information'))

par(mfrow = c(1,1)) 

# Plot tree
rpart.plot(fit,box.palette="orange",main= "Tree")

#lets prune the tree to avoid overfitting using cross validation

printcp(fit) #display crossvalidated error for each tree size
plotcp(fit) #plot cv error

#select Complexity parameter (CP) with lowest crossvalidated error 
#cp restricts the size of tree based on the cost of adding a node being less than cp value


#we can grab this from the plotcp table automatically with 
opt.cp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

#lets prune the tree
fit.pruned <- prune(fit, cp=opt.cp)

#lets review the final tree
rpart.plot(fit.pruned)

#test data
#classes
table(Testdata$redemption_status)

# Predict using trained model on unseen test data
test.preds<- predict(fit, Testdata, type=c("class")) #extract classes defaults to selected class greater than 50%

table(test.preds) #16120 predict no and 1098 yes

#compare against actual ground truth of test set

results <- table(test.preds,Testdata$redemption_status) 
results  
confusionMatrix(test.preds, Testdata$redemption_status)   #balanced accuracy 90.6%

#random forest approach 

library(randomForest)
set.seed(199)
modelLookup("rf")
m.rf <- train(redemption_status~ ., data=smote.train,
              trControl = ctrl,
              metric = "ROC", #using AUC to find best performing parameters
              tuneLength=9,
              method = c("rf") )
m.rf

p.rf<- predict(m.rf,Testdata[-1])
confusionMatrix(p.rf,Testdata$redemption_status)   #balanced accuracy 86%


####################XGBoost
library(xgboost)
set.seed(199)
ROSE_xg <- train(redemption_status ~ ., 
                       method = "xgbTree",
                       metric = "ROC",
                       trControl = ctrl,
                       data = rose.train,
                       verbose=TRUE)
varImp(ROSE_xg)

ROSE_xg <- confusionMatrix(predict(ROSE_xg,Testdata[,-1]), Testdata$redemption_status)#Balanced Accuracy 93%

ROSE_xg    

#release computers cores
stopCluster(cl)





  

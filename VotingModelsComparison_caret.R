
#Model comparisons using CARET package
#Which model provides the most accurate prediction of bugcovering question
#Feature: majorityVote or rankingVote or threshold vote
#Naive Bayes,KNN, RandomForest, SVM, GLM, GLM Bayes

#Algorithm
#1- Train the models with 70% of the dataset and validate with 30% holdout set. Other splits (80/20, 90/10) did not produce better results.
#2- Select models within same method using ROC. Other metrics (Kappa and Accuracy) did not produce better results.
#3- Select models across methods using Sensitivy (recall). I chose recall instead of Specificity or ROC because our goal is to minimize false negatives (i.e., locate all faults)
#4- Estimate the threshold for the aggregation metrics using validation set
#4.1 Predict the if each question is bugcovering or not (use the holdout set of 30%)
#4.2 Among all question predicted as bugcovering take the maximum value of the metric
#Threshold(AM.1) = min(majorityVote(predictedBugCoveringQuestions))
#Threshold(AM.2) = min(thresholdVote(predictedBugCoveringQuestions))
#Threshold(AM.3) = max(rankingVote(predictedBugCoveringQuestions))

#This guarantees that we will prioritize the detection of true bug-covering question at the expense of
#generating false postives.


# Code --------------------------------------------------------------------

#libraries
install.packages("caret")
install.packages("klaR")
install.packages("httpuv")
install.packages("pROC")
#-------------------------------------------------------
library(httpuv)
library(klaR)
library(caret)
library(pROC) # for AUC calculations
library(devtools)

# Import data -------------------------------------------------------------

source("C://Users//Chris//Documents//GitHub//ML_VotingAggregation//aggregateAnswerOptionsPerQuestion.R");
source("C://Users//Chris//Documents//GitHub//ML_VotingAggregation//calculateValidationErrors.R");

#HOw people are crowdsourcing? How people are fixing bugs?
#No continuous interation (iteration?)
#Complementary capacities
#validate assumptions
#allow others to extend the worflow?

summaryTable <- runMain();

#I need to guarantee that some examples (i.e., failing methods)
#do not dominate the training or testing sets. To do that, I need to get a 
#close to equal proportion of examples in both sets

#Scramble the dataset before extracting the training set.
set.seed(8850);
g<- runif((nrow(summaryTable))); #generates a random distribution
summaryTable <- summaryTable[order(g),];

# Convert columns to numeric ----------------------------------------------
summaryTable<- data.frame(summaryTable, stringsAsFactors = FALSE)
summaryTable[,"rankingVote"] <- as.numeric(unlist(summaryTable[,"rankingVote"])); #AM.3
summaryTable[,"Yes.Count"] <- as.numeric(unlist(summaryTable[,"Yes.Count"])); #AM.2
summaryTable[,"majorityVote"] <- as.numeric(unlist(summaryTable[,"majorityVote"])); #AM.1
summaryTable[,"explanatoryVariable"] <- summaryTable[,"Yes.Count"];
summaryTable$bugCoveringLabels <- as.character(summaryTable$bugCovering);
summaryTable$bugCoveringLabels<- replace(summaryTable$bugCoveringLabels,summaryTable$bugCoveringLabels=="FALSE", "F");
summaryTable$bugCoveringLabels<- replace(summaryTable$bugCoveringLabels,summaryTable$bugCoveringLabels=="TRUE", "T");
summaryTable$bugCoveringLabels<- as.factor(summaryTable$bugCoveringLabels);


# Split data for training and validating ----------------------------------
totalData.size <- dim(summaryTable)[1];
training.size <- trunc(totalData.size * 0.7);

training.df <- as.data.frame(summaryTable[1:training.size-1,]);
validation.df <- as.data.frame(summaryTable[training.size:totalData.size,]);


# Create trainControl to be reused by all models --------------------------
#Guarantees that we are going to use the exact same datasets for all models
myFolds <- createFolds(training.df[,"explanatoryVariable"] , k = 10); 

#larger K implies less bias (overfitting). However, larger K implies larger variance, i.e., 
#the prediction has large variation. The reason is that larger K makes each training data large and
#very similar.
#nice explanation here: https://stats.stackexchange.com/questions/27730/choice-of-k-in-k-fold-cross-validation

# Create reusable trainControl object: myControl
kFoldControl <- trainControl(
  index = myFolds, #Train with 9 folds and validate with one
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE, #
  savePredictions = TRUE, #
  summaryFunction = twoClassSummary
);
 

#######################
# Generate each model #

##############


# Naive Bayes -------------------------------------------------------------
nb<-  train(bugCoveringLabels ~ explanatoryVariable,training.df, method="nb", metric="ROC", trControl=kFoldControl);

#nb_auc<-  train(bugCoveringLabels ~ explanatoryVariable,training.df, method="nb", trControl=kFoldControl,  maximize = TRUE);

nb
#nb_auc


#AM.1

#AM.2
#usekernel  ROC        Sens       Spec     
#FALSE      0.7136796  0.9380167  0.4383509
#TRUE       0.7241444  0.9302171  0.4439717

#AM.3:
# usekernel  ROC        Sens       Spec     
# FALSE      0.7546970  0.9031409  0.5664596
# TRUE       0.7534538  0.9270484  0.5095109


# KNN ---------------------------------------------------------------------
knn <- train(bugCoveringLabels ~ explanatoryVariable,training.df, method="knn", metric="ROC", trControl=kFoldControl);

#knn_auc <- train(bugCoveringLabels ~ explanatoryVariable,training.df, method="knn", metric="Kappa", trControl=kFoldControl);

#knn_sens
knn
#Aggre. k  ROC        Sens       Spec
#AM.1:
#AM.2: 7  0.8338240  0.9851064  0.0750000
#AM.3: 5  0.8290137  0.9778947  0.1340909

# Random Forest -----------------------------------------------------------
rf <- train(bugCoveringLabels ~ explanatoryVariable,training.df, method="rf", metric="ROC", trControl=kFoldControl);

# rf_Accuracy<- train(bugCoveringLabels ~ explanatoryVariable,training.df, method="rf", metric="Accuracy", trControl=kFoldControl);
# rf_KAPPA<- train(bugCoveringLabels ~ explanatoryVariable,training.df, method="rf", metric="Kappa", trControl=kFoldControl);
# rf_Accuracy
# rf_ROC
# rf_KAPPA
rf
#Aggre.  ROC        Sens       Spec     
#AM.1:
#AM.2: 0.7938766  0.8638338  0.4812422
#AM.3: 0.8124545  0.8762876  0.5132246



# GBM Gradient Boosting Trees  -------------------------------------------------------------
#Cannot use GBM or xgBoostTree because I have only one feature

# gbm <- train(bugCoveringLabels ~ explanatoryVariable,training.df,
#                 method="gbm", metric="ROC", trControl=kFoldControl);
# 
# #gbm_Accuracy <- train(bugCoveringLabels ~ explanatoryVariable,training.df,
#  #                method="gbm", metric="Accuracy", trControl=kFoldControl);
# 
# gbm
# 
# #gbm_Accuracy

# GLM ---------------------------------------------------------------------
glmModel<- train(bugCoveringLabels ~ explanatoryVariable,training.df, method="glm", metric="ROC",trControl=kFoldControl)

glmModel
#Aggre. ROC        Sens       Spec     
#AM.1:
#AM.2:  0.8276035  0.9338004  0.4748377
#AM.3:  0.8747113  0.9237826  0.4507378

# Bayes GLM ---------------------------------------------------------------
bayesglm<- train(bugCoveringLabels ~ explanatoryVariable,training.df, method="bayesglm", metric="ROC", trControl=kFoldControl);

bayesglm
# Aggre.  ROC        Sens       Spec     
#AM.1:
#AM.2: 0.8797804  0.9338004  0.4748377
#AM.3: 0.8898239  0.9322932  0.4371014


#Not part of Caret and produced results similar to bayesglm
#glmBoost<- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="glmBoost", trControl=kFoldControl);

#Not working
#glmnet<- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="glmnet", trControl=kFoldControl);

#glmnet model is a more sophisticated solution that uses penalty terms to reduce the magnitude 
#of the two GLM coeficients. The goal of GMLNet is to explain as much variance in the model.
#The trade-off is that glmnet accepts more bias in the data (more risk of overfitting)
#In any case, both glmnet and glm produce the exact same results for my data, therefore I favored
#the simplest model.

# SVM ---------------------------------------------------------------------
svmLinear <- train(bugCoveringLabels ~ explanatoryVariable,training.df, method="svmLinear", metric="ROC", trControl=kFoldControl);
svmLinear2 <- train(bugCoveringLabels ~ explanatoryVariable,training.df, method="svmLinear2", metric="ROC",trControl=kFoldControl);
svmLinearWeights <- train(bugCoveringLabels ~ explanatoryVariable,training.df, method="svmLinearWeights", metric="ROC",trControl=kFoldControl);

svmLinear
#Aggre.    ROC        Sens       Spec     
#AM.1:
#AM.2: 0.598301  0.9589485  0.2627181
#AM.3: 0.6798618  0.9643897  0.2357955

svmLinear2
#Aggre. cost  ROC        Sens       Spec     
#AM.1: 
#AM.2: 1.00  0.8009603  0.9616318  0.2776515
#AM.3: 0.50  0.7713566  0.9757671  0.1613636

svmLinearWeights
#Aggre. cost  weight  ROC        Sens       Spec    
#AM.1:
#AM.2: 1.00  2       0.8016421  0.9082113  0.6105458
#AM.3: 0.50  3       0.8102679  0.8439114  0.5645059


# Compare models ----------------------------------------------------------


###################
#Visualize models
resampleList<-resamples(list(svmLinear=svmLinear,svmLinear2=svmLinear2,svmLinearWeights=svmLinearWeights,
                           glm=glmModel,bayesglm=bayesglm, rf=rf, knn=knn, nb=nb
                             ));

bwplot(resampleList,metric="Sens")
densityplot(resampleList,metric="ROC")
dotplot(resampleList,xlim=range(0,1),metric="Sens")
#Compare two best
twoBestList <- resamples(list(svmLinear2=svmLinear2,bayesglm=bayesglm));
xyplot(twoBestList,xlim=range(0,1), metric="ROC")

#compare second and third best
secodThirdBestList <- resamples(list(knn=knn,bayesglm=nb));
xyplot(secodThirdBestList,xlim=range(0,1), metric="ROC")

########################################
#Model selection results
#Best model for ranking (AM.3)
#knn

#Best model for Threshold (AM.1)


#Best model for Majority voting (AM.2)
#svmLinearWeights is tied with bayesglm

##################################################
#Predict n based on best model
compareTable <- data.frame(validation.df$Question.ID,
                           validation.df$explanatoryVariable,
                           validation.df$bugCoveringLabels,
                           predict(nb,validation.df),
                           predict(knn,validation.df),
                           predict(rf,validation.df),
                           predict(bayesglm,validation.df),
                           predict(svmLinear,validation.df),
                           predict(svmLinear2,validation.df),
                           predict(svmLinearWeights,validation.df)
);

colnames(compareTable) <- c("Question.ID","explanatoryVariable","actual","nb","knn","rf",
                            "bayesGLM","svmLinear","svmLinear2","svmWeights");

compareTable[compareTable$actual=="T",];

####################################################
#Predict n based on best model (highest precision)
compareTable <- data.frame(validation.df$explanatoryVariable,
                           validation.df$bugCoveringLabels,
                           predict(nb,validation.df));
colnames(compareTable) <- c("explanatoryVariable","actual","predicted");
compareTable[compareTable$actual=="T",]
predictedBugCoveringList<-compareTable[compareTable$predicted=="T",];
predictedBugCoveringList$explanatoryVariable;
predictedBugCoveringList;

# Estimate n (results of mininal) ---------------------------------------------------

#Computing the miminum value of n that predicted bugCovering True
min(predictedBugCoveringList$explanatoryVariable);

max(predictedBugCoveringList$explanatoryVariable);

# Validate models -------------------------------------------------------------------

#Results from model prediction on a validation set (holdout set)
validationOutcomes <- matrix(ncol = 11, nrow = 0);
colnames(validationOutcomes)<- c("ModelName","AUC","accuracy","trueNegatives","truePositives",
                      "falseNegatives","falsePositives","precision","recall","specificity","sensitivity");

validationOutcomes <- rbind(validationOutcomes, calculateValidationErrors(nb,"NaiveBayes",validation.df));
validationOutcomes <- rbind(validationOutcomes, calculateValidationErrors(knn,"KNearestNeighbor",validation.df));
validationOutcomes <- rbind(validationOutcomes, calculateValidationErrors(rf,"RandomForest",validation.df));
validationOutcomes <- rbind(validationOutcomes, calculateValidationErrors(glmModel,"Generalized Linear Model",validation.df));
validationOutcomes <- rbind(validationOutcomes, calculateValidationErrors(bayesglm,"Generalized Linear Model Bayes",validation.df));
validationOutcomes <- rbind(validationOutcomes, calculateValidationErrors(svmLinear,"SVM Linear",validation.df));
validationOutcomes <- rbind(validationOutcomes, calculateValidationErrors(svmLinear2,"SVM Linear 2",validation.df));
validationOutcomes <- rbind(validationOutcomes, calculateValidationErrors(svmLinearWeights,"SVM Weights",validation.df));

write.csv(validationOutcomes, file = ".//validationErrors.csv");


#Model comparisons using CARET package
#Which model provides the best predictions?
#NaiveBayes,KNN, RandomForest, SVN, or GLM

#libraries
library(caret)


####################
#Import data

source("C://Users//chris//OneDrive//Documentos//GitHub//ML_VotingAggregation//aggregateAnswerOptionsPerQuestion.R");
summaryTable <- runMain();
#summaryTable <- data.frame(summaryTable);

#I need to guarantee that some examples (i.e., failing methods)
#do not dominate the training or testing sets. To do that, I need to get a 
#close to equal proportion of examples in both sets

#Scramble the dataset before extracting the training set.
set.seed(8850);
g<- runif((nrow(summaryTable))); #generates a random distribution
summaryTable <- summaryTable[order(g),];

##################################################
# Create trainControl to be reused by all models #



#convert columns to numeric
summaryTable<- data.frame(summaryTable, stringsAsFactors = FALSE)
summaryTable[,"rankingVote"] <- as.numeric(unlist(summaryTable[,"rankingVote"])); 
summaryTable$bugCoveringLabels <- as.character(summaryTable$bugCovering);
summaryTable$bugCoveringLabels<- replace(summaryTable$bugCoveringLabels,summaryTable$bugCoveringLabels=="FALSE", "F");
summaryTable$bugCoveringLabels<- replace(summaryTable$bugCoveringLabels,summaryTable$bugCoveringLabels=="TRUE", "T");
summaryTable$bugCoveringLabels<- as.factor(summaryTable$bugCoveringLabels);


# sub = sample(nrow(summaryTable), floor(nrow(summaryTable) * 1))
# train = summaryTable[sub,]
# test = summaryTable[-sub,]
# 
# xTrain = train[,"rankingVote"]
# yTrain = as.factor(train$bugCovering)
# 
# nb.fit = train(bugCoveringLabels ~ rankingVote,summaryTable,'nb',trControl=trainControl(method='cv',number=5))

# Create custom indices: myFolds
#Guarantees that we are going to use the exact 
#same datasets for all models
myFolds <- createFolds(summaryTable[,"rankingVote"] , k = 10); 

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
# Naive Bayes

NB_model<- train(bugCoveringLabels ~ rankingVote,summaryTable, method="nb", trControl=kFoldControl);
NB_model

# usekernel  ROC        Sens       Spec     
# FALSE      0.7546970  0.9031409  0.5664596
# TRUE      0.7534538  0.9270484  0.5095109

######
# KNN

KNN_model <- train(bugCoveringLabels ~ rankingVote,summaryTable, method="knn", trControl=kFoldControl);
KNN_model

# k  ROC        Sens       Spec     
# 5  0.8290137  0.9778947  0.1340909

################
# Random Forest


RF_model<- train(bugCoveringLabels ~ rankingVote,summaryTable, method="rf", trControl=kFoldControl);
RF_model

######
# GLM

GLM_model<- train(bugCoveringLabels ~ rankingVote,summaryTable, method="bayesglm", trControl=kFoldControl);
GLM_model

#glm
#ROC        Sens       Spec     
#0.8747113  0.9237826  0.4507378

#glmnet
#ROC        Sens       Spec     
#0.8747113  0.9237826  0.4507378

#glmBoost
#mstop  ROC        Sens       Spec     
#150    0.8898239  0.9259331  0.4174045

# bayesglm
# ROC        Sens       Spec     
# 0.8898239  0.9322932  0.4371014

#glmnet model is a more sophisticated solution that use penalty terms to reduce the magnitude 
#of the two GLM coeficients. The goal of GMLNet is to explain as much variance in the model.
#The trade-off is that glmnet accepts more bias in the data (more risk of overfitting)
#In any case, both glmnet and glm produce the exact same results for my data, therefore I favored
#the simplest model.

######
# SVM

SVM_model <- train(bugCoveringLabels ~ rankingVote,summaryTable, method="svmLinear", trControl=kFoldControl);
SVM_model

#svmLinear
#  ROC        Sens       Spec     
#0.6798618  0.9643897  0.2357955

#svmLinear2
#cost  ROC        Sens       Spec     
#0.50  0.7713566  0.9757671  0.1613636

#svmLinearWeights
# cost  weight  ROC        Sens       Spec    
# 0.50  3       0.8102679  0.8439114  0.5645059


###################
# Compare models


#compare models by
#sensitivity
#specificity
#ROC


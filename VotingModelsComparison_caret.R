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


sub = sample(nrow(summaryTable), floor(nrow(summaryTable) * 1))
train = summaryTable[sub,]
test = summaryTable[-sub,]

xTrain = train[,"rankingVote"]
yTrain = as.factor(train$bugCovering)

nb.fit = train(bugCoveringLabels ~ rankingVote,summaryTable,'nb',trControl=trainControl(method='cv',number=5))

# Create custom indices: myFolds
#Guarantees that we are going to use the exact 
#same datasets for all models
myFolds <- createFolds(summaryTable[,"rankingVote"] , k = 10) 

# Create reusable trainControl object: myControl
kFoldControl <- trainControl(
  index = myFolds,
  classProbs = TRUE, # IMPORTANT!
 # verboseIter = TRUE,
 # savePredictions = TRUE,
  summaryFunction = twoClassSummary
);

trControl=trainControl(
    method='cv',
    number=5,
   
  classProbs = TRUE, # IMPORTANT! 
   savePredictions = TRUE,
   summaryFunction = twoClassSummary

       );

#######################
# Generate each model #

##############
# Naive Bayes

NB_model<- train()

######
# KNN

KNN_model;

################
# Random Forest

RF_model;

######
# GLM

#nb.fit = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=5))

GLM_model<- train(bugCoveringLabels ~ rankingVote,summaryTable, method="glm", trControl=kFoldControl);


GLM_model
######
# SVM

SVM_model;

###################
# Compare models


#compare models by
#sensitivity
#specificity
#ROC


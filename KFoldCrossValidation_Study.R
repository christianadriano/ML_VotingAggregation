#K-Fold cross-validation
#Compare results from different values of K
#KNN, RandomForest, GLM, or SVN

#libraries
#library(dplyr) # for data manipulation
library(caret) # for model-building
#library(DMwR) # for smote implementation
#library(purrr) # for functional programming (map)
install.packages("pROC")
library(pROC) # for AUC calculations



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
summaryTable[,"Yes.Count"] <- as.numeric(unlist(summaryTable[,"Yes.Count"]));
summaryTable[,"majorityVote"] <- as.numeric(unlist(summaryTable[,"majorityVote"]));
summaryTable[,"explanatoryVariable"] <- summaryTable[,"majorityVote"];
summaryTable$bugCoveringLabels <- as.character(summaryTable$bugCovering);
summaryTable$bugCoveringLabels<- replace(summaryTable$bugCoveringLabels,summaryTable$bugCoveringLabels=="FALSE", "F");
summaryTable$bugCoveringLabels<- replace(summaryTable$bugCoveringLabels,summaryTable$bugCoveringLabels=="TRUE", "T");
summaryTable$bugCoveringLabels<- as.factor(summaryTable$bugCoveringLabels);

## Table to store the outcomes from the training and model selection and prediction
outcome <- matrix(ncol = 11, nrow = 40);
colnames(outcome)<- c("kfolds","trainingError","AUC","accuracy","trueNegatives","truePositives",
                      "falseNegatives","falsePositives","precision","recall","specificity","sensitivity");

for(folds in 2:40){
  
  # Create custom indices: myFolds
  #Guarantees that we are going to use the exact same datasets for all models
  
  #larger K implies less bias (overfitting). However, larger K implies larger variance, i.e., 
  #the prediction has large variation. The reason is that larger K makes each training data large and
  #very similar.
  #nice explanation here: https://stats.stackexchange.com/questions/27730/choice-of-k-in-k-fold-cross-validation
  
  # Create reusable trainControl object: myControl
  kFoldControl <- trainControl(
    index = myFolds, #Train with k folds and validate with one
    classProbs = TRUE, # IMPORTANT!
    verboseIter = TRUE, #
    savePredictions = TRUE, #
    summaryFunction = twoClassSummary
  );
  
  #knnModel <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="knn", trControl=kFoldControl);
  #rfModel<- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="rf", trControl=kFoldControl);
  #bayesglmModel<- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="bayesglm", trControl=kFoldControl);
  fitModel <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, 
                                 method="knn", trControl=kFoldControl, metric="Spec");
  
  #check if n changes if I optimize for False Negatives (Sensitivity), but need to know what is the Positive class for training
  
  fitModel
  bugCoveringPredicted <- predict(fitModel,newdata = summaryTable);
  matrixResult<- confusionMatrix(data=bugCoveringPredicted,summaryTable$bugCoveringLabels, positive="T");
  trueNegatives<- matrixResult$table[1,1];
  truePositives<- matrixResult$table[2,2];
  falseNegatives<- matrixResult$table[1,2];
  falsePositives<- matrixResult$table[2,1];?
  
  #compute AUC
  aucValue<- roc(response = as.numeric(summaryTable$bugCoveringLabels),predictor = as.numeric(bugCoveringPredicted)) %>% auc();
  aucValue<-as.numeric(aucValue);
  
  accuracy <- (truePositives + trueNegatives) / (truePositives + trueNegatives + falsePositives + falseNegatives);
  trainingError <- 1-accuracy;
  precision <- truePositives / (truePositives + falsePositives);
  recall <- truePositives / (truePositives + falseNegatives);
  sensitivity <- trueNegatives / (trueNegatives + falsePositives);
  specificity <- truePositives / (truePositives + falseNegatives);
  
   row <- folds-1;
 #row<-1
  outcome[row,"kfolds"]<-folds;
  outcome[row,"trainingError"]<-trainingError;
  outcome[row,"AUC"] <- aucValue;
  outcome[row,"accuracy"]<-accuracy;
  outcome[row,"trueNegatives"]<-trueNegatives;
  outcome[row,"truePositives"]<-truePositives;
  outcome[row,"falseNegatives"]<-falseNegatives;
  outcome[row,"falsePositives"]<-falsePositives;
  outcome[row,"precision"]<-precision;
  outcome[row,"recall"]<-recall;
  outcome[row,"sensitivity"]<-sensitivity;
  outcome[row,"specificity"]<-specificity;
  
}

write.csv(outcome, file = ".//kfold-study//knn_kfold_study.csv");

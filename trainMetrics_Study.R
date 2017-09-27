#Model comparisons using CARET package
#Studying different metrics ROC, Kappa,Sens,Spec, Accuracy

#ROC = good to avoid choosing a threshold
#Kappa = good for when there is a class with low 
#Sens = minimizes false negative
#Spec = minimizes false positives

#Note that the class associated 

#libraries
library(caret)
library(mlbench)
install.packages("Metrics")
library(Metrics)


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


# Create custom indices: myFolds
#Guarantees that we are going to use the exact same datasets for all models
myFolds <- createFolds(summaryTable[,"explanatoryVariable"] , k = 10); 

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

######
# GLM

bayesglm<- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="bayesglm", trControl=kFoldControl);
bayesglm 

######
# SVM
svmLinearWeights_Spec <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="svmLinearWeights", 
                          trControl=kFoldControl, metric="Spec");
svmLinearWeights_Sens <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="svmLinearWeights", 
                               trControl=kFoldControl, metric="Sens");
svmLinearWeights_Kappa <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="svmLinearWeights", 
                               trControl=kFoldControl, metric="Kappa");
svmLinearWeights_Accuracy <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="svmLinearWeights", 
                               trControl=kFoldControl, metric="Accuracy");

 
############################################################################ 
## MAE 
# Define cost functions
# Custom MAE metric in caret format
mae_metric <- function (data,
                        lev = NULL,
                        model = NULL) {
  out <- mae(as.numeric(data$obs),as.numeric(data$pred))
  names(out) <- "MAE"
  out
}

# Create reusable trainControl object: myControl
kFoldControl_MAE <- trainControl(
  index = myFolds, #Train with 9 folds and validate with one
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE, #
  savePredictions = TRUE, #
  summaryFunction = mae_metric
);


svmLinearWeights_MAE <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="svmLinearWeights", 
                                   trControl=kFoldControl_MAE, metric="MAE");


svmLinearWeights_MAE

###############################################################


svmLinearWeights_ROC <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="svmLinearWeights", 
                                   trControl=kFoldControl, metric="Accuracy");

###################
# Compare models 

#Results of mininal

###################
#Visualize models
resampleList<-resamples(list(svm_Spec=svmLinearWeights_Spec,
                             svm_Sens=svmLinearWeights_Sens,
                             svm_Kappa=svmLinearWeights_Kappa,
                             svm_Accuracy=svmLinearWeights_Accuracy,
                             svm_MAE=svmLinearWeights_MAE,
                             svm_ROC=svmLinearWeights_ROC
                        ));

bwplot(resampleList,metric="Kappa")
densityplot(resampleList,metric="ROC")
dotplot(resampleList,xlim=range(0,1),metric="ROC")
#Compare two best
twoBestList <- resamples(list(svmLinearWeights=svmLinearWeights,bayesglm=bayesglm));
xyplot(twoBestList,xlim=range(0,1), metric="ROC")

#compare second and third best
secodThirdBestList <- resamples(list(knn=knn,bayesglm=bayesglm));
xyplot(secodThirdBestList,xlim=range(0,1), metric="ROC")

########################################
#Model selection results
#Best model for ranking (AM.3)

#Best model for Threshold (AM.1)

#Best model for Majority voting (AM.2)
#svmLinearWeights is tied with bayesglm



##################################################
#Predict n based on best model
compareTable <- data.frame(summaryTable$explanatoryVariable,
                           summaryTable$bugCoveringLabels,
                           predict(nb,summaryTable),
                           predict(knn,summaryTable),
                           predict(rf,summaryTable),
                           predict(bayesglm,summaryTable),
                           predict(svmLinearWeights,summaryTable)
);
colnames(compareTable) <- c("explanatoryVariable","actual","nb","knn","rf","glm","svm");

####################################################
#Predict n based on best model
bugCoveringPredicted <- predict(svmLinearWeights_MAE,summaryTable);
compareTable <- data.frame(summaryTable$explanatoryVariable,
                           summaryTable$bugCoveringLabels,
                           predict(svmLinearWeights_MAE,summaryTable)
);
colnames(compareTable) <- c("explanatoryVariable","actual","svm");



####################################################
compareTable

predictedBugCoveringList<-compareTable[compareTable$svm=="T",];
predictedBugCoveringList$explanatoryVariable
predictedBugCoveringList

#Computing the miminum value of n that predicted bugCovering True
min(predictedBugCoveringList$explanatoryVariable);

confusionMatrix(data=bugCoveringPredicted,summaryTable$bugCoveringLabels, positive="T");

#rnkin

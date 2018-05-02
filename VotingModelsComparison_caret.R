#Model comparisons using CARET package
#Which model provides the best predictions?
#NaiveBayes,KNN, RandomForest, SVN, GLM, or xgboostTree

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
#summaryTable <- data.frame(summaryTable);

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
summaryTable[,"explanatoryVariable"] <- summaryTable[,"rankingVote"];
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

#List of models to be later compared
modelList <- vector("list",length=8);

#######################
# Generate each model #

##############


# Naive Bayes -------------------------------------------------------------
nb<-  train(bugCoveringLabels ~ explanatoryVariable,training.df, method="nb", trControl=kFoldControl);

nb


#AM.1

#AM.2
#usekernel  ROC        Sens       Spec     
#FALSE      0.7136796  0.9380167  0.4383509
#TRUE       0.7241444  0.9302171  0.4439717

#AM.3:
# usekernel  ROC        Sens       Spec     
# FALSE      0.7546970  0.9031409  0.5664596
# TRUE       0.7534538  0.9270484  0.5095109
modelList <- list(nb);
#modelList <- list(modelList,nb);
modelList[1];
bugCoveringPredicted <- predict(nb, validation.df);
outcome <- calculateValidationErrors(nb,validation.df);


# KNN ---------------------------------------------------------------------
knn <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="knn", trControl=kFoldControl);

knn
#Aggre. k  ROC        Sens       Spec
#AM.1:
#AM.2: 7  0.8338240  0.9851064  0.0750000
#AM.3: 5  0.8290137  0.9778947  0.1340909
modelList <- c(modelList,knn);

# Random Forest -----------------------------------------------------------
rf<- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="rf", trControl=kFoldControl);

rf
#Aggre.  ROC        Sens       Spec     
#AM.1:
#AM.2: 0.7938766  0.8638338  0.4812422
#AM.3: 0.8124545  0.8762876  0.5132246
modelList <- list();
modelList <- list(modelList);


# xgBoostTree -------------------------------------------------------------
xgbtree <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable,
                method="gbm", trControl=kFoldControl);
xgbtree

modelList <- c(modelList,xgbtree);
# GLM ---------------------------------------------------------------------
glmModel<- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="glm", trControl=kFoldControl)

glmModel
#Aggre. ROC        Sens       Spec     
#AM.1:
#AM.2:  0.8276035  0.9338004  0.4748377
#AM.3:  0.8747113  0.9237826  0.4507378
modelList <- c(modelList,glmModel);


# Bayes GLM ---------------------------------------------------------------
bayesglm<- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="bayesglm", trControl=kFoldControl);

bayesglm
# Aggre.  ROC        Sens       Spec     
#AM.1:
#AM.2: 0.8797804  0.9338004  0.4748377
#AM.3: 0.8898239  0.9322932  0.4371014
modelList <- c(modelList,bayesglm);


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
svmLinear <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="svmLinear", trControl=kFoldControl);
svmLinear2 <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="svmLinear2", trControl=kFoldControl);
svmLinearWeights <- train(bugCoveringLabels ~ explanatoryVariable,summaryTable, method="svmLinearWeights", trControl=kFoldControl, metric="Spec");

svmLinear
#Aggre.    ROC        Sens       Spec     
#AM.1:
#AM.2: 0.598301  0.9589485  0.2627181
#AM.3: 0.6798618  0.9643897  0.2357955
modelList <- c(modelList,svmLinear);

svmLinear2
#Aggre. cost  ROC        Sens       Spec     
#AM.1: 
#AM.2: 1.00  0.8009603  0.9616318  0.2776515
#AM.3: 0.50  0.7713566  0.9757671  0.1613636
modelList <- c(modelList,svmLinear2);

svmLinearWeights
#Aggre. cost  weight  ROC        Sens       Spec    
#AM.1:
#AM.2: 1.00  2       0.8016421  0.9082113  0.6105458
#AM.3: 0.50  3       0.8102679  0.8439114  0.5645059
modelList <- c(modelList,svmLinearWeights);


# Compare models ----------------------------------------------------------

###################
#Visualize models
resampleList<-resamples(list(svmLinear=svmLinear,svmLinear2=svmLinear2,svmLinearWeights=svmLinearWeights,
                           glm=glmModel,bayesglm=bayesglm, rf=rf, knn=knn, nb=nb
                             ));

bwplot(resampleList,metric="ROC")
densityplot(resampleList,metric="ROC")
dotplot(resampleList,xlim=range(0,1),metric="ROC")
#Compare two best
twoBestList <- resamples(list(svmLinear2=svmLinear2,bayesglm=bayesglm));
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
compareTable <- data.frame(validation.df$explanatoryVariable,
                           validation.df$bugCoveringLabels,
                           predict(nb,validation.df),
                           predict(knn,validation.df),
                           predict(rf,validation.df),
                           predict(bayesglm,validation.df),
                           predict(svmLinearWeights,validation.df),
                           predict(svmLinear2,validation.df)
);
colnames(compareTable) <- c("explanatoryVariable","actual","nb","knn","rf",
                            "bayesGLM","svmW","svm2");

compareTable[compareTable$actual=="T",]


####################################################
#Predict n based on best model
compareTable <- data.frame(validation.df$explanatoryVariable,
                           validation.df$bugCoveringLabels,
                           predict(bayesglm,validation.df)
);
colnames(compareTable) <- c("explanatoryVariable","actual","predicted");



####################################################
compareTable

predictedBugCoveringList<-compareTable[compareTable$predicted=="T",];
predictedBugCoveringList$explanatoryVariable
predictedBugCoveringList

modelList <- list(nb.train)
sapply(modelList,class)

str(modelList, max.level=1)

class(modelList[[1]])

validationErrors.df <- calculateValidationErrors(modelList,validation.df);
write.csv(validationErrors.df, file = ".//kfold-study/rf_sens_kfold_study.csv");



# Estimate n (results of mininal) ---------------------------------------------------

#Computing the miminum value of n that predicted bugCovering True
min(predictedBugCoveringList$explanatoryVariable);

max(predictedBugCoveringList$explanatoryVariable);

#rnkin

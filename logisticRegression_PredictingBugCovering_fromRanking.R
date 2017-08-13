#Logistic regression using CARET Package
#Predict bug covering questions based on various values of 
#the parameters in the aggregation methods

install.packages(ElemStatLearn)
library(ElemStatLearn)
library(klaR)
library(caret)
library(gmodels)
library(e1071)
library(caTools)

#Obtain the data

# Import data
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


############################################################################################
# Build model

sub = sample(nrow(summaryTable), floor(nrow(summaryTable) * 0.7))
train = summaryTable[sub,]
test = summaryTable[-sub,]

xTrain = train[,"rankingVote"]
yTrain = as.factor(train$bugCovering);

xTest = test[,"rankingVote"]
yTest = as.factor(test$bugCovering);

xS = summaryTable[,"rankingVote"]
yS = data.frame(summaryTable[,"bugCovering"]);

#To compute Area Under the Curve
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

model <- glm(bugCovering ~ rankingVote, family = binomial(link = "logit"), train)

p <- predict(model, test, type = "response")
colAUC(p,test[["bugCovering"]],plotROC=TRUE)

summary(p)
p_class<- ifelse(p>0.8147059,TRUE, FALSE) #requires to choose a probability threshold
confusionMatrix(p_class,test[["bugCovering"]])

predictted.df <- data.frame(predictted);
CrossTable(predictted.df$class,yS$bugCovering);
confusionMatrix(predictted.df$class,yS$bugCovering);

# Reference
# Prediction FALSE TRUE
# FALSE    33    3
# TRUE      1    2
# 
# Accuracy : 0.8974          
# 95% CI : (0.7578, 0.9713)
# No Information Rate : 0.8718          
# P-Value [Acc > NIR] : 0.4284          
# 
# Kappa : 0.4468          
# Mcnemar's Test P-Value : 0.6171          
# 
# Sensitivity : 0.9706          
# Specificity : 0.4000          
# Pos Pred Value : 0.9167          
# Neg Pred Value : 0.6667          
# Prevalence : 0.8718          
# Detection Rate : 0.8462          
# Detection Prevalence : 0.9231          
# Balanced Accuracy : 0.6853          
# 
# 'Positive' Class : FALSE        


#########################################################

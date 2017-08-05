
# Random Forest to evaluate different models to predict bug-covering questions
## Model-1 based on majority votin
## Model-2 based on threshold of number of YES answers
## Model-3 based on ranking of number of YES answers

install.packages('randomForest')
library(randomForest)
install.packages("rpart.plot")
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart)
library(RColorBrewer)
library(rattle)
library(rpart.plot) 

source("C://Users//chris//OneDrive//Documentos//GitHub//randomForestWorkerConfidenceDifficulty//loadAnswers.R");

##
# Import data
dataf <- loadAnswers("answerList_data.csv");
summary(dataf$Answer.confidence)

#Consolidate answers by question.
#count the number of YES, NO, IDK for each question
dataf$Answer.option

totalData = length(summaryTable$Question.ID);
trainingSize = trunc(totalData * 0.7);
startTestIndex = totalData - trainingSize;
endTestIndex = totalData;

model <- randomForest(as.factor(bugCovering) ~ ranking, 
                      data = as.data.frame(summaryTable[1:trainingSize,]), 
                      importance=TRUE, ntree=2000, type="class");
varImpPlot(model);

# Predict YES answers
Prediction <- predict(model, test,'vote');
submit <- data.frame(Question.ID = test$Question.ID, PredictedLevel = Prediction, Actual = test$bugCovering);
write.csv(submit, file = "C://Users//chris//OneDrive//Documentos//GitHub//randomForestWorkerConfidenceDifficulty//firstforest_ranking.csv", row.names = FALSE);
model$predicted
model$confusion
model$votes

#---------------------------------------------------------------------

library(party)

# Decision Tree
fit <- rpart(as.factor(bugCovering) ~ ranking, method="class",
             data = as.data.frame(summaryTable));
#              data = as.data.frame(summaryTable[1:trainingSize,]));


printcp(fit);
plotcp(fit);
summary(fit);
plot(fit)
plot(fit, uniform=TRUE,main="Classification of bugCovering by ranking");
text(fit, use.n = TRUE, all=TRUE, cex=1);
#ranking higher than 2.5 implies non-bug. 
#Interpret these results from summary
#Run the decision tree for majority and threshold

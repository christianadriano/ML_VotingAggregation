#K-nearest neighbor KNN
#Predict bug covering questions based on various values of 
#the parameters in the aggregation methods


#Obtain the data

# Import data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_VotingAggregation//aggregateAnswerOptionsPerQuestion.R");
summaryTable <- runMain();

#I need to guarantee that some examples (i.e., failing methods)
#do not dominate the training or testing sets. To do that, I need to get a 
#close to equal proportion of examples in both sets. One way to do that is
#to shuffle the data and sample from it.
set.seed(9850);
g<- runif((nrow(summaryTable))); #generates a random distribution
summaryTable <- summaryTable[order(g),];

###########################################################
# Below are two options to partition the data in training and testing sets

#Option-1 Training set (2/3)
totalData = length(summaryTable$Question.ID);
trainingSize = trunc(totalData * 2/3);
startTestIndex = totalData - trainingSize;
endTestIndex = totalData;

#Extract training and test data
trainingData = as.data.frame(summaryTable[1:trainingSize,]);
testData = as.data.frame(summaryTable[startTestIndex:endTestIndex,])

##################################################################
#Option-2 Mark sample with a probability 

set.seed(4321);
ind<- sample(2, nrow(summaryTable),replace = TRUE, prob=c(0.67,0.33));

trainingData <- as.data.frame(summaryTable[ind==1,]);
testData <- as.data.frame(summaryTable[ind==2,]);

##Obtain the ground truth

trainingLabels <- as.data.frame(summaryTable[ind==1,"bugCovering"]);
testLabels <- as.data.frame(summaryTable[ind==2,"bugCovering"]);

##################################################################
#Build the KNN model
install.packages("class")
library(class);

#Select only the ranking as a feature to predict bugCovering
summaryTable <- summaryTable[,c("bugCovering","ranking")];

fitModel <- knn(train =trainingData, test=testData, cl=trainingLabels[,1] , k=2);
attributes(.Last.value)
summary(fitModel);

#Evaluate model

testLabels<-data.frame(testLabels[,1]);
merge <- data.frame(fitModel,testLabels);
names(merge)<- c("Predicted bug","Actual bug");
merge

install.packages("gmodels");
library(gmodels)

CrossTable(x = trainingData[,"bugCovering"], y=fitModel.cv, prop.chisq = FALSE)
plot(fitModel.cv)
fitModel
fitFrame <- data.frame(fitModel)

predictionFrame<-data.frame(fitModel.cv)
mean(trainingData[predictionFrame[,1]==TRUE,"ranking"])
trainingData[predictionFrame[,1]==TRUE,]
predictionFrame[predictionFrame[,1]==TRUE,]

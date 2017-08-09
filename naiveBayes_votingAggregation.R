#K-Nearest Neighbor KNN
#Predict bug covering questions based on various values of 
#the parameters in the aggregation methods

install.packages("class");
library(class);
install.packages("gmodels");
library(gmodels)
install.packages('e1071', dependencies=TRUE)
library(e1071)
install.packages("klaR")
library(klaR)

#Obtain the data

# Import data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_VotingAggregation//aggregateAnswerOptionsPerQuestion.R");
summaryTable <- runMain();
#summaryTable <- data.frame(summaryTable);

#I need to guarantee that some examples (i.e., failing methods)
#do not dominate the training or testing sets. To do that, I need to get a 
#close to equal proportion of examples in both sets

#Scramble the dataset before extracting the training set.
set.seed(9850);
g<- runif((nrow(summaryTable))); #generates a random distribution
summaryTable <- summaryTable[order(g),];

###########################################################################################
# Naive Bayes

totalData = length(summaryTable$Question.ID);
trainingSize = trunc(totalData * 0.7);
startTestIndex = trainingSize + 1;
endTestIndex = totalData;

#convert columns to numeric
summaryTable<- data.frame(summaryTable)
summaryTable[,"rankingVote"] <- as.numeric(unlist(summaryTable[,"rankingVote"])); 
summaryTable$bugCovering <- as.factor(summaryTable$bugCovering);

trainingData<- as.data.frame(summaryTable[1:trainingSize,]);
testingData<-as.data.frame(summaryTable[startTestIndex:endTestIndex,]);

nb.model <- NaiveBayes(as.factor(bugCovering) ~ rankingVote, data =trainingData );

nb.pred <- predict(nb.model, data = testingData);

prediction.df<- data.frame(nb.pred);
CrossTable(x=testingData$bugCovering, y=prediction.df$class, prop.chisq=FALSE);

length(testingData$bugCovering)
length(prediction.df$class)

############################################################################################
# Naive Bayes using CARET

install.packages('ElemStatLearn')
library('ElemStatLearn')
library("klaR")
library("caret")

sub = sample(nrow(summaryTable), floor(nrow(summaryTable) * 1))
train = summaryTable[sub,]
test = summaryTable[-sub,]

xTrain = train[,"rankingVote"]
yTrain = as.factor(train$bugCovering);

xTest = test[,"rankingVote"]
yTest = as.factor(test$bugCovering);

xS = summaryTable[,"rankingVote"]
yS = data.frame(summaryTable[,"bugCovering"]);

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))

predictted<-predict(model$finalModel,xS);
predictted.df <- data.frame(predictted)
CrossTable(predictted.df$class,yS$bugCovering)

#PREDICTION   ACTUAL
#           FALSE TRUE
#FALSE        98   10
#TRUE          6   15
#TP = 15/25
#FP = 6
#FN = 10

plot(yTest)

prop.table(table(predict(model$finalModel,xTest)$class,yTest))
#yTest
#        FALSE       TRUE
#FALSE 0.71794872 0.10256410
#TRUE  0.05128205 0.12820513

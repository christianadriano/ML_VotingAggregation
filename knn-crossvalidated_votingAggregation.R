#K-Nearest Neighbor KNN
#Predict bug covering questions based on various values of 
#the parameters in the aggregation methods

#Obtain the data

# Import data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_VotingAggregation//aggregateAnswerOptionsPerQuestion.R");
summaryTable <- runMain();

#I need to guarantee that some examples (i.e., failing methods)
#do not dominate the training or testing sets. To do that, I need to get a 
#close to equal proportion of examples in both sets

#Scramble the dataset before extracting the training set.
set.seed(9850);
g<- runif((nrow(summaryTable))); #generates a random distribution
summaryTable <- summaryTable[order(g),];
summaryTable[,"ranking"] <- as.numeric(unlist(summaryTable[,"ranking"]));

##################################################################
#Build the KNN model

install.packages("class");
library(class);

#Select only the ranking as a feature to predict bugCovering
summaryTable <- summaryTable[,c("bugCovering","ranking")];

#Prepare explanatory variable (ranking) and target (bugCovering)
trainingData <-data.frame(summaryTable);
trainingData$ranking <- as.numeric(trainingData$ranking);
trainingData$bugCovering <- as.factor(trainingData$bugCovering);


######################################################################################
#Using KNN from CLASS package

fitModel.cv <- knn.cv (train =trainingData, cl=trainingLabels, k=19, l=0, prob = FALSE, use.all=TRUE);

#Evaluate model

install.packages("gmodels");
library(gmodels)

fitModel.cv.df<-data.frame(fitModel.cv)
CrossTable(x = trainingLabels, y=fitModel.cv.df[,1], prop.chisq = FALSE)
#False Positives = 2
#False Negatives = 6
#True Positives = 19
#True Negatives = 102

plot(fitModel.cv)
fitModel.cv

trainingData$bugCovering <- as.factor(trainingData$bugCovering);
predictedBugCoveringList<-trainingData[fitModel.cv.df[,1]==TRUE,];
rankingList <- as.numeric(unlist(predictedBugCoveringList[,2]));
mean(rankingList)
max(rankingList)
hist(rankingList,main="Bug-covering ranking dist., k=3, 5 or 7, knn Class, mean=1.71 ",xlab="ranking");

###################################################################################### 
### Using KNN from CARET package
## https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
## https://dataaspirant.com/2017/01/09/knn-implementation-r-using-caret-package/
install.packages("caret")
library(caret)
install.packages('e1071', dependencies=TRUE)
library(e1071)

# I will do 5 repeats of 10-Fold CV. I will fit
# a KNN model that evaluates 10 values of k

set.seed(1234)

trctrl <- trainControl(method = "repeatedcv", number=10, p=0.9, repeats = 5)

trainingData$ranking <- as.numeric(trainingData$ranking);
trainingData$bugCovering <- as.factor(trainingData$bugCovering);
mean(trainingData$ranking);

knn_fit <- train(bugCovering ~ ranking, data = trainingData, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

bugCoveringPredicted <- predict(knn_fit,newdata = trainingData);

confusionMatrix(data=bugCoveringPredicted,trainingData$bugCovering)
#False Positives = 10
#False Negatives = 6
#True Positives = 15
#True Negatives = 98

df<-data.frame(bugCoveringPredicted)
predictedBugCoveringList<-trainingData[df[,1]==TRUE,];
rankingList <- as.numeric(unlist(predictedBugCoveringList[,2]));
mean(rankingList)
max(rankingList)
hist(rankingList,main="Bug-covering ranking dist., knn caret repeatedcv, mean=1.52, max=2",xlab="ranking");

#Caret produced more false positives than knn.cv from class package. I tried to fine tune it more,
#but was not enough. The k value for Caret seems very high 15 t 23.

###########################################################################################
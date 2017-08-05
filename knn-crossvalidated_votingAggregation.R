#K-nearest neighbor KNN
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

##################################################################
#Build the KNN model

install.packages("class")
library(class);

#Select only the ranking as a feature to predict bugCovering
summaryTable <- summaryTable[,c("bugCovering","ranking")];

#Using CLASS

trainingData <-summaryTable;
trainingLabels<-data.frame(summaryTable$bugCovering);
fitModel.cv <- knn.cv (train =trainingData, cl=trainingLabels[,1], k=1, l=0, prob = FALSE, use.all=TRUE);

#Evaluate model

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

##############################################################
### USING CARET
## https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
## https://dataaspirant.com/2017/01/09/knn-implementation-r-using-caret-package/

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(V1 ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

 